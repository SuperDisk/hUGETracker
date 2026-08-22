unit Codegen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, Instruments, Song, Utils,
  HugeDatatypes, Constants, Dialogs, strutils, FileUtil, LazFileUtils, process;

type

  { EAssemblyException }

  EAssemblyException = class(Exception)
    public
      ProgramName: String;
      constructor Create(Prog, Msg: String);
  end;

  ECodegenRenameException = class(Exception);

  TExportMode = (emNormal, emPreview, emGBS);

procedure AssembleSong(Song: TSong; Filename: string; Mode: TExportMode = emNormal);
procedure RenderSongToGBDKC(Song: TSong; DescriptorName: String; Filename: string; Bank: Integer = -1);
procedure RenderSongToRGBDSAsm(Song: TSong; DescriptorName: String; Filename: string);

implementation

uses AvgLvlTree, fgl;

type
  TNoteFrequencyMap = specialize TFPGMap<Integer, Integer>;
  TNoteCatalog = array of Integer;
  TNoteCatalogs = array[TChannel] of TNoteCatalog;
  TEncodedPattern = array of Byte;

  TNoteCatalogCandidate = record
    NoteRecord: Integer;
    Frequency: Integer;
  end;

  TUsedStuff = record
    HighestDutyInst, HighestWaveInst, HighestNoiseInst: Integer;
    HighestWaveform: Integer;
    UsedPatterns: array[TChannel] of TAvgLvlTree;
  end;

const
  // Pattern bytecode:
  //   0..127   catalog index
  //   128..200 literal note byte, followed by the other two DN bytes
  //   201      unused
  //   202..255 repeat the last catalog note 1..54 additional times
  NOTE_CATALOG_SIZE = 128;
  PATTERN_LITERAL_FLAG = $80;
  PATTERN_RLE_BASE = 201;
  PATTERN_RLE_MAX_REPETITIONS = 255 - PATTERN_RLE_BASE;

function CompareIntPointers(Data1, Data2: Pointer): integer;
begin
  Result := Integer(Data1^) - Integer(Data2^);
end;

function FindUsedStuff(const Song: TSong;
  const OrderMatrix: TOrderMatrix): TUsedStuff;
var
  I, J: Integer;
  Channel: TChannel;
  Pat: PPattern;
  Cell: TCell;
  Instr: TInstrument;
  InstValue: Integer;
  Highest: ^Integer;
  Waveform: Integer;
begin
  for Channel := Low(TChannel) to High(TChannel) do
    Result.UsedPatterns[Channel] := TAvgLvlTree.Create(@CompareIntPointers);
  Result.HighestDutyInst := -1;
  Result.HighestWaveInst := -1;
  Result.HighestNoiseInst := -1;
  Result.HighestWaveform := -1;

  for Instr in Song.Instruments.Wave do begin
    if not Instr.SubpatternEnabled then Continue;

    for Cell in Instr.Subpattern do
      if Cell.EffectCode = $9 then begin
        Waveform := Cell.EffectParams.Value;
        if InRange(Waveform, 0, 15) and (Waveform > Result.HighestWaveform) then
          Result.HighestWaveform := Waveform;
      end;
  end;

  for I := Low(OrderMatrix) to High(OrderMatrix) do begin
    case I of
      0, 1: Highest := @Result.HighestDutyInst;
      2: Highest := @Result.HighestWaveInst;
      3: Highest := @Result.HighestNoiseInst;
    end;

    for J := Low(OrderMatrix[I]) to High(OrderMatrix[I])-1 do begin
      Channel := TChannel(I);
      if Result.UsedPatterns[Channel].Find(@OrderMatrix[I, J]) <> nil then
        Continue;

      Result.UsedPatterns[Channel].Add(@OrderMatrix[I, J]);

      Pat := Song.Patterns.KeyData[OrderMatrix[I, J]];
      for Cell in Pat^ do begin
        if (Cell.EffectCode = $9) and (I = 2) then begin // waveforms on wave channel
          Waveform := Cell.EffectParams.Value;
          if InRange(Waveform, 0, 15) and (Waveform > Result.HighestWaveform) then
            Result.HighestWaveform := Waveform;
        end;

        if Cell.Instrument = 0 then Continue;

        if InRange(Cell.Instrument, 0, 15) then
          InstValue := Cell.Instrument
        else
          InstValue := 0;

        if InstValue > Highest^ then
          Highest^ := InstValue;

        if (I = 2) then begin // Wave channel
          Waveform := Song.Instruments.Wave[Cell.Instrument].Waveform;
          if Waveform > Result.HighestWaveform then
            Result.HighestWaveform := Waveform;
        end;
      end;
    end;
  end;
end;

procedure FreeUsedStuff(const UsedStuff: TUsedStuff);
var
  Channel: TChannel;
begin
  for Channel := Low(TChannel) to High(TChannel) do
    UsedStuff.UsedPatterns[Channel].Free;
end;

function PatternIsUsedInChannel(Pattern: Integer; Channel: TChannel;
  const UsedStuff: TUsedStuff): Boolean;
begin
  Result := UsedStuff.UsedPatterns[Channel].Find(@Pattern) <> nil;
end;

function InstrumentIsUsed(Instrument: Integer; Type_: TInstrumentType; const UsedStuff: TUsedStuff): Boolean;
begin
  case Type_ of
    itSquare: Result := Instrument <= UsedStuff.HighestDutyInst;
    itWave: Result := Instrument <= UsedStuff.HighestWaveInst;
    itNoise: Result := Instrument <= UsedStuff.HighestNoiseInst;
  end;
end;

function CellToNoteRecord(const Cell: TCell): Integer;
var
  Note, Instrument, Effect: Integer;
  B1, B2, B3: Byte;
begin
  if InRange(Cell.Note, 0, HIGHEST_NOTE) then
    Note := Cell.Note
  else
    Note := LAST_NOTE;

  if InRange(Cell.Instrument, 0, 15) then
    Instrument := Cell.Instrument
  else
    Instrument := 0;

  Effect := (Cell.EffectCode shl 8) or Cell.EffectParams.Value;
  DN(Note, Instrument, Effect, B1, B2, B3);
  Result := B1 or (B2 shl 8) or (B3 shl 16);
end;

function NoteRecordByte(NoteRecord, Index: Integer): Byte;
begin
  Result := Byte(NoteRecord shr (Index * 8));
end;

function CatalogIndexOf(const Catalog: TNoteCatalog;
  NoteRecord: Integer): Integer;
var
  I: Integer;
begin
  for I := 0 to Length(Catalog) - 1 do
    if Catalog[I] = NoteRecord then Exit(I);
  Result := -1;
end;

function CandidateComesBefore(const A, B: TNoteCatalogCandidate): Boolean;
begin
  if A.Frequency <> B.Frequency then
    Exit(A.Frequency > B.Frequency);
  Result := A.NoteRecord < B.NoteRecord;
end;

procedure SortCatalogCandidates(var Candidates: array of TNoteCatalogCandidate;
  Left, Right: Integer);
var
  I, J: Integer;
  Pivot, Temp: TNoteCatalogCandidate;
begin
  I := Left;
  J := Right;
  Pivot := Candidates[(Left + Right) div 2];
  repeat
    while CandidateComesBefore(Candidates[I], Pivot) do Inc(I);
    while CandidateComesBefore(Pivot, Candidates[J]) do Dec(J);
    if I <= J then begin
      Temp := Candidates[I];
      Candidates[I] := Candidates[J];
      Candidates[J] := Temp;
      Inc(I);
      Dec(J);
    end;
  until I > J;

  if Left < J then SortCatalogCandidates(Candidates, Left, J);
  if I < Right then SortCatalogCandidates(Candidates, I, Right);
end;

function BuildNoteCatalog(const Song: TSong; Channel: TChannel;
  const UsedStuff: TUsedStuff): TNoteCatalog;
var
  Frequencies: TNoteFrequencyMap;
  Candidates: array of TNoteCatalogCandidate;
  PatternIndex, CellIndex, FrequencyIndex, I, CatalogLength: Integer;
  NoteRecord: Integer;
  Pattern: PPattern;
begin
  Result := nil;
  Frequencies := TNoteFrequencyMap.Create;
  try
    Frequencies.Sorted := True;
    for PatternIndex := 0 to Song.Patterns.Count - 1 do begin
      if not PatternIsUsedInChannel(Song.Patterns.Keys[PatternIndex], Channel,
        UsedStuff) then Continue;

      Pattern := Song.Patterns.Data[PatternIndex];
      for CellIndex := Low(TPattern) to High(TPattern) do begin
        NoteRecord := CellToNoteRecord(Pattern^[CellIndex]);
        FrequencyIndex := Frequencies.IndexOf(NoteRecord);
        if FrequencyIndex = -1 then
          Frequencies.Add(NoteRecord, 1)
        else
          Frequencies.Data[FrequencyIndex] :=
            Frequencies.Data[FrequencyIndex] + 1;
      end;
    end;

    SetLength(Candidates, Frequencies.Count);
    for I := 0 to Frequencies.Count - 1 do begin
      Candidates[I].NoteRecord := Frequencies.Keys[I];
      Candidates[I].Frequency := Frequencies.Data[I];
    end;
  finally
    Frequencies.Free;
  end;

  if Length(Candidates) > 1 then
    SortCatalogCandidates(Candidates, 0, High(Candidates));

  CatalogLength := Min(Length(Candidates), NOTE_CATALOG_SIZE);
  SetLength(Result, CatalogLength);
  for I := 0 to CatalogLength - 1 do
    Result[I] := Candidates[I].NoteRecord;
end;

function BuildNoteCatalogs(const Song: TSong;
  const UsedStuff: TUsedStuff): TNoteCatalogs;
var
  Channel: TChannel;
begin
  for Channel := Low(TChannel) to High(TChannel) do
    Result[Channel] := BuildNoteCatalog(Song, Channel, UsedStuff);
end;

function EncodePattern(const Pattern: TPattern;
  const Catalog: TNoteCatalog): TEncodedPattern;
var
  Row, RunEnd, Remaining, Repetitions, OutputIndex, CatalogIndex: Integer;
  NoteRecord: Integer;

  procedure Emit(Value: Byte);
  begin
    Result[OutputIndex] := Value;
    Inc(OutputIndex);
  end;

begin
  Result := nil;
  SetLength(Result, Length(Pattern) * 3);
  OutputIndex := 0;
  Row := Low(TPattern);
  while Row <= High(TPattern) do begin
    NoteRecord := CellToNoteRecord(Pattern[Row]);
    CatalogIndex := CatalogIndexOf(Catalog, NoteRecord);
    if CatalogIndex = -1 then begin
      Emit(NoteRecordByte(NoteRecord, 0) or PATTERN_LITERAL_FLAG);
      Emit(NoteRecordByte(NoteRecord, 1));
      Emit(NoteRecordByte(NoteRecord, 2));
      Inc(Row);
      Continue;
    end;

    Emit(CatalogIndex);
    RunEnd := Row + 1;
    while (RunEnd <= High(TPattern)) and
      (CellToNoteRecord(Pattern[RunEnd]) = NoteRecord) do
      Inc(RunEnd);

    Remaining := RunEnd - Row - 1;
    while Remaining > 0 do begin
      Repetitions := Min(Remaining, PATTERN_RLE_MAX_REPETITIONS);
      Emit(PATTERN_RLE_BASE + Repetitions);
      Dec(Remaining, Repetitions);
    end;
    Row := RunEnd;
  end;
  SetLength(Result, OutputIndex);
end;

function ChannelPatternName(Channel: TChannel; PatternKey: Integer): String;
begin
  Result := Format('P%d_%d', [Ord(Channel) + 1, PatternKey]);
end;

function NoteRecordDNArgs(NoteRecord: Integer; HexPrefix: String): String;
var
  B1, B2, B3: Byte;
  Note, Instrument, Effect: Integer;
begin
  B1 := NoteRecordByte(NoteRecord, 0);
  B2 := NoteRecordByte(NoteRecord, 1);
  B3 := NoteRecordByte(NoteRecord, 2);
  Note := B1 and $7F;
  Instrument := (B2 shr 4) or ((B1 and $80) shr 3);
  Effect := ((B2 and $0F) shl 8) or B3;
  Result := Format('%d,%d,%s%s',
    [Note, Instrument, HexPrefix, HexStr(Effect, 3)]);
end;

function EncodedByteRange(const Encoded: TEncodedPattern; Start,
  ByteCount: Integer): String;
var
  I, Last: Integer;
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.StrictDelimiter := True;
    SL.Delimiter := ',';
    Last := Min(Start + ByteCount, Length(Encoded)) - 1;
    for I := Start to Last do
      SL.Add(IntToStr(Encoded[I]));
    Result := SL.DelimitedText;
  finally
    SL.Free;
  end;
end;

function RenderGBDKNoteCatalog(Name: String;
  const Catalog: TNoteCatalog): String;
var
  I: Integer;
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Add('static const unsigned char ' + Name + '[] = {');
    for I := 0 to Length(Catalog) - 1 do
      SL.Add('    DN(' + NoteRecordDNArgs(Catalog[I], '0x') + '),');
    SL.Add('};');
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function RenderRGBDSNoteCatalog(Name: String;
  const Catalog: TNoteCatalog): String;
var
  I: Integer;
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Add(Name + ':');
    for I := 0 to Length(Catalog) - 1 do
      SL.Add(' dn ' + NoteRecordDNArgs(Catalog[I], '$'));
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function RenderGBDKCompressedPattern(Name: String;
  const Encoded: TEncodedPattern): String;
const
  BYTES_PER_LINE = 16;
var
  I: Integer;
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Add('static const unsigned char ' + Name + '[] = {');
    I := 0;
    while I < Length(Encoded) do begin
      SL.Add('    ' + EncodedByteRange(Encoded, I, BYTES_PER_LINE) + ',');
      Inc(I, BYTES_PER_LINE);
    end;
    SL.Add('};');
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function RenderRGBDSCompressedPattern(Name: String;
  const Encoded: TEncodedPattern): String;
const
  BYTES_PER_LINE = 16;
var
  I: Integer;
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Add(Name + ':');
    I := 0;
    while I < Length(Encoded) do begin
      SL.Add(' db ' + EncodedByteRange(Encoded, I, BYTES_PER_LINE));
      Inc(I, BYTES_PER_LINE);
    end;
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

procedure RenderSongToGBDKC(Song: TSong; DescriptorName: String; Filename: string; Bank: Integer = -1);
  function RenderGBDKSubpatternCell(Cell: TCell; Last: Boolean): string;
  var
    SL: TStringList;
  begin
    SL := TStringList.Create;
    SL.Delimiter := ',';

    if Cell.Note = NO_NOTE then
      SL.Add('___')
    else
      SL.Add(IntToStr(Cell.Note));

    if Last and (Cell.Volume = 0) then
      SL.Add(IntToStr(1)) // Automatically insert jump back to row 1 if last cell
    else
      SL.Add(IntToStr(EnsureRange(Cell.Volume, 0, 32)));

    SL.Add('0x' + EffectCodeToStr(Cell.EffectCode, Cell.EffectParams));

    Result := SL.DelimitedText;
    SL.Free;
  end;

  function RenderGBDKSubpattern(Name: string; Pat: TPattern): string;
  var
    I: Integer;
  begin
    Result := 'static const unsigned char ' + Name + '[] = {' + LineEnding;
    for I := 0 to 31 do
      Result += '    DN(' + RenderGBDKSubpatternCell(Pat[I], I = 31) + '),' + LineEnding;
    Result += '};';
  end;

  function RenderGBDKOrder(Channel: TChannel;
    Order: array of integer): string;
  var
    SL: TStringList;
    I: integer;
  begin
    SL := TStringList.Create;
    SL.StrictDelimiter := True;
    SL.Delimiter := ',';

    for I := Low(Order) to High(Order)-1 do // HACK: account for off-by-one error
      SL.Add(ChannelPatternName(Channel, Order[I]));

    Result := 'static const unsigned char* const order' +
      IntToStr(Ord(Channel) + 1) + '[] = {';
    Result += SL.DelimitedText;
    Result += '};';
    SL.Free;
  end;

  function RenderGBDKInstrument(Instrument: TInstrument; Num: Integer): string;
  var
    SL: TStringList;
    AsmInstrument: TAsmInstrument;
    J: integer;
    HighMask: byte;
    TypePrefix: String;
  begin
    AsmInstrument := InstrumentToBytes(Instrument);
    SL := TStringList.Create;
    SL.StrictDelimiter := True;
    SL.Delimiter := ',';

    if Instrument.Type_ = itNoise then
    begin
      SL.Add(IntToStr(AsmInstrument[1])); // envelope

      HighMask := AsmInstrument[0];
      if Instrument.LengthEnabled then
        HighMask := HighMask or %01000000;
      if Instrument.CounterStep = swSeven then
        HighMask := HighMask or %10000000;
      SL.Add(IntToStr(HighMask));
    end
    else
      for J := Low(AsmInstrument) to High(AsmInstrument) do
        SL.Add(IntToStr(AsmInstrument[J]));

    WriteStr(TypePrefix, Instrument.Type_);

    if Instrument.SubpatternEnabled then
      SL.Insert(SL.Count-1, Format('%sSP%d', [TypePrefix, Num]))
    else
      SL.Insert(SL.Count-1, '0');

    if Instrument.Type_ = itNoise then begin
      SL.Add('0');
      SL.Add('0');
    end;

    Result := '{'+SL.DelimitedText+'}';
  end;

  function RenderGBDKInstrumentBank(Name: string; Bank: TInstrumentBank; Limit: Integer): string;
  var
    I: integer;
    InstrType: String;
  begin
    case Bank[1].Type_ of
      itSquare: InstrType := 'hUGEDutyInstr_t';
      itWave: InstrType := 'hUGEWaveInstr_t';
      itNoise: InstrType := 'hUGENoiseInstr_t';
    end;

    if Limit = -1 then
      Exit('static const ' + InstrType + '* ' + Name + ' = NULL;'+LineEnding);

    Result := 'static const ' + InstrType + ' ' + Name + '[] = {'+LineEnding;
    for I := Low(Bank) to Limit do begin
      Result += '    '+RenderGBDKInstrument(Bank[I], I) + ','+LineEnding;
    end;
    Result += '};';
  end;

  function RenderGBDKWaves(Waves: TWaveBank; Limit: Integer): string;
  var
    I, J: integer;
  begin
    if Limit = -1 then
      Exit('static const unsigned char* waves = NULL;'+LineEnding);

    Result := 'static const unsigned char waves[] = {'+LineEnding;
    for I := Low(Waves) to Limit do
    begin
      Result += '    ';
      J := Low(Waves[I]);
      while J < High(Waves[I]) do
      begin
        Result += IntToStr((Waves[I, J] shl 4) or Waves[I, J + 1])+',';
        Inc(J, 2);
      end;
      Result += LineEnding;
    end;
    Result += '};';
  end;

var
  OrderMatrix: TOrderMatrix;
  NoteCatalogs: TNoteCatalogs;
  EncodedPattern: TEncodedPattern;
  OutSL: TStringList;
  I: integer;
  Channel: TChannel;
  F: Text;
  TypePrefix: String;
  UsedStuff: TUsedStuff;
begin
  OrderMatrix := BuildOrderMatrix(Song, False);
  UsedStuff := FindUsedStuff(Song, OrderMatrix);
  NoteCatalogs := BuildNoteCatalogs(Song, UsedStuff);

  OutSL := TStringList.Create;

  if Bank <> -1 then begin
    OutSL.Add('#pragma bank '+IntToStr(Bank));
    OutSL.Add('');
  end;

  OutSL.Add('#include "hUGEDriver.h"');
  OutSL.Add('#include <stddef.h>');
  OutSL.Add('');

  for Channel := Low(TChannel) to High(TChannel) do begin
    OutSL.Add(RenderGBDKNoteCatalog('note_catalog' +
      IntToStr(Ord(Channel) + 1), NoteCatalogs[Channel]));
    OutSL.Add('');
  end;

  for Channel := Low(TChannel) to High(TChannel) do
    for I := 0 to Song.Patterns.Count - 1 do
      if PatternIsUsedInChannel(Song.Patterns.Keys[I], Channel,
        UsedStuff) then begin
        EncodedPattern := EncodePattern(Song.Patterns.Data[I]^,
          NoteCatalogs[Channel]);
        OutSL.Add(RenderGBDKCompressedPattern(
          ChannelPatternName(Channel, Song.Patterns.Keys[I]),
          EncodedPattern));
      end;
  OutSL.Add('');

  for I := Low(Song.Instruments.All) to High(Song.Instruments.All) do
    if InstrumentIsUsed(ModInst(I), Song.Instruments.All[I].Type_, UsedStuff) then
      with Song.Instruments.All[I] do begin
        if SubpatternEnabled then begin
          WriteStr(TypePrefix, Type_);
          OutSL.Add(RenderGBDKSubpattern(TypePrefix+'SP' + IntToStr(ModInst(I)), Subpattern));
        end;
      end;

  for Channel := Low(TChannel) to High(TChannel) do
    OutSL.Add(RenderGBDKOrder(Channel, OrderMatrix[Ord(Channel)]));
  OutSL.Add('');

  OutSL.Add(RenderGBDKInstrumentBank('duty_instruments', Song.Instruments.Duty, UsedStuff.HighestDutyInst));
  OutSL.Add(RenderGBDKInstrumentBank('wave_instruments', Song.Instruments.Wave, UsedStuff.HighestWaveInst));
  OutSL.Add(RenderGBDKInstrumentBank('noise_instruments', Song.Instruments.Noise, UsedStuff.HighestNoiseInst));
  OutSL.Add('');

  OutSL.Add(RenderGBDKWaves(Song.Waves, UsedStuff.HighestWaveform));
  OutSL.Add('');

  if Bank <> -1 then
    OutSL.Add(Format('const void __at(%d) __bank_%s;', [Bank, DescriptorName]));

  OutSL.Add(Format(
    'const hUGESong_t %s = {%d, %d, %d, %d, %d, order1, order2, order3,'+
    'order4, duty_instruments, wave_instruments, noise_instruments, NULL, waves,'+
    ' note_catalog1, note_catalog2, note_catalog3, note_catalog4};',
    [DescriptorName,
     Song.TicksPerRow[0], Song.TicksPerRow[1], Song.TicksPerRow[2], Song.TicksPerRow[3],
     OrderCount(Song)*2
    ]));

  AssignFile(F, Filename);
  Rewrite(F);
  Write(F, OutSL.Text);
  CloseFile(F);

  OutSL.Free;
  FreeUsedStuff(UsedStuff);
end;

function RenderOrderTable(OrderMatrix: TOrderMatrix;
  IncludeCatalogPointers: Boolean = False): string;
  function ArrayHelper(Channel: TChannel; Ints: array of integer): string;
  var
    I: integer;
    SL: TStringList;
  begin
    SL := TStringList.Create;
    SL.StrictDelimiter := True;
    SL.Delimiter := ',';
    for I := Low(Ints) to High(Ints)-1 do // HACK: account for off-by-one error
      SL.Add(ChannelPatternName(Channel, Ints[I]));
    Result := SL.DelimitedText;
    SL.Free;
  end;

var
  Res: TStringList;
begin
  Res := TStringList.Create;

  if IncludeCatalogPointers then
    Res.Add('dw note_catalog1, note_catalog2, note_catalog3, note_catalog4');
  Res.Add('order1: dw ' + ArrayHelper(chDuty1, OrderMatrix[0]));
  Res.Add('order2: dw ' + ArrayHelper(chDuty2, OrderMatrix[1]));
  Res.Add('order3: dw ' + ArrayHelper(chWave, OrderMatrix[2]));
  Res.Add('order4: dw ' + ArrayHelper(chNoise, OrderMatrix[3]));

  Result := Res.Text;
  Res.Free;
end;

function RenderInstruments(Instruments: TInstrumentBank; Limit: Integer): string;
var
  ResultSL: TStringList;
  AsmInstrument: TAsmInstrument;
  I, J: integer;
  TypePrefix: string;
  HighMask: byte;
begin
  ResultSL := TStringList.Create;

  for I := Low(Instruments) to Limit do
  begin
    WriteStr(TypePrefix, Instruments[I].Type_);
    ResultSL.Add(Format('%s%s:', [TypePrefix, 'inst' + IntToStr(I)]));

    AsmInstrument := InstrumentToBytes(Instruments[I]);

    if Instruments[I].Type_ = itNoise then
    begin
      ResultSL.Add('db '+IntToStr(AsmInstrument[1])); // envelope

      HighMask := AsmInstrument[0];
      if Instruments[I].LengthEnabled then
        HighMask := HighMask or %01000000;
      if Instruments[I].CounterStep = swSeven then
        HighMask := HighMask or %10000000;
      ResultSL.Add('db '+IntToStr(HighMask));
    end
    else begin
      for J := Low(AsmInstrument) to High(AsmInstrument) do
        ResultSL.Add('db '+IntToStr(AsmInstrument[J]));
    end;

    if Instruments[I].SubpatternEnabled then
      ResultSL.Insert(ResultSL.Count-1, Format('dw %sSP%d', [TypePrefix, I]))
    else
      ResultSL.Insert(ResultSL.Count-1, 'dw 0');

    if Instruments[I].Type_ = itNoise then
      ResultSL.Add('ds 2');

    ResultSL.Add('');
  end;

  Result := ResultSL.Text;
  ResultSL.Free;
end;

function RenderSubpatternCell(Cell: TCell; Last: Boolean): string;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  SL.Delimiter := ',';
  SL.StrictDelimiter := True;

  if (Cell.Note = NO_NOTE) then
    SL.Add('___')
  else
    SL.Add(IntToStr(Cell.Note));

  if Last and (Cell.Volume = 0) then
    SL.Add(IntToStr(1)) // Automatically insert jump back to row 1 if last cell
  else
    SL.Add(IntToStr(EnsureRange(Cell.Volume, 0, 32)));

  SL.Add('$' + EffectCodeToStr(Cell.EffectCode, Cell.EffectParams));

  // RGBDS thinks you're defining a new macro if you don't have a space first.
  Result := ' dn ' + SL.DelimitedText;
  SL.Free;
end;

function RenderSubpattern(Name: string; Pattern: TPattern): string;
var
  SL: TStringList;
  I: integer;
begin
  SL := TStringList.Create;
  SL.Add(Name + ':');

  for I := 0 to 31 do
    SL.Add(RenderSubpatternCell(Pattern[I], I = 31)); // TODO: hardcoded value

  Result := SL.Text;
  SL.Free;
end;

function RenderWaveforms(Waves: TWaveBank; Limit: Integer): string;
var
  SL, ResultSL: TStringList;
  I, J: integer;
begin
  ResultSL := TStringList.Create;

  for I := Low(Waves) to Limit do
  begin
    SL := TStringList.Create;
    SL.StrictDelimiter := True;
    SL.Delimiter := ',';

    J := Low(Waves[I]);
    while J < High(Waves[I]) do
    begin
      SL.Add(IntToStr((Waves[I, J] shl 4) or Waves[I, J + 1]));
      Inc(J, 2);
    end;
    ResultSL.Add(Format('wave%d: db %s', [I, SL.DelimitedText]));
    SL.Free;
  end;

  Result := ResultSL.Text;
  ResultSL.Free;
end;

procedure RenderSongToRGBDSAsm(Song: TSong; DescriptorName: String; Filename: string);
var
  OrderMatrix: TOrderMatrix;
  NoteCatalogs: TNoteCatalogs;
  EncodedPattern: TEncodedPattern;
  OutSL: TStringList;
  F: Text;
  I: Integer;
  Channel: TChannel;
  TypePrefix: String;
  UsedStuff: TUsedStuff;
begin
  OrderMatrix := BuildOrderMatrix(Song, False);
  UsedStuff := FindUsedStuff(Song, OrderMatrix);
  NoteCatalogs := BuildNoteCatalogs(Song, UsedStuff);

  OutSL := TStringList.Create;

  OutSL.Add('include "hUGE.inc"');
  OutSL.Add('');
  OutSL.Add('SECTION "'+DescriptorName+' Song Data", ROMX');
  OutSL.Add('');

  // Render song descriptor
  OutSL.Add(DescriptorName+'::');
  OutSL.Add('db '+IntToStr(Song.TicksPerRow[0])+', '
                 +IntToStr(Song.TicksPerRow[1])+', '
                 +IntToStr(Song.TicksPerRow[2])+', '
                 +IntToStr(Song.TicksPerRow[3]));
  OutSL.Add('db '+IntToStr(OrderCount(Song)*2));
  OutSL.Add('dw order1, order2, order3, order4');
  OutSL.Add('dw duty_instruments, wave_instruments, noise_instruments');
  OutSL.Add('dw routines');
  OutSL.Add('dw waves');
  OutSL.Add('dw note_catalog1, note_catalog2, note_catalog3, note_catalog4');
  OutSL.Add('');

  // Render order matrix
  OutSL.Add(RenderOrderTable(OrderMatrix));

  // Render channel note catalogs
  for Channel := Low(TChannel) to High(TChannel) do begin
    OutSL.Add(RenderRGBDSNoteCatalog('note_catalog' +
      IntToStr(Ord(Channel) + 1), NoteCatalogs[Channel]));
    OutSL.Add('');
  end;

  // Render instruments
  OutSL.Add('duty_instruments:');
  OutSL.Add(RenderInstruments(Song.Instruments.Duty, UsedStuff.HighestDutyInst));
  OutSL.Add('');

  OutSL.Add('wave_instruments:');
  OutSL.Add(RenderInstruments(Song.Instruments.Wave, UsedStuff.HighestWaveInst));
  OutSL.Add('');

  OutSL.Add('noise_instruments:');
  OutSL.Add(RenderInstruments(Song.Instruments.Noise, UsedStuff.HighestNoiseInst));
  OutSL.Add('');

  // Render routines
  OutSL.Add('routines:');
  for I := Low(TRoutineBank) to High(TRoutineBank) do begin
    OutSL.Add('__hUGE_Routine_'+IntToStr(I)+':');
    OutSL.Add(Song.Routines[I]);
    OutSL.Add('__end_hUGE_Routine_'+IntToStr(I)+':');
    OutSL.Add('ret');
    OutSL.Add('');
  end;

  // Render waves
  OutSL.Add('waves:');
  OutSL.Add(RenderWaveforms(Song.Waves, UsedStuff.HighestWaveform));

  // Render channel-specific compressed patterns
  for Channel := Low(TChannel) to High(TChannel) do
    for I := 0 to Song.Patterns.Count - 1 do
      if PatternIsUsedInChannel(Song.Patterns.Keys[I], Channel,
        UsedStuff) then begin
        EncodedPattern := EncodePattern(Song.Patterns.Data[I]^,
          NoteCatalogs[Channel]);
        OutSL.Add(RenderRGBDSCompressedPattern(
          ChannelPatternName(Channel, Song.Patterns.Keys[I]),
          EncodedPattern));
      end;

  // Render subpatterns
  for I := Low(Song.Instruments.All) to High(Song.Instruments.All) do
    if InstrumentIsUsed(ModInst(I), Song.Instruments.All[I].Type_, UsedStuff) then
      with Song.Instruments.All[I] do begin
        if SubpatternEnabled then begin
          WriteStr(TypePrefix, Type_);
          OutSL.Add(RenderSubpattern(TypePrefix+'SP' + IntToStr(ModInst(I)), Subpattern));
        end;
      end;

  AssignFile(F, Filename);
  Rewrite(F);
  Write(F, OutSL.Text);
  CloseFile(F);

  OutSL.Free;
  FreeUsedStuff(UsedStuff);
end;

procedure RectifyGBSFile(GBSFile: string);
{ This procedure removes the useless $400 bytes located after the GBS header.
  Since RGBDS has no feature to simply offset a section of code, we have to
  manually introduce padding and then remove it with this silly routine. }
var
  Stream: TFileStream;
  Buffer: array of byte;
begin
  Stream := TFileStream.Create(GBSFile, fmOpenRead);
  SetLength(Buffer, Stream.Size - $400);
  Stream.Read(Buffer[0], $70);
  Stream.Seek($400, soCurrent); // Skip the empty byte padding
  Stream.Read(Buffer[$70], Stream.Size - Stream.Position);
  Stream.Free;

  Stream := TFileStream.Create(GBSFile, fmOpenWrite);
  Stream.WriteBuffer(Buffer[0], Length(Buffer));
  Stream.Free;
end;

procedure AssembleSong(Song: TSong; Filename: string; Mode: TExportMode);
var
  OrderMatrix: TOrderMatrix;
  NoteCatalogs: TNoteCatalogs;
  EncodedPattern: TEncodedPattern;
  OutFile: Text;
  I: integer;
  Channel: TChannel;
  TypePrefix: String;
  Proc: TProcess;
  FilePath: string;
  RenameSucceeded: Boolean;
  UsedStuff: TUsedStuff;

  procedure Die;
  var
    OutSL: TStringList;
  begin
    OutSL := TStringList.Create;
    try
      OutSL.LoadFromStream(Proc.Output);
      raise EAssemblyException.Create(Proc.Executable, OutSL.Text);
    finally
      OutSL.Free;
    end;
  end;

  procedure WriteHTT(F: string; S: string);
  begin
    AssignFile(OutFile, F);
    Rewrite(OutFile);
    Write(OutFile, S);
    CloseFile(OutFile);
  end;

  function Assemble(OutFile: string; InFile: string; Defines: array of string): integer;
  var
    Define: string;
  begin
    Proc.Executable := 'rgbasm';
    Proc.Parameters.Clear;
    Proc.Parameters.Add('--include');
    Proc.Parameters.Add(ConcatPaths([CacheDir, 'render']));
    Proc.Parameters.Add('--include');
    Proc.Parameters.Add(ConcatPaths([RuntimeDir, 'hUGEDriver']));
    Proc.Parameters.Add('-o' + OutFile);
    for Define in Defines do
      // HACK: This just removes all instances of double quotes to avoid a bug
      // where arguments don't get passed in right with double quotes.
      Proc.Parameters.Add('-D' + ReplaceStr(Define, '"', ''));
    Proc.Parameters.Add(InFile);
    Proc.Execute;

    Result := Proc.ExitStatus;
  end;

  function Link(OutFile: string; InFiles: array of string; Map: string = '';
    Sym: string = ''): integer;
  var
    InFile: string;
  begin
    Proc.Executable := 'rgblink';
    Proc.Parameters.Clear;
    Proc.Parameters.Add('-o' + OutFile);
    if Map <> '' then
      Proc.Parameters.Add('-m' + Map);
    if Sym <> '' then
      Proc.Parameters.Add('-n' + Sym);
    for InFile in InFiles do
      Proc.Parameters.Add(InFile);
    Proc.Execute;

    Result := Proc.ExitStatus;
  end;

  function Fix(GBFile: string): integer;
  begin
    Proc.Executable := 'rgbfix';
    Proc.Parameters.Clear;
    Proc.Parameters.Add('-p0');
    Proc.Parameters.Add('-v');
    Proc.Parameters.Add(GBFile);
    Proc.Execute;

    Result := Proc.ExitStatus;
  end;
begin
  OrderMatrix := BuildOrderMatrix(Song, False);
  UsedStuff := FindUsedStuff(Song, OrderMatrix);
  NoteCatalogs := BuildNoteCatalogs(Song, UsedStuff);

  if not DirectoryExists(ConcatPaths([CacheDir, 'render'])) then
    CreateDir(ConcatPaths([CacheDir, 'render']));

  FilePath := Filename;
  Filename := ConcatPaths([CacheDir, 'render', ExtractFileNameWithoutExt(ExtractFileNameOnly(Filename))]);

  WriteHTT(ConcatPaths([CacheDir, 'render', 'wave.htt']), RenderWaveforms(Song.Waves, UsedStuff.HighestWaveform));
  WriteHTT(ConcatPaths([CacheDir, 'render', 'order.htt']),
    RenderOrderTable(OrderMatrix, True));
  WriteHTT(ConcatPaths([CacheDir, 'render', 'duty_instrument.htt']),  RenderInstruments(Song.Instruments.Duty, UsedStuff.HighestDutyInst));
  WriteHTT(ConcatPaths([CacheDir, 'render', 'wave_instrument.htt']),  RenderInstruments(Song.Instruments.Wave, UsedStuff.HighestWaveInst));
  WriteHTT(ConcatPaths([CacheDir, 'render', 'noise_instrument.htt']), RenderInstruments(Song.Instruments.Noise, UsedStuff.HighestNoiseInst));
  for I := Low(TRoutineBank) to High(TRoutineBank) do
    WriteHTT(ConcatPaths([CacheDir, 'render', 'routine'+IntToStr(I)+'.htt']), Song.Routines[I]);

  AssignFile(OutFile, ConcatPaths([CacheDir, 'render', 'pattern.htt']));
  Rewrite(OutFile);

  for Channel := Low(TChannel) to High(TChannel) do begin
    Write(OutFile, RenderRGBDSNoteCatalog('note_catalog' +
      IntToStr(Ord(Channel) + 1), NoteCatalogs[Channel]));
    WriteLn(OutFile);
  end;

  for Channel := Low(TChannel) to High(TChannel) do
    for I := 0 to Song.Patterns.Count - 1 do
      if PatternIsUsedInChannel(Song.Patterns.Keys[I], Channel,
        UsedStuff) then begin
        EncodedPattern := EncodePattern(Song.Patterns.Data[I]^,
          NoteCatalogs[Channel]);
        Write(OutFile, RenderRGBDSCompressedPattern(
          ChannelPatternName(Channel, Song.Patterns.Keys[I]),
          EncodedPattern));
      end;

  CloseFile(OutFile);

  AssignFile(OutFile, ConcatPaths([CacheDir, 'render', 'subpattern.htt']));
  Rewrite(OutFile);

  for I := Low(Song.Instruments.All) to High(Song.Instruments.All) do
    if InstrumentIsUsed(ModInst(I), Song.Instruments.All[I].Type_, UsedStuff) then
      with Song.Instruments.All[I] do begin
        if SubpatternEnabled then begin
          WriteStr(TypePrefix, Type_);
          Write(OutFile, RenderSubpattern(TypePrefix+'SP' + IntToStr(ModInst(I)), Subpattern));
        end;
      end;

  CloseFile(OutFile);

  // Build the file
  Proc := TProcess.Create(nil);
  Proc.Options := Proc.Options + [poWaitOnExit, poUsePipes, poStdErrToOutput, poNoConsole];

  try
    // Assemble
    if Mode = emPreview then
    begin
      if Assemble(Filename + '_driver.obj',
                  ConcatPaths([RuntimeDir, 'hUGEDriver', 'hUGEDriver.asm']),
                  ['PREVIEW_MODE']) <> 0 then Die;
    end
    else if Assemble(Filename + '_driver.obj', ConcatPaths([RuntimeDir, 'hUGEDriver', 'hUGEDriver.asm']), []) <> 0 then Die;

    if Assemble(Filename + '_song.obj',
                ConcatPaths([RuntimeDir, 'hUGEDriver', 'song.asm']),
                ['SONG_DESCRIPTOR=song',
                 'ORDER_COUNT='+IntToStr(OrderCount(Song)*2),
                 'TICKS0='+IntToStr(Song.TicksPerRow[0]),
                 'TICKS1='+IntToStr(Song.TicksPerRow[1]),
                 'TICKS2='+IntToStr(Song.TicksPerRow[2]),
                 'TICKS3='+IntToStr(Song.TicksPerRow[3])]) <> 0 then Die;

    if Mode = emGBS then
    begin
      if Assemble(Filename + '_gbs.obj',
                  ConcatPaths([RuntimeDir, 'hUGEDriver', 'gbs.asm']),
                  ['SONG_DESCRIPTOR=song',
                   'GBS_TITLE="'+PadRight(LeftStr(Song.Name, 32), 32)+'"',
                   'GBS_AUTHOR="'+PadRight(LeftStr(Song.Artist, 32), 32)+'"',
                   'GBS_COPYRIGHT="'+PadRight(IntToStr(CurrentYear), 32)+'"',
                   'TIMER_MODULO='+IntToStr(IfThen(Song.TimerEnabled, Song.TimerDivider, 0)),
                   'TIMER_CONTROL='+IntToStr(IfThen(Song.TimerEnabled, 4, 0))]) <> 0 then Die;
    end
    else if Assemble(Filename + '_player.obj',
                     ConcatPaths([RuntimeDir, 'hUGEDriver', 'player.asm']),
                     ['SONG_DESCRIPTOR=song',
                      IfThen(Song.TimerEnabled, 'USE_TIMER=1', ''),
                      'TIMER_MODULO='+IntToStr(Song.TimerDivider)]) <> 0 then Die;

    // Link
    if Mode = emGBS then
    begin
      if Link(Filename + '.gbs',
              [Filename + '_driver.obj',
               Filename + '_song.obj',
               Filename + '_gbs.obj']) <> 0 then Die;
    end
    else
    begin
      if Link(Filename + '.gb',
              [Filename + '_driver.obj',
               Filename + '_song.obj',
               Filename + '_player.obj'],
              Filename + '.map',
              Filename + '.sym') <> 0 then Die;
    end;

    // Fix
    if Mode = emGBS then
      RectifyGBSFile(Filename + '.gbs')
    else
    begin
      if (Fix(Filename + '.gb') <> 0) then
        Die;
    end;

    // Move to destination
    if Mode <> emPreview then
    begin
      if FileExists(FilePath) then
        DeleteFile(FilePath);

      case Mode of
        emGBS: RenameSucceeded := RenameFile(Filename + '.gbs', FilePath);
        else RenameSucceeded := RenameFile(Filename + '.gb', FilePath);
      end;

      if not RenameSucceeded then
        raise ECodegenRenameException.Create(FilePath);

      {$ifdef DEVELOPMENT}
      RenameFile(Filename + '.sym', FilePath + '.sym');
      RenameFile(Filename + '.map', FilePath + '.map');
      {$endif}
      {$ifdef PRODUCTION}
      DeleteFile(Filename + '.sym');
      DeleteFile(Filename + '.map');
      {$endif}

      DeleteFile(Filename + '_driver.obj');
      DeleteFile(Filename + '_song.obj');
      DeleteFile(Filename + '_player.obj');
      DeleteFile(Filename + '_gbs.obj');
    end;
  finally
    Proc.Free;
    FreeUsedStuff(UsedStuff);
  end;
end;

{ EAssemblyException }

constructor EAssemblyException.Create(Prog, Msg: String);
begin
  inherited Create(Msg);
  Self.ProgramName := Prog;
end;

end.
