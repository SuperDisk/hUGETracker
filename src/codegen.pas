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

function OrderCount(const Song: TSong): Integer;
var
  OrderMatrix: TOrderMatrix;
begin
  OrderMatrix := Song.OrderMatrix;
  Result := MaxIntValue([High(OrderMatrix[0]), High(OrderMatrix[1]),
    High(OrderMatrix[2]), High(OrderMatrix[3])]) * 2;
end;

procedure RenderSongToGBDKC(Song: TSong; DescriptorName: String; Filename: string; Bank: Integer = -1);
  function RenderGBDKCell(Cell: TCell): string;
  var
    SL: TStringList;
  begin
    SL := TStringList.Create;
    SL.Delimiter := ',';

    if (Cell.Note = NO_NOTE) or (NoteToCMap.IndexOf(Cell.Note) = -1) then
      SL.Add('___')
    else
      SL.Add(NoteToCMap.KeyData[Cell.Note]);

    if InRange(Cell.Instrument, 0, 15) then
      SL.Add(IntToStr(Cell.Instrument))
    else
      SL.Add('0');

    SL.Add('0x' + EffectCodeToStr(Cell.EffectCode, Cell.EffectParams));

    Result := SL.DelimitedText;
    SL.Free;
  end;

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

  function RenderGBDKPattern(Name: string; Pat: TPattern): string;
  var
    Cell: TCell;
  begin
    Result := 'static const unsigned char ' + Name + '[] = {' + LineEnding;
    for Cell in Pat do
      Result += '    DN(' + RenderGBDKCell(Cell) + '),' + LineEnding;
    Result += '};';
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

  function RenderGBDKOrder(Number: integer; Order: array of integer): string;
  var
    SL: TStringList;
    I: integer;
  begin
    SL := TStringList.Create;
    SL.StrictDelimiter := True;
    SL.Delimiter := ',';

    for I := Low(Order) to High(Order)-1 do // HACK: account for off-by-one error
      SL.Add('P' + IntToStr(Order[I]));

    Result := 'static const unsigned char* const order' + IntToStr(Number) + '[] = {';
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

  function RenderGBDKInstrumentBank(Name: string; Bank: TInstrumentBank): string;
  var
    I: integer;
    InstrType: String;
  begin
    case Bank[1].Type_ of
      itSquare: InstrType := 'hUGEDutyInstr_t';
      itWave: InstrType := 'hUGEWaveInstr_t';
      itNoise: InstrType := 'hUGENoiseInstr_t';
    end;
    Result := 'static const ' + InstrType + ' ' + Name + '[] = {'+LineEnding;
    for I := Low(Bank) to High(Bank) do begin
      Result += '    '+RenderGBDKInstrument(Bank[I], I) + ','+LineEnding;
    end;
    Result += '};';
  end;

  function RenderGBDKWaves(Waves: TWaveBank): string;
  var
    I, J: integer;
  begin
    Result := 'static const unsigned char waves[] = {'+LineEnding;
    for I := Low(Waves) to High(Waves) do
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
  OutSL: TStringList;
  I: integer;
  F: Text;
  TypePrefix: String;
begin
  Song := OptimizeSong(Song);

  OutSL := TStringList.Create;

  if Bank <> -1 then begin
    OutSL.Add('#pragma bank '+IntToStr(Bank));
    OutSL.Add('');
  end;

  OutSL.Add('#include "hUGEDriver.h"');
  OutSL.Add('#include <stddef.h>');
  OutSL.Add('');

  for I := 0 to Song.Patterns.Count - 1 do
    if PatternIsUsed(Song.Patterns.Keys[I], Song) then
      OutSL.Add(RenderGBDKPattern('P' + IntToStr(Song.Patterns.Keys[I]),
        Song.Patterns.Data[I]^));
  OutSL.Add('');

  for I := Low(Song.Instruments.All) to High(Song.Instruments.All) do
    with Song.Instruments.All[I] do begin
      if SubpatternEnabled then begin
        WriteStr(TypePrefix, Type_);
        OutSL.Add(RenderGBDKSubpattern(TypePrefix+'SP' + IntToStr(ModInst(I)), Subpattern));
      end;
    end;

  OutSL.Add(RenderGBDKOrder(1, Song.OrderMatrix[0]));
  OutSL.Add(RenderGBDKOrder(2, Song.OrderMatrix[1]));
  OutSL.Add(RenderGBDKOrder(3, Song.OrderMatrix[2]));
  OutSL.Add(RenderGBDKOrder(4, Song.OrderMatrix[3]));
  OutSL.Add('');

  OutSL.Add(RenderGBDKInstrumentBank('duty_instruments', Song.Instruments.Duty));
  OutSL.Add(RenderGBDKInstrumentBank('wave_instruments', Song.Instruments.Wave));
  OutSL.Add(RenderGBDKInstrumentBank('noise_instruments', Song.Instruments.Noise));
  OutSL.Add('');

  OutSL.Add(RenderGBDKWaves(Song.Waves));
  OutSL.Add('');

  if Bank <> -1 then
    OutSL.Add(Format('const void __at(%d) __bank_%s;', [Bank, DescriptorName]));

  OutSL.Add(Format(
    'const hUGESong_t %s = {%d, %d, %d, %d, %d, order1, order2, order3,'+
    'order4, duty_instruments, wave_instruments, noise_instruments, NULL, waves};',
    [DescriptorName,
     Song.TicksPerRow[0], Song.TicksPerRow[1], Song.TicksPerRow[2], Song.TicksPerRow[3],
     OrderCount(Song)
    ]));

  AssignFile(F, Filename);
  Rewrite(F);
  Write(F, OutSL.Text);
  CloseFile(F);

  OutSL.Free;
end;

function RenderOrderTable(OrderMatrix: TOrderMatrix): string;
  function ArrayHelper(Ints: array of integer): string;
  var
    I: integer;
    SL: TStringList;
  begin
    SL := TStringList.Create;
    SL.StrictDelimiter := True;
    SL.Delimiter := ',';
    for I := Low(Ints) to High(Ints)-1 do // HACK: account for the off-by-one error
      SL.Add('P' + IntToStr(Ints[I]));
    Result := SL.DelimitedText;
    SL.Free;
  end;

var
  Res: TStringList;
begin
  Res := TStringList.Create;

  Res.Add('order1: dw ' + ArrayHelper(OrderMatrix[0]));
  Res.Add('order2: dw ' + ArrayHelper(OrderMatrix[1]));
  Res.Add('order3: dw ' + ArrayHelper(OrderMatrix[2]));
  Res.Add('order4: dw ' + ArrayHelper(OrderMatrix[3]));

  Result := Res.Text;
  Res.Free;
end;

function RenderInstruments(Instruments: TInstrumentBank): string;
var
  ResultSL: TStringList;
  AsmInstrument: TAsmInstrument;
  I, J: integer;
  TypePrefix: string;
  HighMask: byte;
begin
  ResultSL := TStringList.Create;

  for I := Low(Instruments) to High(Instruments) do
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

function RenderCell(Cell: TCell): string;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  SL.Delimiter := ',';
  SL.StrictDelimiter := True;

  if (Cell.Note = NO_NOTE) or (NoteToDriverMap.IndexOf(Cell.Note) = -1) then
    SL.Add('___')
  else
    SL.Add(NoteToDriverMap.KeyData[Cell.Note]);

  if InRange(Cell.Instrument, 0, 15) then
    SL.Add(IntToStr(Cell.Instrument))
  else
    SL.Add('0');

  SL.Add('$' + EffectCodeToStr(Cell.EffectCode, Cell.EffectParams));

  // RGBDS thinks you're defining a new macro if you don't have a space first.
  Result := ' dn ' + SL.DelimitedText;
  SL.Free;
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

function RenderPattern(Name: string; Pattern: TPattern): string;
var
  SL: TStringList;
  I: integer;
begin
  SL := TStringList.Create;
  SL.Add(Name + ':');

  for I := Low(TPattern) to High(TPattern) do
    SL.Add(RenderCell(Pattern[I]));

  Result := SL.Text;
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

function RenderWaveforms(Waves: TWaveBank): string;
var
  SL, ResultSL: TStringList;
  I, J: integer;
begin
  ResultSL := TStringList.Create;

  for I := Low(Waves) to High(Waves) do
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
  OutSL: TStringList;
  F: Text;
  I: Integer;
  TypePrefix: String;
begin
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
  OutSL.Add('dw '+IntToStr(OrderCount(Song)));
  OutSL.Add('dw order1, order2, order3, order4');
  OutSL.Add('dw duty_instruments, wave_instruments, noise_instruments');
  OutSL.Add('dw routines');
  OutSL.Add('dw waves');
  OutSL.Add('');

  // Render order matrix
  OutSL.Add(RenderOrderTable(Song.OrderMatrix));

  // Render patterns
  for I := 0 to Song.Patterns.Count - 1 do
    if PatternIsUsed(Song.Patterns.Keys[I], Song) then
      OutSL.Add(RenderPattern('P' + IntToStr(Song.Patterns.Keys[I]), Song.Patterns.Data[I]^));

  // Render subpatterns
  for I := Low(Song.Instruments.All) to High(Song.Instruments.All) do
    with Song.Instruments.All[I] do begin
      if SubpatternEnabled then begin
        WriteStr(TypePrefix, Type_);
        OutSL.Add(RenderSubpattern(TypePrefix+'SP' + IntToStr(ModInst(I)), Subpattern));
      end;
    end;

  // Render instruments
  OutSL.Add('duty_instruments:');
  OutSL.Add(RenderInstruments(Song.Instruments.Duty));
  OutSL.Add('');

  OutSL.Add('wave_instruments:');
  OutSL.Add(RenderInstruments(Song.Instruments.Wave));
  OutSL.Add('');

  OutSL.Add('noise_instruments:');
  OutSL.Add(RenderInstruments(Song.Instruments.Noise));
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
  OutSL.Add(RenderWaveforms(Song.Waves));

  AssignFile(F, Filename);
  Rewrite(F);
  Write(F, OutSL.Text);
  CloseFile(F);

  OutSL.Free;
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
  OutFile: Text;
  I: integer;
  TypePrefix: String;
  Proc: TProcess;
  FilePath: string;
  RenameSucceeded: Boolean;

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
    Proc.Parameters.Add('-i' + ConcatPaths([CacheDir, 'render']));
    Proc.Parameters.Add('-i' + ConcatPaths([RuntimeDir, 'hUGEDriver']));
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
  Song := OptimizeSong(Song);

  if not DirectoryExists(ConcatPaths([CacheDir, 'render'])) then
    CreateDir(ConcatPaths([CacheDir, 'render']));

  FilePath := Filename;
  Filename := ConcatPaths([CacheDir, 'render', ExtractFileNameWithoutExt(ExtractFileNameOnly(Filename))]);

  WriteHTT(ConcatPaths([CacheDir, 'render', 'wave.htt']), RenderWaveforms(Song.Waves));
  WriteHTT(ConcatPaths([CacheDir, 'render', 'order.htt']), RenderOrderTable(Song.OrderMatrix));
  WriteHTT(ConcatPaths([CacheDir, 'render', 'duty_instrument.htt']),  RenderInstruments(Song.Instruments.Duty));
  WriteHTT(ConcatPaths([CacheDir, 'render', 'wave_instrument.htt']),  RenderInstruments(Song.Instruments.Wave));
  WriteHTT(ConcatPaths([CacheDir, 'render', 'noise_instrument.htt']), RenderInstruments(Song.Instruments.Noise));
  for I := Low(TRoutineBank) to High(TRoutineBank) do
    WriteHTT(ConcatPaths([CacheDir, 'render', 'routine'+IntToStr(I)+'.htt']), Song.Routines[I]);

  AssignFile(OutFile, ConcatPaths([CacheDir, 'render', 'pattern.htt']));
  Rewrite(OutFile);

  for I := 0 to Song.Patterns.Count - 1 do
    if PatternIsUsed(Song.Patterns.Keys[I], Song) then
      Write(OutFile, RenderPattern('P' + IntToStr(Song.Patterns.Keys[I]),
        Song.Patterns.Data[I]^));

  CloseFile(OutFile);

  AssignFile(OutFile, ConcatPaths([CacheDir, 'render', 'subpattern.htt']));
  Rewrite(OutFile);

  for I := Low(Song.Instruments.All) to High(Song.Instruments.All) do
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
    else if Assemble(Filename + '_driver.obj', 'hUGEDriver/hUGEDriver.asm', []) <> 0 then Die;

    if Assemble(Filename + '_song.obj',
                ConcatPaths([RuntimeDir, 'hUGEDriver', 'song.asm']),
                ['SONG_DESCRIPTOR=song',
                 'ORDER_COUNT='+IntToStr(OrderCount(Song)),
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
  end;
end;

{ EAssemblyException }

constructor EAssemblyException.Create(Prog, Msg: String);
begin
  inherited Create(Msg);
  Self.ProgramName := Prog;
end;

end.
