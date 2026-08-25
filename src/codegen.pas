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
function GetLastGeneratedSongSize: Integer;

implementation

uses AvgLvlTree, fgl;

type
  TNoteFrequencyMap = specialize TFPGMap<Integer, Integer>;
  TNoteCatalog = array of Integer;
  TNoteCatalogs = array[TChannel] of TNoteCatalog;
  TEncodedPattern = array of Integer;
  TRoutineDictionary = array of TEncodedPattern;

  TEncodedPatternEntry = record
    Channel: TChannel;
    PatternKey: Integer;
    Pattern: TEncodedPattern;
  end;
  TEncodedPatterns = array of TEncodedPatternEntry;

  TChannelEncoding = record
    Patterns: TEncodedPatterns;
    DictionaryIndex: Integer;
  end;

  TPatternDictionary = record
    Routines: TRoutineDictionary;
    ChannelMask: Byte;
  end;
  TPatternDictionaries = array of TPatternDictionary;

  TSongEncoding = record
    Channels: array[TChannel] of TChannelEncoding;
    Dictionaries: TPatternDictionaries;
  end;

  TGroupEncoding = record
    Patterns: TEncodedPatterns;
    Routines: TRoutineDictionary;
    EncodedSize: Integer;
  end;

  TRoutineCandidateStats = record
    Occurrences: Integer;
    LastStream: Integer;
    NextOffset: Integer;
  end;

  TRoutineCandidate = record
    Hash: LongWord;
    StreamIndex: Integer;
    StartOffset: Integer;
    Length: Integer;
    EncodedBytes: Integer;
    Stats: TRoutineCandidateStats;
  end;
  TRoutineCandidates = array of TRoutineCandidate;

  TNoteCatalogCandidate = record
    NoteRecord: Integer;
    Frequency: Integer;
  end;

  TInstrumentIndexMap = array[TInstrumentType, 0..15] of Integer;
  TInstrumentCounts = array[TInstrumentType] of Integer;
  TWaveformIndexMap = array[0..15] of Integer;

  TUsedStuff = record
    InstrumentMap: TInstrumentIndexMap;
    InstrumentCount: TInstrumentCounts;
    WaveformMap: TWaveformIndexMap;
    WaveformCount: Integer;
    UsedPatterns: array[TChannel] of TAvgLvlTree;
  end;

const
  // Pattern bytecode:
  //   0..180   catalog index
  //   181..253 literal note (opcode - 181), followed by the other two DN bytes
  //   254      call, followed by a little-endian routine address
  //   255      return from a routine
  NOTE_CATALOG_SIZE = 181;
  NOTE_LITERAL_BASE = NOTE_CATALOG_SIZE;
  PATTERN_CALL = 254;
  PATTERN_RETURN = 255;
  PATTERN_CALL_SIZE = 3;
  PATTERN_RETURN_SIZE = 1;

  // Calls use these compact internal tokens while the compressor is working.
  // They are rendered as PATTERN_CALL plus a two-byte absolute address.
  INTERNAL_CALL_BASE = 201;
  MAX_PATTERN_ROUTINES = 54;
  // hUGEDriver sizes each channel's return stack from this upper bound.
  // Keep MAX_PATTERN_DEPTH in hUGEDriver.asm in sync with this value.
  MAX_PATTERN_DEPTH = 4;
  MAX_ROUTINE_LENGTH = 64;

var
  LastGeneratedSongSize: Integer = -1;

function GetLastGeneratedSongSize: Integer;
begin
  Result := LastGeneratedSongSize;
end;

function CompareIntPointers(Data1, Data2: Pointer): integer;
begin
  Result := Integer(Data1^) - Integer(Data2^);
end;

function ChannelInstrumentType(Channel: TChannel): TInstrumentType;
begin
  case Channel of
    chDuty1, chDuty2: Result := itSquare;
    chWave: Result := itWave;
    chNoise: Result := itNoise;
  end;
end;

function FindUsedStuff(const Song: TSong;
  const OrderMatrix: TOrderMatrix): TUsedStuff;
var
  I, J: Integer;
  Channel: TChannel;
  InstrumentType: TInstrumentType;
  Pat: PPattern;
  Cell: TCell;
  Instr: TInstrument;
  Waveform: Integer;
  UsedInstruments: array[TInstrumentType, 1..15] of Boolean;
  UsedWaveforms: array[0..15] of Boolean;
begin
  FillChar(UsedInstruments, SizeOf(UsedInstruments), 0);
  FillChar(UsedWaveforms, SizeOf(UsedWaveforms), 0);
  for Channel := Low(TChannel) to High(TChannel) do
    Result.UsedPatterns[Channel] := TAvgLvlTree.Create(@CompareIntPointers);
  for InstrumentType := Low(TInstrumentType) to High(TInstrumentType) do begin
    Result.InstrumentCount[InstrumentType] := 0;
    for I := 0 to 15 do Result.InstrumentMap[InstrumentType, I] := -1;
    Result.InstrumentMap[InstrumentType, 0] := 0;
  end;
  Result.WaveformCount := 0;
  for I := 0 to 15 do Result.WaveformMap[I] := -1;

  for I := Low(OrderMatrix) to High(OrderMatrix) do begin
    Channel := TChannel(I);
    InstrumentType := ChannelInstrumentType(Channel);
    for J := Low(OrderMatrix[I]) to High(OrderMatrix[I])-1 do begin
      if Result.UsedPatterns[Channel].Find(@OrderMatrix[I, J]) <> nil then
        Continue;

      Result.UsedPatterns[Channel].Add(@OrderMatrix[I, J]);

      Pat := Song.Patterns.KeyData[OrderMatrix[I, J]];
      for Cell in Pat^ do begin
        if (Cell.EffectCode = $9) and (Channel = chWave) then begin
          Waveform := Cell.EffectParams.Value;
          if InRange(Waveform, 0, 15) then
            UsedWaveforms[Waveform] := True;
        end;

        if InRange(Cell.Instrument, 1, 15) then
          UsedInstruments[InstrumentType, Cell.Instrument] := True;
      end;
    end;
  end;

  // A used wave instrument can select waveforms both initially and from its
  // subpattern. Ignore subpatterns belonging to instruments that will not be
  // exported.
  for I := 1 to 15 do begin
    if not UsedInstruments[itWave, I] then Continue;
    Instr := Song.Instruments.Wave[I];
    if InRange(Instr.Waveform, 0, 15) then
      UsedWaveforms[Instr.Waveform] := True;
    if Instr.SubpatternEnabled then
      for Cell in Instr.Subpattern do
        if (Cell.EffectCode = $9) and
          InRange(Cell.EffectParams.Value, 0, 15) then
          UsedWaveforms[Cell.EffectParams.Value] := True;
  end;

  // Keep source ordering stable while packing out holes.
  for InstrumentType := Low(TInstrumentType) to High(TInstrumentType) do
    for I := 1 to 15 do
      if UsedInstruments[InstrumentType, I] then begin
        Inc(Result.InstrumentCount[InstrumentType]);
        Result.InstrumentMap[InstrumentType, I] :=
          Result.InstrumentCount[InstrumentType];
      end;
  for I := 0 to 15 do
    if UsedWaveforms[I] then begin
      Result.WaveformMap[I] := Result.WaveformCount;
      Inc(Result.WaveformCount);
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

function InstrumentIsUsed(Instrument: Integer; Type_: TInstrumentType;
  const UsedStuff: TUsedStuff): Boolean;
begin
  Result := InRange(Instrument, 1, 15) and
    (UsedStuff.InstrumentMap[Type_, Instrument] > 0);
end;

function RemapInstrument(Instrument: Integer; Type_: TInstrumentType;
  const UsedStuff: TUsedStuff): Integer;
begin
  if InRange(Instrument, 1, 15) and
    (UsedStuff.InstrumentMap[Type_, Instrument] > 0) then
    Result := UsedStuff.InstrumentMap[Type_, Instrument]
  else
    Result := 0;
end;

function RemapWaveform(Waveform: Integer;
  const UsedStuff: TUsedStuff): Integer;
begin
  if InRange(Waveform, 0, 15) and
    (UsedStuff.WaveformMap[Waveform] >= 0) then
    Result := UsedStuff.WaveformMap[Waveform]
  else
    Result := Waveform;
end;

function CellToNoteRecord(const Cell: TCell; Channel: TChannel;
  const UsedStuff: TUsedStuff): Integer;
var
  Note, Instrument, Effect, EffectParams: Integer;
  B1, B2, B3: Byte;
begin
  if InRange(Cell.Note, 0, HIGHEST_NOTE) then
    Note := Cell.Note
  else
    Note := LAST_NOTE;

  Instrument := RemapInstrument(Cell.Instrument,
    ChannelInstrumentType(Channel), UsedStuff);

  EffectParams := Cell.EffectParams.Value;
  if (Channel = chWave) and (Cell.EffectCode = $9) then
    EffectParams := RemapWaveform(EffectParams, UsedStuff);
  Effect := (Cell.EffectCode shl 8) or EffectParams;
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
        NoteRecord := CellToNoteRecord(Pattern^[CellIndex], Channel,
          UsedStuff);
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

  CatalogLength := 0;
  while (CatalogLength < Length(Candidates)) and
    (CatalogLength < NOTE_CATALOG_SIZE) and
    (Candidates[CatalogLength].Frequency >= 2) do
    Inc(CatalogLength);
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

function EncodePattern(const Pattern: TPattern; Channel: TChannel;
  const Catalog: TNoteCatalog;
  const UsedStuff: TUsedStuff): TEncodedPattern;
var
  Row, CatalogIndex: Integer;
  NoteRecord: Integer;
begin
  Result := nil;
  SetLength(Result, Length(Pattern));
  for Row := Low(TPattern) to High(TPattern) do begin
    NoteRecord := CellToNoteRecord(Pattern[Row], Channel, UsedStuff);
    CatalogIndex := CatalogIndexOf(Catalog, NoteRecord);
    if CatalogIndex >= 0 then
      Result[Row] := CatalogIndex
    else
      Result[Row] := -NoteRecord - 1;
  end;
end;

function RoutineTokenEligible(Token: Integer): Boolean;
begin
  Result := InRange(Token, 0, NOTE_CATALOG_SIZE - 1) or
    InRange(Token, INTERNAL_CALL_BASE,
      INTERNAL_CALL_BASE + MAX_PATTERN_ROUTINES - 1);
end;

function EncodedTokenSize(Token: Integer): Integer;
begin
  if Token < 0 then
    Result := 3
  else if InRange(Token, INTERNAL_CALL_BASE,
    INTERNAL_CALL_BASE + MAX_PATTERN_ROUTINES - 1) then
    Result := PATTERN_CALL_SIZE
  else
    Result := 1;
end;

function EncodedStreamSize(const Stream: TEncodedPattern): Integer;
var
  Token: Integer;
begin
  Result := 0;
  for Token in Stream do Inc(Result, EncodedTokenSize(Token));
end;

function CandidateMatches(const Stream: TEncodedPattern; Offset: Integer;
  const Candidate: TEncodedPattern): Boolean;
var
  I: Integer;
begin
  if Offset + Length(Candidate) > Length(Stream) then Exit(False);
  for I := 0 to Length(Candidate) - 1 do
    if Stream[Offset + I] <> Candidate[I] then Exit(False);
  Result := True;
end;

procedure ReplaceCandidate(var Stream: TEncodedPattern;
  const Candidate: TEncodedPattern; Replacement: Integer);
var
  NewStream: TEncodedPattern;
  Offset, NewLength: Integer;
begin
  SetLength(NewStream, Length(Stream));
  Offset := 0;
  NewLength := 0;
  while Offset < Length(Stream) do begin
    if CandidateMatches(Stream, Offset, Candidate) then begin
      NewStream[NewLength] := Replacement;
      Inc(NewLength);
      Inc(Offset, Length(Candidate));
    end
    else begin
      NewStream[NewLength] := Stream[Offset];
      Inc(NewLength);
      Inc(Offset);
    end;
  end;
  SetLength(NewStream, NewLength);
  Stream := NewStream;
end;

procedure InlineRoutine(var Stream: TEncodedPattern; RoutineIndex: Integer;
  const Body: TEncodedPattern);
var
  NewStream: TEncodedPattern;
  Token, BodyToken, CallToken, CallCount, NewLength: Integer;
begin
  CallToken := INTERNAL_CALL_BASE + RoutineIndex;
  CallCount := 0;
  for Token in Stream do
    if Token = CallToken then Inc(CallCount);
  if CallCount = 0 then Exit;

  SetLength(NewStream, Length(Stream) +
    CallCount * (Length(Body) - 1));
  NewLength := 0;
  for Token in Stream do begin
    if Token = CallToken then begin
      for BodyToken in Body do begin
        NewStream[NewLength] := BodyToken;
        Inc(NewLength);
      end;
    end
    else begin
      NewStream[NewLength] := Token;
      Inc(NewLength);
    end;
  end;
  Stream := NewStream;
end;

function BuildGroupEncoding(const Song: TSong; ChannelMask: Byte;
  const UsedStuff: TUsedStuff;
  const Catalogs: TNoteCatalogs; Optimize: Boolean): TGroupEncoding;
var
  Candidates: TRoutineCandidates;
  CandidateBuckets: array of Integer;
  CandidateCount: Integer;
  Stats: TRoutineCandidateStats;
  BestCandidate: TEncodedPattern;
  PatternIndex, EntryIndex, RoutineIndex, CandidateLength: Integer;
  CandidateBytes, RoutineBytes: Integer;
  StreamIndex, CandidateIndex, BestCandidateIndex: Integer;
  I, Gain, BestGain, BestScore: Integer;
  Score, DictionaryBytes, Replacement: Integer;
  ReferenceCounts, RoutineMap: array of Integer;
  Active: array of Boolean;
  BestRoutine, Saving, BestSaving, NewRoutineIndex: Integer;
  RoutineDepth, MaxPrefixDepth: array of Integer;
  DepthVisitState: array of Byte;
  DepthCandidate, DepthCandidateCost, InlineCost: Integer;
  Channel: TChannel;
  CompactedRoutines: TRoutineDictionary;
  Group: ^TGroupEncoding;

  function StreamToken(AStreamIndex, Offset: Integer): Integer;
  begin
    if AStreamIndex < Length(Group^.Patterns) then
      Result := Group^.Patterns[AStreamIndex].Pattern[Offset]
    else
      Result := Group^.Routines[
        AStreamIndex - Length(Group^.Patterns)][Offset];
  end;

  function CandidateSpanMatches(const ACandidate: TRoutineCandidate;
    const Stream: TEncodedPattern; StartOffset: Integer): Boolean;
  var
    Offset: Integer;
  begin
    for Offset := 0 to ACandidate.Length - 1 do
      if StreamToken(ACandidate.StreamIndex,
        ACandidate.StartOffset + Offset) <> Stream[StartOffset + Offset] then
        Exit(False);
    Result := True;
  end;

  procedure GrowCandidateHash;
  var
    NewBuckets: array of Integer;
    NewSize, Index, Slot: Integer;
  begin
    if Length(CandidateBuckets) = 0 then
      NewSize := 1024
    else
      NewSize := Length(CandidateBuckets) * 2;
    SetLength(NewBuckets, NewSize);
    for Index := 0 to CandidateCount - 1 do begin
      Slot := Candidates[Index].Hash and LongWord(NewSize - 1);
      while NewBuckets[Slot] <> 0 do
        Slot := (Slot + 1) and (NewSize - 1);
      NewBuckets[Slot] := Index + 1;
    end;
    CandidateBuckets := NewBuckets;
  end;

  procedure InitializeCandidates;
  begin
    Candidates := nil;
    CandidateBuckets := nil;
    CandidateCount := 0;
    GrowCandidateHash;
  end;

  procedure RecordCandidateOccurrence(const Stream: TEncodedPattern;
    AStreamIndex, AStart, ALength, AEncodedBytes: Integer; AHash: LongWord);
  var
    Index, Slot: Integer;
  begin
    if (CandidateCount + 1) * 10 >= Length(CandidateBuckets) * 7 then
      GrowCandidateHash;

    Slot := AHash and LongWord(Length(CandidateBuckets) - 1);
    while CandidateBuckets[Slot] <> 0 do begin
      Index := CandidateBuckets[Slot] - 1;
      if (Candidates[Index].Hash = AHash) and
        (Candidates[Index].Length = ALength) and
        CandidateSpanMatches(Candidates[Index], Stream, AStart) then begin
        if (Candidates[Index].Stats.LastStream <> AStreamIndex) or
          (AStart >= Candidates[Index].Stats.NextOffset) then begin
          Inc(Candidates[Index].Stats.Occurrences);
          Candidates[Index].Stats.LastStream := AStreamIndex;
          Candidates[Index].Stats.NextOffset := AStart + ALength;
        end;
        Exit;
      end;
      Slot := (Slot + 1) and (Length(CandidateBuckets) - 1);
    end;

    if CandidateCount = Length(Candidates) then begin
      if CandidateCount = 0 then
        SetLength(Candidates, 1024)
      else
        SetLength(Candidates, CandidateCount * 2);
    end;
    Candidates[CandidateCount].Hash := AHash;
    Candidates[CandidateCount].StreamIndex := AStreamIndex;
    Candidates[CandidateCount].StartOffset := AStart;
    Candidates[CandidateCount].Length := ALength;
    Candidates[CandidateCount].EncodedBytes := AEncodedBytes;
    Candidates[CandidateCount].Stats.Occurrences := 1;
    Candidates[CandidateCount].Stats.LastStream := AStreamIndex;
    Candidates[CandidateCount].Stats.NextOffset := AStart + ALength;
    CandidateBuckets[Slot] := CandidateCount + 1;
    Inc(CandidateCount);
  end;

  procedure RecordCandidate(const Stream: TEncodedPattern;
    AStreamIndex: Integer);
  var
    AStart, ALength, Token, Index, AEncodedBytes: Integer;
    AHash: LongWord;
  begin
    for AStart := 0 to Length(Stream) - 1 do begin
      if not RoutineTokenEligible(Stream[AStart]) then Continue;
      AHash := $811C9DC5;
      AEncodedBytes := 0;
      for ALength := 1 to MAX_ROUTINE_LENGTH do begin
        Index := AStart + ALength - 1;
        if Index >= Length(Stream) then Break;
        Token := Stream[Index];
        if not RoutineTokenEligible(Token) then Break;
        AHash := ((AHash shl 5) or (AHash shr 27)) xor LongWord(Token + 1);
        Inc(AEncodedBytes, EncodedTokenSize(Token));
        if ALength < 2 then Continue;
        RecordCandidateOccurrence(Stream, AStreamIndex, AStart, ALength,
          AEncodedBytes, AHash);
      end;
    end;
  end;

  function CandidateLexicallyBefore(LeftIndex, RightIndex: Integer): Boolean;
  var
    Offset, LeftToken, RightToken: Integer;
  begin
    for Offset := 0 to Candidates[LeftIndex].Length - 1 do begin
      LeftToken := StreamToken(Candidates[LeftIndex].StreamIndex,
        Candidates[LeftIndex].StartOffset + Offset);
      RightToken := StreamToken(Candidates[RightIndex].StreamIndex,
        Candidates[RightIndex].StartOffset + Offset);
      if LeftToken <> RightToken then Exit(LeftToken < RightToken);
    end;
    Result := False;
  end;

  procedure CountRoutineReferences(const Stream: TEncodedPattern);
  var
    Token, CalledRoutine: Integer;
  begin
    for Token in Stream do begin
      if not InRange(Token, INTERNAL_CALL_BASE,
        INTERNAL_CALL_BASE + MAX_PATTERN_ROUTINES - 1) then Continue;
      CalledRoutine := Token - INTERNAL_CALL_BASE;
      if InRange(CalledRoutine, 0, Length(Active) - 1) and
        Active[CalledRoutine] then
        Inc(ReferenceCounts[CalledRoutine]);
    end;
  end;

  function MeasureRoutineDepth(ARoutineIndex: Integer): Integer;
  var
    Token, CalledRoutine, ChildDepth: Integer;
  begin
    if DepthVisitState[ARoutineIndex] = 2 then
      Exit(RoutineDepth[ARoutineIndex]);
    if DepthVisitState[ARoutineIndex] = 1 then
      raise Exception.Create('Recursive pattern phrase cycle');

    DepthVisitState[ARoutineIndex] := 1;
    Result := 1;
    for Token in Group^.Routines[ARoutineIndex] do begin
      if not InRange(Token, INTERNAL_CALL_BASE,
        INTERNAL_CALL_BASE + MAX_PATTERN_ROUTINES - 1) then Continue;
      CalledRoutine := Token - INTERNAL_CALL_BASE;
      if not InRange(CalledRoutine, 0, Length(Active) - 1) or
        not Active[CalledRoutine] then
        raise Exception.CreateFmt('Invalid pattern phrase call %d',
          [CalledRoutine]);
      ChildDepth := 1 + MeasureRoutineDepth(CalledRoutine);
      if ChildDepth > Result then Result := ChildDepth;
    end;
    RoutineDepth[ARoutineIndex] := Result;
    DepthVisitState[ARoutineIndex] := 2;
  end;

  procedure PropagateRoutinePrefix(ARoutineIndex, PrefixDepth: Integer);
  var
    Token, CalledRoutine, ChildPrefix: Integer;
  begin
    if PrefixDepth <= MaxPrefixDepth[ARoutineIndex] then Exit;
    MaxPrefixDepth[ARoutineIndex] := PrefixDepth;
    ChildPrefix := PrefixDepth + 1;
    for Token in Group^.Routines[ARoutineIndex] do begin
      if not InRange(Token, INTERNAL_CALL_BASE,
        INTERNAL_CALL_BASE + MAX_PATTERN_ROUTINES - 1) then Continue;
      CalledRoutine := Token - INTERNAL_CALL_BASE;
      if InRange(CalledRoutine, 0, Length(Active) - 1) and
        Active[CalledRoutine] then
        PropagateRoutinePrefix(CalledRoutine, ChildPrefix);
    end;
  end;

  procedure PropagatePatternPrefixes(const Stream: TEncodedPattern);
  var
    Token, CalledRoutine: Integer;
  begin
    for Token in Stream do begin
      if not InRange(Token, INTERNAL_CALL_BASE,
        INTERNAL_CALL_BASE + MAX_PATTERN_ROUTINES - 1) then Continue;
      CalledRoutine := Token - INTERNAL_CALL_BASE;
      if InRange(CalledRoutine, 0, Length(Active) - 1) and
        Active[CalledRoutine] then
        PropagateRoutinePrefix(CalledRoutine, 1);
    end;
  end;

begin
  Group := @Result;
  Result.Patterns := nil;
  Result.Routines := nil;

  for Channel := Low(TChannel) to High(TChannel) do begin
    if (ChannelMask and (1 shl Ord(Channel))) = 0 then Continue;
    for PatternIndex := 0 to Song.Patterns.Count - 1 do begin
      if not PatternIsUsedInChannel(Song.Patterns.Keys[PatternIndex], Channel,
        UsedStuff) then Continue;
      EntryIndex := Length(Result.Patterns);
      SetLength(Result.Patterns, EntryIndex + 1);
      Result.Patterns[EntryIndex].Channel := Channel;
      Result.Patterns[EntryIndex].PatternKey := Song.Patterns.Keys[PatternIndex];
      Result.Patterns[EntryIndex].Pattern := EncodePattern(
        Song.Patterns.Data[PatternIndex]^, Channel, Catalogs[Channel],
        UsedStuff);
    end;
  end;

  if not Optimize then begin
    Result.EncodedSize := 0;
    for EntryIndex := 0 to Length(Result.Patterns) - 1 do
      Inc(Result.EncodedSize,
        EncodedStreamSize(Result.Patterns[EntryIndex].Pattern));
    Exit;
  end;

  while Length(Result.Routines) < MAX_PATTERN_ROUTINES do begin
    InitializeCandidates;
    StreamIndex := 0;
    for EntryIndex := 0 to Length(Result.Patterns) - 1 do begin
      RecordCandidate(Result.Patterns[EntryIndex].Pattern, StreamIndex);
      Inc(StreamIndex);
    end;
    for RoutineIndex := 0 to Length(Result.Routines) - 1 do begin
      RecordCandidate(Result.Routines[RoutineIndex], StreamIndex);
      Inc(StreamIndex);
    end;

    BestCandidateIndex := -1;
    BestGain := 0;
    BestScore := 0;
    for CandidateIndex := 0 to CandidateCount - 1 do begin
      Stats := Candidates[CandidateIndex].Stats;
      CandidateLength := Candidates[CandidateIndex].Length;
      CandidateBytes := Candidates[CandidateIndex].EncodedBytes;

      Gain := Stats.Occurrences *
        (CandidateBytes - PATTERN_CALL_SIZE) -
        (CandidateBytes + PATTERN_RETURN_SIZE);
      if Gain <= 0 then Continue;
      // The small use-count bias consistently improves the corpus result.
      Score := Gain * 2 + Stats.Occurrences;
      if (Score > BestScore) or
        ((Score = BestScore) and
         ((BestCandidateIndex < 0) or
          (CandidateLength > Candidates[BestCandidateIndex].Length) or
          ((CandidateLength = Candidates[BestCandidateIndex].Length) and
           CandidateLexicallyBefore(CandidateIndex,
             BestCandidateIndex)))) then begin
        BestCandidateIndex := CandidateIndex;
        BestGain := Gain;
        BestScore := Score;
      end;
    end;

    BestCandidate := nil;
    if BestCandidateIndex >= 0 then begin
      SetLength(BestCandidate, Candidates[BestCandidateIndex].Length);
      for I := 0 to Length(BestCandidate) - 1 do
        BestCandidate[I] := StreamToken(
          Candidates[BestCandidateIndex].StreamIndex,
          Candidates[BestCandidateIndex].StartOffset + I);
    end;

    if (BestGain <= 0) or (Length(BestCandidate) = 0) then Break;

    Replacement := INTERNAL_CALL_BASE + Length(Result.Routines);
    for EntryIndex := 0 to Length(Result.Patterns) - 1 do
      ReplaceCandidate(Result.Patterns[EntryIndex].Pattern,
        BestCandidate, Replacement);
    for RoutineIndex := 0 to Length(Result.Routines) - 1 do
      ReplaceCandidate(Result.Routines[RoutineIndex],
        BestCandidate, Replacement);

    RoutineIndex := Length(Result.Routines);
    SetLength(Result.Routines, RoutineIndex + 1);
    SetLength(Result.Routines[RoutineIndex], Length(BestCandidate));
    for I := 0 to Length(BestCandidate) - 1 do
      Result.Routines[RoutineIndex][I] := BestCandidate[I];
  end;

  // Later routines can make an older routine unprofitable. Inline any such
  // routines before assigning the final, dense CALL ids.
  SetLength(Active, Length(Result.Routines));
  for RoutineIndex := 0 to Length(Active) - 1 do Active[RoutineIndex] := True;
  repeat
    SetLength(ReferenceCounts, Length(Result.Routines));
    for I := 0 to Length(ReferenceCounts) - 1 do ReferenceCounts[I] := 0;
    for EntryIndex := 0 to Length(Result.Patterns) - 1 do
      CountRoutineReferences(Result.Patterns[EntryIndex].Pattern);
    for RoutineIndex := 0 to Length(Result.Routines) - 1 do
      if Active[RoutineIndex] then
        CountRoutineReferences(Result.Routines[RoutineIndex]);

    BestRoutine := -1;
    BestSaving := 0;
    for RoutineIndex := 0 to Length(Result.Routines) - 1 do begin
      if not Active[RoutineIndex] then Continue;
      RoutineBytes := EncodedStreamSize(Result.Routines[RoutineIndex]);
      Saving := RoutineBytes + PATTERN_RETURN_SIZE -
        ReferenceCounts[RoutineIndex] *
          (RoutineBytes - PATTERN_CALL_SIZE);
      if Saving > BestSaving then begin
        BestSaving := Saving;
        BestRoutine := RoutineIndex;
      end;
    end;
    if BestRoutine >= 0 then begin
      for EntryIndex := 0 to Length(Result.Patterns) - 1 do
        InlineRoutine(Result.Patterns[EntryIndex].Pattern, BestRoutine,
          Result.Routines[BestRoutine]);
      for RoutineIndex := 0 to Length(Result.Routines) - 1 do
        if Active[RoutineIndex] and (RoutineIndex <> BestRoutine) then
          InlineRoutine(Result.Routines[RoutineIndex], BestRoutine,
            Result.Routines[BestRoutine]);
      Active[BestRoutine] := False;
    end;
  until BestRoutine < 0;

  // Phrase count and phrase nesting are independent. Keep all profitable
  // phrases, but inline the cheapest routines involved in an over-deep call
  // chain until the driver-side return stack has a small, fixed bound.
  repeat
    SetLength(RoutineDepth, Length(Result.Routines));
    SetLength(MaxPrefixDepth, Length(Result.Routines));
    SetLength(DepthVisitState, Length(Result.Routines));
    for I := 0 to Length(Result.Routines) - 1 do begin
      RoutineDepth[I] := 0;
      MaxPrefixDepth[I] := 0;
      DepthVisitState[I] := 0;
    end;
    for RoutineIndex := 0 to Length(Result.Routines) - 1 do
      if Active[RoutineIndex] then MeasureRoutineDepth(RoutineIndex);
    for EntryIndex := 0 to Length(Result.Patterns) - 1 do
      PropagatePatternPrefixes(Result.Patterns[EntryIndex].Pattern);

    SetLength(ReferenceCounts, Length(Result.Routines));
    for I := 0 to Length(ReferenceCounts) - 1 do ReferenceCounts[I] := 0;
    for EntryIndex := 0 to Length(Result.Patterns) - 1 do
      CountRoutineReferences(Result.Patterns[EntryIndex].Pattern);
    for RoutineIndex := 0 to Length(Result.Routines) - 1 do
      if Active[RoutineIndex] then
        CountRoutineReferences(Result.Routines[RoutineIndex]);

    DepthCandidate := -1;
    DepthCandidateCost := MaxInt;
    for RoutineIndex := 0 to Length(Result.Routines) - 1 do begin
      if not Active[RoutineIndex] or
        (MaxPrefixDepth[RoutineIndex] = 0) or
        (MaxPrefixDepth[RoutineIndex] + RoutineDepth[RoutineIndex] - 1 <=
          MAX_PATTERN_DEPTH) then Continue;
      RoutineBytes := EncodedStreamSize(Result.Routines[RoutineIndex]);
      InlineCost := ReferenceCounts[RoutineIndex] *
        (RoutineBytes - PATTERN_CALL_SIZE) -
        (RoutineBytes + PATTERN_RETURN_SIZE);
      if InlineCost < DepthCandidateCost then begin
        DepthCandidate := RoutineIndex;
        DepthCandidateCost := InlineCost;
      end;
    end;

    if DepthCandidate >= 0 then begin
      for EntryIndex := 0 to Length(Result.Patterns) - 1 do
        InlineRoutine(Result.Patterns[EntryIndex].Pattern, DepthCandidate,
          Result.Routines[DepthCandidate]);
      for RoutineIndex := 0 to Length(Result.Routines) - 1 do
        if Active[RoutineIndex] and (RoutineIndex <> DepthCandidate) then
          InlineRoutine(Result.Routines[RoutineIndex], DepthCandidate,
            Result.Routines[DepthCandidate]);
      Active[DepthCandidate] := False;
    end;
  until DepthCandidate < 0;

  SetLength(RoutineMap, Length(Result.Routines));
  NewRoutineIndex := 0;
  for RoutineIndex := 0 to Length(Result.Routines) - 1 do begin
    if Active[RoutineIndex] then begin
      RoutineMap[RoutineIndex] := NewRoutineIndex;
      Inc(NewRoutineIndex);
    end
    else
      RoutineMap[RoutineIndex] := -1;
  end;
  SetLength(CompactedRoutines, NewRoutineIndex);
  NewRoutineIndex := 0;
  for RoutineIndex := 0 to Length(Result.Routines) - 1 do
    if Active[RoutineIndex] then begin
      CompactedRoutines[NewRoutineIndex] := Result.Routines[RoutineIndex];
      Inc(NewRoutineIndex);
    end;

  for EntryIndex := 0 to Length(Result.Patterns) - 1 do
    for I := 0 to Length(Result.Patterns[EntryIndex].Pattern) - 1 do
      if InRange(Result.Patterns[EntryIndex].Pattern[I], INTERNAL_CALL_BASE,
        INTERNAL_CALL_BASE + MAX_PATTERN_ROUTINES - 1) then
        Result.Patterns[EntryIndex].Pattern[I] := INTERNAL_CALL_BASE +
          RoutineMap[Result.Patterns[EntryIndex].Pattern[I] -
            INTERNAL_CALL_BASE];
  for RoutineIndex := 0 to Length(CompactedRoutines) - 1 do
    for I := 0 to Length(CompactedRoutines[RoutineIndex]) - 1 do
      if InRange(CompactedRoutines[RoutineIndex][I], INTERNAL_CALL_BASE,
        INTERNAL_CALL_BASE + MAX_PATTERN_ROUTINES - 1) then
        CompactedRoutines[RoutineIndex][I] := INTERNAL_CALL_BASE +
          RoutineMap[CompactedRoutines[RoutineIndex][I] -
            INTERNAL_CALL_BASE];
  Result.Routines := CompactedRoutines;

  Result.EncodedSize := 0;
  for EntryIndex := 0 to Length(Result.Patterns) - 1 do
    Inc(Result.EncodedSize,
      EncodedStreamSize(Result.Patterns[EntryIndex].Pattern));
  DictionaryBytes := 0;
  for RoutineIndex := 0 to Length(Result.Routines) - 1 do
    Inc(DictionaryBytes, EncodedStreamSize(Result.Routines[RoutineIndex]) +
      PATTERN_RETURN_SIZE);
  Inc(Result.EncodedSize, DictionaryBytes);
end;

procedure AppendPatternEntry(var Patterns: TEncodedPatterns;
  const Entry: TEncodedPatternEntry);
var
  Index: Integer;
begin
  Index := Length(Patterns);
  SetLength(Patterns, Index + 1);
  Patterns[Index] := Entry;
end;

procedure ValidateChannelEncoding(const Song: TSong; Channel: TChannel;
  const Catalog: TNoteCatalog; const Encoding: TChannelEncoding;
  const Dictionary: TPatternDictionary; const UsedStuff: TUsedStuff);
var
  EntryIndex, PatternIndex, Row: Integer;
  OriginalPattern: PPattern;
  OnStack: array of Boolean;

  procedure CheckNoteRecord(Value: Integer);
  begin
    if Row > High(TPattern) then
      raise Exception.CreateFmt(
        'Pattern routine encoding overran channel %d pattern %d',
        [Ord(Channel) + 1, Encoding.Patterns[EntryIndex].PatternKey]);
    if Value <> CellToNoteRecord(OriginalPattern^[Row], Channel,
      UsedStuff) then
      raise Exception.CreateFmt(
        'Pattern routine encoding mismatch in channel %d pattern %d row %d',
        [Ord(Channel) + 1, Encoding.Patterns[EntryIndex].PatternKey, Row]);
    Inc(Row);
  end;

  procedure DecodeStream(const Stream: TEncodedPattern; Depth: Integer);
  var
    Token, RoutineIndex, NoteRecord: Integer;
  begin
    for Token in Stream do begin
      if Token < 0 then begin
        NoteRecord := -Token - 1;
        CheckNoteRecord(NoteRecord);
      end
      else if Token < NOTE_CATALOG_SIZE then begin
        if Token >= Length(Catalog) then
          raise Exception.CreateFmt(
            'Invalid catalog index in channel %d pattern %d',
            [Ord(Channel) + 1, Encoding.Patterns[EntryIndex].PatternKey]);
        CheckNoteRecord(Catalog[Token]);
      end
      else if InRange(Token, INTERNAL_CALL_BASE,
        INTERNAL_CALL_BASE + MAX_PATTERN_ROUTINES - 1) then begin
        RoutineIndex := Token - INTERNAL_CALL_BASE;
        if not InRange(RoutineIndex, 0, Length(Dictionary.Routines) - 1) then
          raise Exception.CreateFmt(
            'Invalid routine index in channel %d pattern %d',
            [Ord(Channel) + 1, Encoding.Patterns[EntryIndex].PatternKey]);
        if OnStack[RoutineIndex] then
          raise Exception.CreateFmt(
            'Recursive routine cycle in channel %d pattern %d',
            [Ord(Channel) + 1, Encoding.Patterns[EntryIndex].PatternKey]);
        if Depth >= MAX_PATTERN_DEPTH then
          raise Exception.CreateFmt(
            'Pattern routine depth exceeds %d in channel %d pattern %d',
            [MAX_PATTERN_DEPTH, Ord(Channel) + 1,
             Encoding.Patterns[EntryIndex].PatternKey]);
        OnStack[RoutineIndex] := True;
        DecodeStream(Dictionary.Routines[RoutineIndex], Depth + 1);
        OnStack[RoutineIndex] := False;
      end
      else
        raise Exception.CreateFmt(
          'Invalid pattern token %d in channel %d pattern %d',
          [Token, Ord(Channel) + 1,
           Encoding.Patterns[EntryIndex].PatternKey]);
    end;
  end;

begin
  SetLength(OnStack, Length(Dictionary.Routines));
  for EntryIndex := 0 to Length(Encoding.Patterns) - 1 do begin
    PatternIndex := Song.Patterns.IndexOf(
      Encoding.Patterns[EntryIndex].PatternKey);
    if PatternIndex = -1 then
      raise Exception.CreateFmt('Missing encoded pattern %d',
        [Encoding.Patterns[EntryIndex].PatternKey]);
    OriginalPattern := Song.Patterns.Data[PatternIndex];
    Row := Low(TPattern);
    DecodeStream(Encoding.Patterns[EntryIndex].Pattern, 0);
    if Row <> Length(TPattern) then
      raise Exception.CreateFmt(
        'Pattern routine encoding ended early in channel %d pattern %d',
        [Ord(Channel) + 1, Encoding.Patterns[EntryIndex].PatternKey]);
  end;
end;

function BuildSongEncoding(const Song: TSong; const UsedStuff: TUsedStuff;
  const Catalogs: TNoteCatalogs; Optimize: Boolean): TSongEncoding;
const
  ALL_CHANNELS_MASK = (1 shl 4) - 1;
var
  Groups: array[1..ALL_CHANNELS_MASK] of TGroupEncoding;
  BestCost, BestGroup: array[0..ALL_CHANNELS_MASK] of Integer;
  Mask, SubMask, FirstBit, Cost, ChosenMask, DictionaryIndex: Integer;
  EntryIndex: Integer;
  Channel: TChannel;
begin
  Result.Dictionaries := nil;
  for Channel := Low(TChannel) to High(TChannel) do begin
    Result.Channels[Channel].Patterns := nil;
    Result.Channels[Channel].DictionaryIndex := -1;
  end;

  if not Optimize then begin
    Groups[ALL_CHANNELS_MASK] := BuildGroupEncoding(Song,
      ALL_CHANNELS_MASK, UsedStuff, Catalogs, False);
    SetLength(Result.Dictionaries, 1);
    Result.Dictionaries[0].Routines := nil;
    Result.Dictionaries[0].ChannelMask := ALL_CHANNELS_MASK;
    for Channel := Low(TChannel) to High(TChannel) do
      Result.Channels[Channel].DictionaryIndex := 0;
    for EntryIndex := 0 to
      Length(Groups[ALL_CHANNELS_MASK].Patterns) - 1 do
      AppendPatternEntry(
        Result.Channels[
          Groups[ALL_CHANNELS_MASK].Patterns[EntryIndex].Channel].Patterns,
        Groups[ALL_CHANNELS_MASK].Patterns[EntryIndex]);
    for Channel := Low(TChannel) to High(TChannel) do
      ValidateChannelEncoding(Song, Channel, Catalogs[Channel],
        Result.Channels[Channel], Result.Dictionaries[0], UsedStuff);
    Exit;
  end;

  for Mask := 1 to ALL_CHANNELS_MASK do
    Groups[Mask] := BuildGroupEncoding(Song, Mask, UsedStuff, Catalogs, True);

  BestCost[0] := 0;
  BestGroup[0] := 0;
  for Mask := 1 to ALL_CHANNELS_MASK do begin
    BestCost[Mask] := MaxInt;
    BestGroup[Mask] := 0;
    FirstBit := 1;
    while (Mask and FirstBit) = 0 do FirstBit := FirstBit shl 1;
    SubMask := Mask;
    while SubMask > 0 do begin
      if (SubMask and FirstBit) <> 0 then begin
        Cost := Groups[SubMask].EncodedSize + BestCost[Mask xor SubMask];
        if Cost < BestCost[Mask] then begin
          BestCost[Mask] := Cost;
          BestGroup[Mask] := SubMask;
        end;
      end;
      SubMask := (SubMask - 1) and Mask;
    end;
  end;

  Mask := ALL_CHANNELS_MASK;
  while Mask <> 0 do begin
    ChosenMask := BestGroup[Mask];
    DictionaryIndex := Length(Result.Dictionaries);
    SetLength(Result.Dictionaries, DictionaryIndex + 1);
    Result.Dictionaries[DictionaryIndex].Routines :=
      Groups[ChosenMask].Routines;
    Result.Dictionaries[DictionaryIndex].ChannelMask := ChosenMask;
    for Channel := Low(TChannel) to High(TChannel) do
      if (ChosenMask and (1 shl Ord(Channel))) <> 0 then
        Result.Channels[Channel].DictionaryIndex := DictionaryIndex;
    for EntryIndex := 0 to Length(Groups[ChosenMask].Patterns) - 1 do
      AppendPatternEntry(
        Result.Channels[Groups[ChosenMask].Patterns[EntryIndex].Channel].Patterns,
        Groups[ChosenMask].Patterns[EntryIndex]);
    Mask := Mask xor ChosenMask;
  end;

  for Channel := Low(TChannel) to High(TChannel) do begin
    DictionaryIndex := Result.Channels[Channel].DictionaryIndex;
    if not InRange(DictionaryIndex, 0, Length(Result.Dictionaries) - 1) then
      raise Exception.CreateFmt('Missing pattern dictionary for channel %d',
        [Ord(Channel) + 1]);
    ValidateChannelEncoding(Song, Channel, Catalogs[Channel],
      Result.Channels[Channel], Result.Dictionaries[DictionaryIndex],
      UsedStuff);
  end;
end;

function ChannelPatternName(Channel: TChannel; PatternKey: Integer): String;
begin
  Result := Format('P%d_%d', [Ord(Channel) + 1, PatternKey]);
end;

function PatternDictionaryName(DictionaryIndex: Integer): String;
begin
  Result := Format('pattern_dictionary%d', [DictionaryIndex + 1]);
end;

function PatternRoutineName(const DictionaryName: String;
  RoutineIndex: Integer): String;
begin
  Result := Format('%s_routine_%d', [DictionaryName, RoutineIndex]);
end;

function IsCallToken(Token: Integer): Boolean;
begin
  Result := InRange(Token, INTERNAL_CALL_BASE,
    INTERNAL_CALL_BASE + MAX_PATTERN_ROUTINES - 1);
end;

function CalledRoutineIndex(Token: Integer): Integer;
begin
  Result := Token - INTERNAL_CALL_BASE;
end;

function ChannelPatternDictionaryName(const Encoding: TSongEncoding;
  Channel: TChannel): String;
begin
  Result := PatternDictionaryName(
    Encoding.Channels[Channel].DictionaryIndex);
end;

function NoteRecordNoteName(NoteRecord: Integer; CStyle: Boolean): String;
var
  Note: Integer;
begin
  Note := NoteRecordByte(NoteRecord, 0) and $7F;
  if Note = LAST_NOTE then Exit('LAST_NOTE');

  if CStyle then begin
    if NoteToCMap.IndexOf(Note) <> -1 then
      Exit(NoteToCMap.KeyData[Note]);
  end
  else if NoteToDriverMap.IndexOf(Note) <> -1 then
    Exit(NoteToDriverMap.KeyData[Note]);

  Result := IntToStr(Note);
end;

function NoteRecordDNArgs(NoteRecord: Integer; HexPrefix: String;
  CStyle: Boolean): String;
var
  B1, B2, B3: Byte;
  Instrument, Effect: Integer;
begin
  B1 := NoteRecordByte(NoteRecord, 0);
  B2 := NoteRecordByte(NoteRecord, 1);
  B3 := NoteRecordByte(NoteRecord, 2);
  Instrument := (B2 shr 4) or ((B1 and $80) shr 3);
  Effect := ((B2 and $0F) shl 8) or B3;
  Result := Format('%s,%d,%s%s',
    [NoteRecordNoteName(NoteRecord, CStyle), Instrument, HexPrefix,
     HexStr(Effect, 3)]);
end;

function RenderGBDKPatternMacros: String;
begin
  Result := Format(
    '#define dn_literal(NOTE, INSTRUMENT, EFFECT) '+
      '(unsigned char)(%d + (NOTE)), '+
      '(unsigned char)((((INSTRUMENT) << 4) & 0xFF) | ((EFFECT) >> 8)), '+
      '(unsigned char)((EFFECT) & 0xFF)' + LineEnding +
    'typedef struct {' + LineEnding +
    '    unsigned char opcode;' + LineEnding +
    '    const unsigned char *target;' + LineEnding +
    '} hUGEPatternCallCommand_t;' + LineEnding +
    '#define pattern_call(TARGET) {%d, (const unsigned char *)(TARGET)}' +
      LineEnding +
    '#define pattern_return (unsigned char)%d',
    [NOTE_LITERAL_BASE, PATTERN_CALL, PATTERN_RETURN]);
end;

function RenderRGBDSPatternMacros: String;
begin
  Result := Format(
    'MACRO dn_literal' + LineEnding +
    ' db %d + \1' + LineEnding +
    ' db (((\2 << 4) & $FF) | (\3 >> 8))' + LineEnding +
    ' db LOW(\3)' + LineEnding +
    'ENDM' + LineEnding + LineEnding +
    'MACRO pattern_call' + LineEnding +
    ' db %d' + LineEnding +
    ' dw \1' + LineEnding +
    'ENDM' + LineEnding + LineEnding +
    'MACRO pattern_return' + LineEnding +
    ' db %d' + LineEnding +
    'ENDM', [NOTE_LITERAL_BASE, PATTERN_CALL, PATTERN_RETURN]);
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
    if Length(Catalog) = 0 then
      SL.Add('    0,')
    else
      for I := 0 to Length(Catalog) - 1 do
        SL.Add('    DN(' + NoteRecordDNArgs(Catalog[I], '0x', True) + '),');
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
      SL.Add(' dn ' + NoteRecordDNArgs(Catalog[I], '$', False));
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

procedure AddGBDKStreamType(SL: TStringList; const Name: String;
  const Encoded: TEncodedPattern; Terminated: Boolean);
var
  I, J, FieldIndex: Integer;
begin
  SL.Add('struct ' + Name + '_type {');
  FieldIndex := 0;
  for I := 0 to Length(Encoded) - 1 do begin
    if IsCallToken(Encoded[I]) then
      SL.Add(Format('    hUGEPatternCallCommand_t call_%d;', [FieldIndex]))
    else if Encoded[I] < 0 then
      for J := 0 to 2 do begin
        SL.Add(Format('    unsigned char byte_%d;', [FieldIndex]));
        Inc(FieldIndex);
      end
    else
      SL.Add(Format('    unsigned char byte_%d;', [FieldIndex]));
    Inc(FieldIndex);
  end;
  if Terminated then
    SL.Add(Format('    unsigned char byte_%d;', [FieldIndex]));
  SL.Add('};');
end;

procedure AddGBDKStreamDefinition(SL: TStringList; const Name: String;
  const Encoded: TEncodedPattern; const DictionaryName: String;
  Terminated: Boolean);
var
  I, NoteRecord: Integer;
begin
  SL.Add('static const struct ' + Name + '_type ' + Name + ' = {');
  for I := 0 to Length(Encoded) - 1 do begin
    if Encoded[I] < 0 then begin
      NoteRecord := -Encoded[I] - 1;
      SL.Add('    dn_literal(' +
        NoteRecordDNArgs(NoteRecord, '0x', True) + '),');
    end
    else if Encoded[I] < NOTE_CATALOG_SIZE then
      SL.Add('    ' + IntToStr(Encoded[I]) + ',')
    else
      SL.Add(Format('    pattern_call(&%s),',
        [PatternRoutineName(DictionaryName,
          CalledRoutineIndex(Encoded[I]))]));
  end;
  if Terminated then SL.Add('    pattern_return,');
  SL.Add('};');
end;

function RenderGBDKPatternDictionary(Name: String;
  const Routines: TRoutineDictionary): String;
var
  I: Integer;
  SL: TStringList;
  RoutineName: String;
  VisitState: array of Byte;
  RenderOrder: array of Integer;

  procedure VisitRoutine(RoutineIndex: Integer);
  var
    Token, CalledIndex, OrderIndex: Integer;
  begin
    if VisitState[RoutineIndex] = 2 then Exit;
    if VisitState[RoutineIndex] = 1 then
      raise Exception.CreateFmt('Recursive pattern phrase cycle at %s',
        [PatternRoutineName(Name, RoutineIndex)]);
    VisitState[RoutineIndex] := 1;
    for Token in Routines[RoutineIndex] do
      if IsCallToken(Token) then begin
        CalledIndex := CalledRoutineIndex(Token);
        if not InRange(CalledIndex, 0, Length(Routines) - 1) then
          raise Exception.CreateFmt('Invalid phrase call %d in %s',
            [CalledIndex, PatternRoutineName(Name, RoutineIndex)]);
        VisitRoutine(CalledIndex);
      end;
    VisitState[RoutineIndex] := 2;
    OrderIndex := Length(RenderOrder);
    SetLength(RenderOrder, OrderIndex + 1);
    RenderOrder[OrderIndex] := RoutineIndex;
  end;
begin
  SL := TStringList.Create;
  try
    SL.Add('/* Direct-address, RETURN-terminated pattern phrases. */');
    for I := 0 to Length(Routines) - 1 do begin
      RoutineName := PatternRoutineName(Name, I);
      AddGBDKStreamType(SL, RoutineName, Routines[I], True);
    end;
    SetLength(VisitState, Length(Routines));
    RenderOrder := nil;
    for I := 0 to Length(Routines) - 1 do VisitRoutine(I);
    for I := 0 to Length(RenderOrder) - 1 do begin
      RoutineName := PatternRoutineName(Name, RenderOrder[I]);
      AddGBDKStreamDefinition(SL, RoutineName,
        Routines[RenderOrder[I]], Name, True);
    end;
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function RenderRGBDSPatternDictionary(Name: String;
  const Routines: TRoutineDictionary): String;
var
  I, J, Token, NoteRecord: Integer;
  SL: TStringList;
  RoutineName: String;
begin
  SL := TStringList.Create;
  try
    SL.Add('; Direct-address, RETURN-terminated pattern phrases.');
    for I := 0 to Length(Routines) - 1 do begin
      RoutineName := PatternRoutineName(Name, I);
      SL.Add(RoutineName + ':');
      for J := 0 to Length(Routines[I]) - 1 do begin
        Token := Routines[I][J];
        if Token < 0 then begin
          NoteRecord := -Token - 1;
          SL.Add(' dn_literal ' +
            NoteRecordDNArgs(NoteRecord, '$', False));
        end
        else if Token < NOTE_CATALOG_SIZE then
          SL.Add(' db ' + IntToStr(Token))
        else
          SL.Add(' pattern_call ' + PatternRoutineName(Name,
            CalledRoutineIndex(Token)));
      end;
      SL.Add(' pattern_return');
    end;
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function RenderGBDKCompressedPattern(Name: String;
  const Encoded: TEncodedPattern; const DictionaryName: String): String;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    AddGBDKStreamType(SL, Name, Encoded, False);
    AddGBDKStreamDefinition(SL, Name, Encoded, DictionaryName, False);
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function RenderRGBDSCompressedPattern(Name: String;
  const Encoded: TEncodedPattern; const DictionaryName: String): String;
var
  I, NoteRecord: Integer;
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Add(Name + ':');
    for I := 0 to Length(Encoded) - 1 do begin
      if Encoded[I] < 0 then begin
        NoteRecord := -Encoded[I] - 1;
        SL.Add(' dn_literal ' +
          NoteRecordDNArgs(NoteRecord, '$', False));
      end
      else if Encoded[I] < NOTE_CATALOG_SIZE then
        SL.Add(' db ' + IntToStr(Encoded[I]))
      else
        SL.Add(' pattern_call ' + PatternRoutineName(DictionaryName,
          CalledRoutineIndex(Encoded[I])));
    end;
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

procedure RenderSongToGBDKC(Song: TSong; DescriptorName: String; Filename: string; Bank: Integer = -1);
  function RenderGBDKSubpatternCell(Cell: TCell; Last: Boolean;
    InstrumentType: TInstrumentType;
    const AUsedStuff: TUsedStuff): string;
  var
    SL: TStringList;
    EffectParams: TEffectParams;
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

    EffectParams := Cell.EffectParams;
    if (InstrumentType = itWave) and (Cell.EffectCode = $9) then
      EffectParams.Value := RemapWaveform(EffectParams.Value, AUsedStuff);
    SL.Add('0x' + EffectCodeToStr(Cell.EffectCode, EffectParams));

    Result := SL.DelimitedText;
    SL.Free;
  end;

  function RenderGBDKSubpattern(Name: string; Pat: TPattern;
    InstrumentType: TInstrumentType;
    const AUsedStuff: TUsedStuff): string;
  var
    I: Integer;
  begin
    Result := 'static const unsigned char ' + Name + '[] = {' + LineEnding;
    for I := 0 to 31 do
      Result += '    DN(' + RenderGBDKSubpatternCell(Pat[I], I = 31,
        InstrumentType, AUsedStuff) + '),' + LineEnding;
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
      SL.Add('(const unsigned char *)&' +
        ChannelPatternName(Channel, Order[I]));

    Result := 'static const unsigned char* const order' +
      IntToStr(Ord(Channel) + 1) + '[] = {';
    Result += SL.DelimitedText;
    Result += '};';
    SL.Free;
  end;

  function RenderGBDKInstrument(Instrument: TInstrument; Num: Integer;
    const AUsedStuff: TUsedStuff): string;
  var
    SL: TStringList;
    AsmInstrument: TAsmInstrument;
    J: integer;
    HighMask: byte;
    TypePrefix: String;
  begin
    if Instrument.Type_ = itWave then
      Instrument.Waveform := RemapWaveform(Instrument.Waveform, AUsedStuff);
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

  function RenderGBDKInstrumentBank(Name: string;
    InstrumentBank: TInstrumentBank;
    const AUsedStuff: TUsedStuff): string;
  var
    I, NewIndex: integer;
    InstrType: String;
    InstrumentType: TInstrumentType;
  begin
    InstrumentType := InstrumentBank[1].Type_;
    case InstrumentType of
      itSquare: InstrType := 'hUGEDutyInstr_t';
      itWave: InstrType := 'hUGEWaveInstr_t';
      itNoise: InstrType := 'hUGENoiseInstr_t';
    end;

    if AUsedStuff.InstrumentCount[InstrumentType] = 0 then
      Exit('static const ' + InstrType + '* ' + Name + ' = NULL;'+LineEnding);

    Result := 'static const ' + InstrType + ' ' + Name + '[] = {'+LineEnding;
    for I := Low(InstrumentBank) to High(InstrumentBank) do
      if InstrumentIsUsed(I, InstrumentType, AUsedStuff) then begin
        NewIndex := AUsedStuff.InstrumentMap[InstrumentType, I];
        Result += '    '+RenderGBDKInstrument(InstrumentBank[I], NewIndex,
          AUsedStuff) + ','+LineEnding;
      end;
    Result += '};';
  end;

  function RenderGBDKWaves(Waves: TWaveBank;
    const AUsedStuff: TUsedStuff): string;
  var
    I, J: integer;
  begin
    if AUsedStuff.WaveformCount = 0 then
      Exit('static const unsigned char* waves = NULL;'+LineEnding);

    Result := 'static const unsigned char waves[] = {'+LineEnding;
    for I := Low(Waves) to High(Waves) do begin
      if AUsedStuff.WaveformMap[I] < 0 then Continue;
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
  Encoding: TSongEncoding;
  OutSL: TStringList;
  I, EntryIndex: integer;
  Channel: TChannel;
  F: Text;
  TypePrefix: String;
  UsedStuff: TUsedStuff;
begin
  OrderMatrix := BuildOrderMatrix(Song, True);
  UsedStuff := FindUsedStuff(Song, OrderMatrix);
  NoteCatalogs := BuildNoteCatalogs(Song, UsedStuff);
  Encoding := BuildSongEncoding(Song, UsedStuff, NoteCatalogs, True);

  OutSL := TStringList.Create;

  if Bank <> -1 then begin
    OutSL.Add('#pragma bank '+IntToStr(Bank));
    OutSL.Add('');
  end;

  OutSL.Add('#include "hUGEDriver.h"');
  OutSL.Add('#include <stddef.h>');
  OutSL.Add('');
  OutSL.Add(RenderGBDKPatternMacros);
  OutSL.Add('');

  for Channel := Low(TChannel) to High(TChannel) do begin
    OutSL.Add(RenderGBDKNoteCatalog('note_catalog' +
      IntToStr(Ord(Channel) + 1), NoteCatalogs[Channel]));
    OutSL.Add('');
  end;

  for I := 0 to Length(Encoding.Dictionaries) - 1 do begin
    OutSL.Add(RenderGBDKPatternDictionary(PatternDictionaryName(I),
      Encoding.Dictionaries[I].Routines));
    OutSL.Add('');
  end;

  for Channel := Low(TChannel) to High(TChannel) do
    for EntryIndex := 0 to Length(Encoding.Channels[Channel].Patterns) - 1 do
      OutSL.Add(RenderGBDKCompressedPattern(
        ChannelPatternName(Channel,
          Encoding.Channels[Channel].Patterns[EntryIndex].PatternKey),
        Encoding.Channels[Channel].Patterns[EntryIndex].Pattern,
        ChannelPatternDictionaryName(Encoding, Channel)));
  OutSL.Add('');

  for I := Low(Song.Instruments.All) to High(Song.Instruments.All) do
    if InstrumentIsUsed(ModInst(I), Song.Instruments.All[I].Type_, UsedStuff) then
      with Song.Instruments.All[I] do begin
        if SubpatternEnabled then begin
          WriteStr(TypePrefix, Type_);
          OutSL.Add(RenderGBDKSubpattern(TypePrefix+'SP' +
            IntToStr(UsedStuff.InstrumentMap[Type_, ModInst(I)]),
            Subpattern, Type_, UsedStuff));
        end;
      end;

  for Channel := Low(TChannel) to High(TChannel) do
    OutSL.Add(RenderGBDKOrder(Channel, OrderMatrix[Ord(Channel)]));
  OutSL.Add('');

  OutSL.Add(RenderGBDKInstrumentBank('duty_instruments',
    Song.Instruments.Duty, UsedStuff));
  OutSL.Add(RenderGBDKInstrumentBank('wave_instruments',
    Song.Instruments.Wave, UsedStuff));
  OutSL.Add(RenderGBDKInstrumentBank('noise_instruments',
    Song.Instruments.Noise, UsedStuff));
  OutSL.Add('');

  OutSL.Add(RenderGBDKWaves(Song.Waves, UsedStuff));
  OutSL.Add('');

  if Bank <> -1 then
    OutSL.Add(Format('const void __at(%d) __bank_%s;', [Bank, DescriptorName]));

  OutSL.Add(Format(
    'const hUGESong_t %s = {%d, %d, %d, %d, %d, order1, order2, order3,'+
    'order4, note_catalog1, note_catalog2, note_catalog3, note_catalog4,'+
    ' duty_instruments, wave_instruments, noise_instruments, NULL, waves};',
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

function RenderOrderTable(OrderMatrix: TOrderMatrix): string;
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

  Res.Add('order1: dw ' + ArrayHelper(chDuty1, OrderMatrix[0]));
  Res.Add('order2: dw ' + ArrayHelper(chDuty2, OrderMatrix[1]));
  Res.Add('order3: dw ' + ArrayHelper(chWave, OrderMatrix[2]));
  Res.Add('order4: dw ' + ArrayHelper(chNoise, OrderMatrix[3]));

  Result := Res.Text;
  Res.Free;
end;

function RenderInstruments(Instruments: TInstrumentBank;
  const UsedStuff: TUsedStuff): string;
var
  ResultSL: TStringList;
  AsmInstrument: TAsmInstrument;
  I, J, NewIndex: integer;
  Instrument, ExportInstrument: TInstrument;
  InstrumentType: TInstrumentType;
  TypePrefix: string;
  HighMask: byte;
begin
  ResultSL := TStringList.Create;
  InstrumentType := Instruments[1].Type_;

  for I := Low(Instruments) to High(Instruments) do
  begin
    if not InstrumentIsUsed(I, InstrumentType, UsedStuff) then Continue;
    Instrument := Instruments[I];
    ExportInstrument := Instrument;
    if InstrumentType = itWave then
      ExportInstrument.Waveform := RemapWaveform(Instrument.Waveform,
        UsedStuff);
    NewIndex := UsedStuff.InstrumentMap[InstrumentType, I];
    WriteStr(TypePrefix, InstrumentType);
    ResultSL.Add(Format('%s%s:',
      [TypePrefix, 'inst' + IntToStr(NewIndex)]));

    AsmInstrument := InstrumentToBytes(ExportInstrument);

    if InstrumentType = itNoise then
    begin
      ResultSL.Add('db '+IntToStr(AsmInstrument[1])); // envelope

      HighMask := AsmInstrument[0];
      if Instrument.LengthEnabled then
        HighMask := HighMask or %01000000;
      if Instrument.CounterStep = swSeven then
        HighMask := HighMask or %10000000;
      ResultSL.Add('db '+IntToStr(HighMask));
    end
    else begin
      for J := Low(AsmInstrument) to High(AsmInstrument) do
        ResultSL.Add('db '+IntToStr(AsmInstrument[J]));
    end;

    if Instrument.SubpatternEnabled then
      ResultSL.Insert(ResultSL.Count-1,
        Format('dw %sSP%d', [TypePrefix, NewIndex]))
    else
      ResultSL.Insert(ResultSL.Count-1, 'dw 0');

    if InstrumentType = itNoise then
      ResultSL.Add('ds 2');

    ResultSL.Add('');
  end;

  Result := ResultSL.Text;
  ResultSL.Free;
end;

function RenderSubpatternCell(Cell: TCell; Last: Boolean;
  InstrumentType: TInstrumentType; const UsedStuff: TUsedStuff): string;
var
  SL: TStringList;
  EffectParams: TEffectParams;
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

  EffectParams := Cell.EffectParams;
  if (InstrumentType = itWave) and (Cell.EffectCode = $9) then
    EffectParams.Value := RemapWaveform(EffectParams.Value, UsedStuff);
  SL.Add('$' + EffectCodeToStr(Cell.EffectCode, EffectParams));

  // RGBDS thinks you're defining a new macro if you don't have a space first.
  Result := ' dn ' + SL.DelimitedText;
  SL.Free;
end;

function RenderSubpattern(Name: string; Pattern: TPattern;
  InstrumentType: TInstrumentType; const UsedStuff: TUsedStuff): string;
var
  SL: TStringList;
  I: integer;
begin
  SL := TStringList.Create;
  SL.Add(Name + ':');

  for I := 0 to 31 do
    SL.Add(RenderSubpatternCell(Pattern[I], I = 31, InstrumentType,
      UsedStuff)); // TODO: hardcoded value

  Result := SL.Text;
  SL.Free;
end;

function RenderWaveforms(Waves: TWaveBank;
  const UsedStuff: TUsedStuff): string;
var
  SL, ResultSL: TStringList;
  I, J, NewIndex: integer;
begin
  ResultSL := TStringList.Create;

  for I := Low(Waves) to High(Waves) do
  begin
    NewIndex := UsedStuff.WaveformMap[I];
    if NewIndex < 0 then Continue;
    SL := TStringList.Create;
    SL.StrictDelimiter := True;
    SL.Delimiter := ',';

    J := Low(Waves[I]);
    while J < High(Waves[I]) do
    begin
      SL.Add(IntToStr((Waves[I, J] shl 4) or Waves[I, J + 1]));
      Inc(J, 2);
    end;
    ResultSL.Add(Format('wave%d: db %s', [NewIndex, SL.DelimitedText]));
    SL.Free;
  end;

  Result := ResultSL.Text;
  ResultSL.Free;
end;

procedure RenderSongToRGBDSAsm(Song: TSong; DescriptorName: String; Filename: string);
var
  OrderMatrix: TOrderMatrix;
  NoteCatalogs: TNoteCatalogs;
  Encoding: TSongEncoding;
  OutSL: TStringList;
  F: Text;
  I, EntryIndex: Integer;
  Channel: TChannel;
  TypePrefix: String;
  UsedStuff: TUsedStuff;
begin
  OrderMatrix := BuildOrderMatrix(Song, True);
  UsedStuff := FindUsedStuff(Song, OrderMatrix);
  NoteCatalogs := BuildNoteCatalogs(Song, UsedStuff);
  Encoding := BuildSongEncoding(Song, UsedStuff, NoteCatalogs, True);

  OutSL := TStringList.Create;

  OutSL.Add('include "hUGE.inc"');
  OutSL.Add('');
  OutSL.Add(RenderRGBDSPatternMacros);
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
  OutSL.Add('dw note_catalog1, note_catalog2, note_catalog3, note_catalog4');
  OutSL.Add('dw duty_instruments, wave_instruments, noise_instruments');
  OutSL.Add('dw routines');
  OutSL.Add('dw waves');
  OutSL.Add('');

  // Render order matrix
  OutSL.Add(RenderOrderTable(OrderMatrix));

  // Render channel note catalogs
  for Channel := Low(TChannel) to High(TChannel) do begin
    OutSL.Add(RenderRGBDSNoteCatalog('note_catalog' +
      IntToStr(Ord(Channel) + 1), NoteCatalogs[Channel]));
    OutSL.Add('');
  end;

  for I := 0 to Length(Encoding.Dictionaries) - 1 do begin
    OutSL.Add(RenderRGBDSPatternDictionary(PatternDictionaryName(I),
      Encoding.Dictionaries[I].Routines));
    OutSL.Add('');
  end;

  // Render instruments
  OutSL.Add('duty_instruments:');
  OutSL.Add(RenderInstruments(Song.Instruments.Duty, UsedStuff));
  OutSL.Add('');

  OutSL.Add('wave_instruments:');
  OutSL.Add(RenderInstruments(Song.Instruments.Wave, UsedStuff));
  OutSL.Add('');

  OutSL.Add('noise_instruments:');
  OutSL.Add(RenderInstruments(Song.Instruments.Noise, UsedStuff));
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
  OutSL.Add(RenderWaveforms(Song.Waves, UsedStuff));

  // Render channel-specific compressed patterns
  for Channel := Low(TChannel) to High(TChannel) do
    for EntryIndex := 0 to Length(Encoding.Channels[Channel].Patterns) - 1 do
      OutSL.Add(RenderRGBDSCompressedPattern(
        ChannelPatternName(Channel,
          Encoding.Channels[Channel].Patterns[EntryIndex].PatternKey),
        Encoding.Channels[Channel].Patterns[EntryIndex].Pattern,
        ChannelPatternDictionaryName(Encoding, Channel)));

  // Render subpatterns
  for I := Low(Song.Instruments.All) to High(Song.Instruments.All) do
    if InstrumentIsUsed(ModInst(I), Song.Instruments.All[I].Type_, UsedStuff) then
      with Song.Instruments.All[I] do begin
        if SubpatternEnabled then begin
          WriteStr(TypePrefix, Type_);
          OutSL.Add(RenderSubpattern(TypePrefix+'SP' +
            IntToStr(UsedStuff.InstrumentMap[Type_, ModInst(I)]),
            Subpattern, Type_, UsedStuff));
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

function ReadSongSizeFromMap(const MapFile: String): Integer;
const
  SONG_SECTION_MARKER = '["Song Data"]';
var
  Lines: TStringList;
  Line: String;
  SizeStart, SizeEnd: Integer;
begin
  Result := -1;
  if not FileExists(MapFile) then Exit;

  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(MapFile);
    for Line in Lines do begin
      if Pos(SONG_SECTION_MARKER, Line) = 0 then Continue;

      SizeStart := Pos('($', Line);
      if SizeStart = 0 then Continue;
      Inc(SizeStart, 2);
      SizeEnd := PosEx(' ', Line, SizeStart);
      if SizeEnd = 0 then Continue;

      if TryStrToInt('$' + Copy(Line, SizeStart, SizeEnd - SizeStart),
        Result) then Exit;
    end;
  finally
    Lines.Free;
  end;
end;

procedure AssembleSong(Song: TSong; Filename: string; Mode: TExportMode);
var
  OrderMatrix: TOrderMatrix;
  NoteCatalogs: TNoteCatalogs;
  Encoding: TSongEncoding;
  OutFile: Text;
  I, EntryIndex: integer;
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
  LastGeneratedSongSize := -1;
  OrderMatrix := BuildOrderMatrix(Song, True);
  UsedStuff := FindUsedStuff(Song, OrderMatrix);
  NoteCatalogs := BuildNoteCatalogs(Song, UsedStuff);
  Encoding := BuildSongEncoding(Song, UsedStuff, NoteCatalogs,
    Mode <> emPreview);

  if not DirectoryExists(ConcatPaths([CacheDir, 'render'])) then
    CreateDir(ConcatPaths([CacheDir, 'render']));

  FilePath := Filename;
  Filename := ConcatPaths([CacheDir, 'render', ExtractFileNameWithoutExt(ExtractFileNameOnly(Filename))]);

  WriteHTT(ConcatPaths([CacheDir, 'render', 'wave.htt']),
    RenderWaveforms(Song.Waves, UsedStuff));
  WriteHTT(ConcatPaths([CacheDir, 'render', 'order.htt']),
    RenderOrderTable(OrderMatrix));
  WriteHTT(ConcatPaths([CacheDir, 'render', 'duty_instrument.htt']),
    RenderInstruments(Song.Instruments.Duty, UsedStuff));
  WriteHTT(ConcatPaths([CacheDir, 'render', 'wave_instrument.htt']),
    RenderInstruments(Song.Instruments.Wave, UsedStuff));
  WriteHTT(ConcatPaths([CacheDir, 'render', 'noise_instrument.htt']),
    RenderInstruments(Song.Instruments.Noise, UsedStuff));
  for I := Low(TRoutineBank) to High(TRoutineBank) do
    WriteHTT(ConcatPaths([CacheDir, 'render', 'routine'+IntToStr(I)+'.htt']), Song.Routines[I]);

  AssignFile(OutFile, ConcatPaths([CacheDir, 'render', 'pattern.htt']));
  Rewrite(OutFile);

  Write(OutFile, RenderRGBDSPatternMacros);
  WriteLn(OutFile);
  WriteLn(OutFile);

  for Channel := Low(TChannel) to High(TChannel) do begin
    Write(OutFile, RenderRGBDSNoteCatalog('note_catalog' +
      IntToStr(Ord(Channel) + 1), NoteCatalogs[Channel]));
    WriteLn(OutFile);
  end;

  for I := 0 to Length(Encoding.Dictionaries) - 1 do begin
    Write(OutFile, RenderRGBDSPatternDictionary(PatternDictionaryName(I),
      Encoding.Dictionaries[I].Routines));
    WriteLn(OutFile);
  end;

  for Channel := Low(TChannel) to High(TChannel) do
    for EntryIndex := 0 to Length(Encoding.Channels[Channel].Patterns) - 1 do
      Write(OutFile, RenderRGBDSCompressedPattern(
        ChannelPatternName(Channel,
          Encoding.Channels[Channel].Patterns[EntryIndex].PatternKey),
        Encoding.Channels[Channel].Patterns[EntryIndex].Pattern,
        ChannelPatternDictionaryName(Encoding, Channel)));

  CloseFile(OutFile);

  AssignFile(OutFile, ConcatPaths([CacheDir, 'render', 'subpattern.htt']));
  Rewrite(OutFile);

  for I := Low(Song.Instruments.All) to High(Song.Instruments.All) do
    if InstrumentIsUsed(ModInst(I), Song.Instruments.All[I].Type_, UsedStuff) then
      with Song.Instruments.All[I] do begin
        if SubpatternEnabled then begin
          WriteStr(TypePrefix, Type_);
          Write(OutFile, RenderSubpattern(TypePrefix+'SP' +
            IntToStr(UsedStuff.InstrumentMap[Type_, ModInst(I)]),
            Subpattern, Type_, UsedStuff));
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
               Filename + '_gbs.obj'],
              Filename + '.map',
              Filename + '.sym') <> 0 then Die;
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

    LastGeneratedSongSize := ReadSongSizeFromMap(Filename + '.map');

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
