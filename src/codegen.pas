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
    BodyOccurrences: Integer;
    LastStream: Integer;
    NextOffset: Integer;
  end;
  TRoutineCandidateMap = specialize TFPGMap<RawByteString,
    TRoutineCandidateStats>;

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
  //   201..254 call routine 0..53
  //   255      return from a routine
  NOTE_CATALOG_SIZE = 128;
  PATTERN_CALL_BASE = 201;
  PATTERN_CALL_COUNT = 54;
  PATTERN_RETURN = 255;
  MAX_ROUTINE_LENGTH = 64;
  MAX_PATTERN_DICTIONARY_BYTES = 256;
  PATTERN_DICTIONARY_POINTER_BYTES = 4 * 2;

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

function EncodePattern(const Pattern: TPattern;
  const Catalog: TNoteCatalog): TEncodedPattern;
var
  Row, CatalogIndex: Integer;
  NoteRecord: Integer;
begin
  Result := nil;
  SetLength(Result, Length(Pattern));
  for Row := Low(TPattern) to High(TPattern) do begin
    NoteRecord := CellToNoteRecord(Pattern[Row]);
    CatalogIndex := CatalogIndexOf(Catalog, NoteRecord);
    if CatalogIndex >= 0 then
      Result[Row] := CatalogIndex
    else
      Result[Row] := -NoteRecord - 1;
  end;
end;

procedure AppendEncodedToken(var Pattern: TEncodedPattern; Token: Integer);
var
  I: Integer;
begin
  I := Length(Pattern);
  SetLength(Pattern, I + 1);
  Pattern[I] := Token;
end;

function RoutineTokenEligible(Token: Integer): Boolean;
begin
  Result := InRange(Token, 0, NOTE_CATALOG_SIZE - 1) or
    InRange(Token, PATTERN_CALL_BASE,
      PATTERN_CALL_BASE + PATTERN_CALL_COUNT - 1);
end;

function EncodedTokenSize(Token: Integer): Integer;
begin
  if Token < 0 then Result := 3
  else Result := 1;
end;

function EncodedStreamSize(const Stream: TEncodedPattern): Integer;
var
  Token: Integer;
begin
  Result := 0;
  for Token in Stream do Inc(Result, EncodedTokenSize(Token));
end;

function CandidateMatches(const Stream: TEncodedPattern; Offset: Integer;
  const Candidate: RawByteString): Boolean;
var
  I: Integer;
begin
  if Offset + Length(Candidate) > Length(Stream) then Exit(False);
  for I := 1 to Length(Candidate) do
    if Stream[Offset + I - 1] <> Ord(Candidate[I]) then Exit(False);
  Result := True;
end;

procedure ReplaceCandidate(var Stream: TEncodedPattern;
  const Candidate: RawByteString; Replacement: Integer);
var
  NewStream: TEncodedPattern;
  Offset: Integer;
begin
  NewStream := nil;
  Offset := 0;
  while Offset < Length(Stream) do begin
    if CandidateMatches(Stream, Offset, Candidate) then begin
      AppendEncodedToken(NewStream, Replacement);
      Inc(Offset, Length(Candidate));
    end
    else begin
      AppendEncodedToken(NewStream, Stream[Offset]);
      Inc(Offset);
    end;
  end;
  Stream := NewStream;
end;

procedure InlineRoutine(var Stream: TEncodedPattern; RoutineIndex: Integer;
  const Body: TEncodedPattern);
var
  NewStream: TEncodedPattern;
  Token, BodyToken: Integer;
begin
  NewStream := nil;
  for Token in Stream do begin
    if Token = PATTERN_CALL_BASE + RoutineIndex then
      for BodyToken in Body do AppendEncodedToken(NewStream, BodyToken)
    else
      AppendEncodedToken(NewStream, Token);
  end;
  Stream := NewStream;
end;

function BuildGroupEncoding(const Song: TSong; ChannelMask: Byte;
  const UsedStuff: TUsedStuff;
  const Catalogs: TNoteCatalogs): TGroupEncoding;
var
  Candidates: TRoutineCandidateMap;
  Stats, BestStats: TRoutineCandidateStats;
  Candidate, BestCandidate: RawByteString;
  PatternIndex, EntryIndex, RoutineIndex, CandidateLength: Integer;
  StreamIndex, CandidateIndex, I, Gain, BestGain, BestScore: Integer;
  Score, DictionaryBytes, NewDictionaryBytes, Replacement: Integer;
  ReferenceCounts, RoutineMap: array of Integer;
  Active: array of Boolean;
  BestRoutine, Saving, BestSaving, NewRoutineIndex: Integer;
  Channel: TChannel;
  CompactedRoutines: TRoutineDictionary;

  procedure RecordCandidate(const Stream: TEncodedPattern;
    AStreamIndex: Integer; IsBody: Boolean);
  var
    AStart, ALength, Token, Index: Integer;
    AStats: TRoutineCandidateStats;
    AKey: RawByteString;
  begin
    for AStart := 0 to Length(Stream) - 1 do begin
      if not RoutineTokenEligible(Stream[AStart]) then Continue;
      AKey := '';
      for ALength := 1 to MAX_ROUTINE_LENGTH do begin
        Index := AStart + ALength - 1;
        if Index >= Length(Stream) then Break;
        Token := Stream[Index];
        if not RoutineTokenEligible(Token) then Break;
        AKey += AnsiChar(Token);
        if ALength < 2 then Continue;

        Index := Candidates.IndexOf(AKey);
        if Index = -1 then begin
          AStats.Occurrences := 1;
          AStats.BodyOccurrences := Ord(IsBody);
          AStats.LastStream := AStreamIndex;
          AStats.NextOffset := AStart + ALength;
          Candidates.Add(AKey, AStats);
        end
        else begin
          AStats := Candidates.Data[Index];
          if (AStats.LastStream <> AStreamIndex) or
            (AStart >= AStats.NextOffset) then begin
            Inc(AStats.Occurrences);
            if IsBody then Inc(AStats.BodyOccurrences);
            AStats.LastStream := AStreamIndex;
            AStats.NextOffset := AStart + ALength;
            Candidates.Data[Index] := AStats;
          end;
        end;
      end;
    end;
  end;

  procedure CountRoutineReferences(const Stream: TEncodedPattern);
  var
    Token, CalledRoutine: Integer;
  begin
    for Token in Stream do begin
      if not InRange(Token, PATTERN_CALL_BASE,
        PATTERN_CALL_BASE + PATTERN_CALL_COUNT - 1) then Continue;
      CalledRoutine := Token - PATTERN_CALL_BASE;
      if InRange(CalledRoutine, 0, Length(Active) - 1) and
        Active[CalledRoutine] then
        Inc(ReferenceCounts[CalledRoutine]);
    end;
  end;

begin
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
        Song.Patterns.Data[PatternIndex]^, Catalogs[Channel]);
    end;
  end;

  DictionaryBytes := 0;
  while Length(Result.Routines) < PATTERN_CALL_COUNT do begin
    Candidates := TRoutineCandidateMap.Create;
    try
      Candidates.Sorted := True;
      StreamIndex := 0;
      for EntryIndex := 0 to Length(Result.Patterns) - 1 do begin
        RecordCandidate(Result.Patterns[EntryIndex].Pattern,
          StreamIndex, False);
        Inc(StreamIndex);
      end;
      for RoutineIndex := 0 to Length(Result.Routines) - 1 do begin
        RecordCandidate(Result.Routines[RoutineIndex], StreamIndex, True);
        Inc(StreamIndex);
      end;

      BestCandidate := '';
      BestGain := 0;
      BestScore := 0;
      FillChar(BestStats, SizeOf(BestStats), 0);
      for CandidateIndex := 0 to Candidates.Count - 1 do begin
        Candidate := Candidates.Keys[CandidateIndex];
        Stats := Candidates.Data[CandidateIndex];
        CandidateLength := Length(Candidate);
        NewDictionaryBytes := DictionaryBytes -
          Stats.BodyOccurrences * (CandidateLength - 1) +
          CandidateLength + 2; // Offset-table entry, body, RETURN.
        if NewDictionaryBytes > MAX_PATTERN_DICTIONARY_BYTES then Continue;

        Gain := Stats.Occurrences * (CandidateLength - 1) -
          (CandidateLength + 2);
        if Gain <= 0 then Continue;
        // The small use-count bias consistently improves the corpus result.
        Score := Gain * 2 + Stats.Occurrences;
        if (Score > BestScore) or
          ((Score = BestScore) and (Length(BestCandidate) > 0) and
           (CandidateLength > Length(BestCandidate))) then begin
          BestCandidate := Candidate;
          BestStats := Stats;
          BestGain := Gain;
          BestScore := Score;
        end;
      end;
    finally
      Candidates.Free;
    end;

    if (BestGain <= 0) or (Length(BestCandidate) = 0) then Break;

    Replacement := PATTERN_CALL_BASE + Length(Result.Routines);
    for EntryIndex := 0 to Length(Result.Patterns) - 1 do
      ReplaceCandidate(Result.Patterns[EntryIndex].Pattern,
        BestCandidate, Replacement);
    for RoutineIndex := 0 to Length(Result.Routines) - 1 do
      ReplaceCandidate(Result.Routines[RoutineIndex],
        BestCandidate, Replacement);

    RoutineIndex := Length(Result.Routines);
    SetLength(Result.Routines, RoutineIndex + 1);
    SetLength(Result.Routines[RoutineIndex], Length(BestCandidate));
    for I := 1 to Length(BestCandidate) do
      Result.Routines[RoutineIndex][I - 1] := Ord(BestCandidate[I]);
    DictionaryBytes := DictionaryBytes -
      BestStats.BodyOccurrences * (Length(BestCandidate) - 1) +
      Length(BestCandidate) + 2;
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
      Saving := Length(Result.Routines[RoutineIndex]) + 2 -
        ReferenceCounts[RoutineIndex] *
          (Length(Result.Routines[RoutineIndex]) - 1);
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
      if InRange(Result.Patterns[EntryIndex].Pattern[I], PATTERN_CALL_BASE,
        PATTERN_CALL_BASE + PATTERN_CALL_COUNT - 1) then
        Result.Patterns[EntryIndex].Pattern[I] := PATTERN_CALL_BASE +
          RoutineMap[Result.Patterns[EntryIndex].Pattern[I] -
            PATTERN_CALL_BASE];
  for RoutineIndex := 0 to Length(CompactedRoutines) - 1 do
    for I := 0 to Length(CompactedRoutines[RoutineIndex]) - 1 do
      if InRange(CompactedRoutines[RoutineIndex][I], PATTERN_CALL_BASE,
        PATTERN_CALL_BASE + PATTERN_CALL_COUNT - 1) then
        CompactedRoutines[RoutineIndex][I] := PATTERN_CALL_BASE +
          RoutineMap[CompactedRoutines[RoutineIndex][I] - PATTERN_CALL_BASE];
  Result.Routines := CompactedRoutines;

  Result.EncodedSize := 0;
  for EntryIndex := 0 to Length(Result.Patterns) - 1 do
    Inc(Result.EncodedSize,
      EncodedStreamSize(Result.Patterns[EntryIndex].Pattern));
  if Length(Result.Routines) = 0 then
    DictionaryBytes := 1 // Keep an addressable dummy dictionary byte.
  else begin
    DictionaryBytes := Length(Result.Routines); // Offset table.
    for RoutineIndex := 0 to Length(Result.Routines) - 1 do
      Inc(DictionaryBytes,
        EncodedStreamSize(Result.Routines[RoutineIndex]) + 1); // RETURN.
  end;
  if DictionaryBytes > MAX_PATTERN_DICTIONARY_BYTES then
    raise Exception.CreateFmt('Pattern dictionary is %d bytes; maximum is %d',
      [DictionaryBytes, MAX_PATTERN_DICTIONARY_BYTES]);
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
  const Dictionary: TPatternDictionary);
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
    if Value <> CellToNoteRecord(OriginalPattern^[Row]) then
      raise Exception.CreateFmt(
        'Pattern routine encoding mismatch in channel %d pattern %d row %d',
        [Ord(Channel) + 1, Encoding.Patterns[EntryIndex].PatternKey, Row]);
    Inc(Row);
  end;

  procedure DecodeStream(const Stream: TEncodedPattern);
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
      else if InRange(Token, PATTERN_CALL_BASE,
        PATTERN_CALL_BASE + PATTERN_CALL_COUNT - 1) then begin
        RoutineIndex := Token - PATTERN_CALL_BASE;
        if not InRange(RoutineIndex, 0, Length(Dictionary.Routines) - 1) then
          raise Exception.CreateFmt(
            'Invalid routine index in channel %d pattern %d',
            [Ord(Channel) + 1, Encoding.Patterns[EntryIndex].PatternKey]);
        if OnStack[RoutineIndex] then
          raise Exception.CreateFmt(
            'Recursive routine cycle in channel %d pattern %d',
            [Ord(Channel) + 1, Encoding.Patterns[EntryIndex].PatternKey]);
        OnStack[RoutineIndex] := True;
        DecodeStream(Dictionary.Routines[RoutineIndex]);
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
    DecodeStream(Encoding.Patterns[EntryIndex].Pattern);
    if Row <> Length(TPattern) then
      raise Exception.CreateFmt(
        'Pattern routine encoding ended early in channel %d pattern %d',
        [Ord(Channel) + 1, Encoding.Patterns[EntryIndex].PatternKey]);
  end;
end;

function BuildSongEncoding(const Song: TSong; const UsedStuff: TUsedStuff;
  const Catalogs: TNoteCatalogs): TSongEncoding;
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

  for Mask := 1 to ALL_CHANNELS_MASK do
    Groups[Mask] := BuildGroupEncoding(Song, Mask, UsedStuff, Catalogs);

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
      Result.Channels[Channel], Result.Dictionaries[DictionaryIndex]);
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
      'DN((NOTE) | 0x80, INSTRUMENT, EFFECT)' + LineEnding +
    '#define pattern_call(INDEX) (unsigned char)(%d + (INDEX))' +
      LineEnding +
    '#define pattern_return (unsigned char)%d',
    [PATTERN_CALL_BASE, PATTERN_RETURN]);
end;

function RenderRGBDSPatternMacros: String;
begin
  Result := Format(
    'MACRO dn_literal' + LineEnding +
    ' dn (\1 | $80), \2, \3' + LineEnding +
    'ENDM' + LineEnding + LineEnding +
    'MACRO pattern_call' + LineEnding +
    ' db %d + \1' + LineEnding +
    'ENDM' + LineEnding + LineEnding +
    'MACRO pattern_return' + LineEnding +
    ' db %d' + LineEnding +
    'ENDM', [PATTERN_CALL_BASE, PATTERN_RETURN]);
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

function RenderGBDKPatternDictionary(Name: String;
  const Routines: TRoutineDictionary): String;
var
  I, J, Offset, Token, NoteRecord: Integer;
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Add('/* Direct routine-offset table followed by RETURN-terminated ' +
      'routine bytecode. */');
    SL.Add('static const unsigned char ' + Name + '[] = {');
    if Length(Routines) = 0 then
      SL.Add('    0,')
    else begin
      Offset := Length(Routines);
      for I := 0 to Length(Routines) - 1 do begin
        SL.Add(Format('    %d, /* routine %d offset */', [Offset, I]));
        Inc(Offset, EncodedStreamSize(Routines[I]) + 1);
      end;
      for I := 0 to Length(Routines) - 1 do begin
        SL.Add(Format('    /* routine %d */', [I]));
        for J := 0 to Length(Routines[I]) - 1 do begin
          Token := Routines[I][J];
          if Token < 0 then begin
            NoteRecord := -Token - 1;
            SL.Add('    dn_literal(' +
              NoteRecordDNArgs(NoteRecord, '0x', True) + '),');
          end
          else if Token < NOTE_CATALOG_SIZE then
            SL.Add('    ' + IntToStr(Token) + ',')
          else
            SL.Add(Format('    pattern_call(%d),',
              [Token - PATTERN_CALL_BASE]));
        end;
        SL.Add('    pattern_return,');
      end;
    end;
    SL.Add('};');
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
    SL.Add('; Direct routine-offset table followed by RETURN-terminated ' +
      'routine bytecode.');
    SL.Add(Name + ':');
    if Length(Routines) = 0 then
      SL.Add(' db 0')
    else begin
      for I := 0 to Length(Routines) - 1 do begin
        RoutineName := Format('%s_routine_%d', [Name, I]);
        SL.Add(Format(' db %s - %s ; routine %d offset',
          [RoutineName, Name, I]));
      end;
      for I := 0 to Length(Routines) - 1 do begin
        SL.Add(Format('%s_routine_%d:', [Name, I]));
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
            SL.Add(Format(' pattern_call %d',
              [Token - PATTERN_CALL_BASE]));
        end;
        SL.Add(' pattern_return');
      end;
      SL.Add(Format('ASSERT @ - %s <= %d',
        [Name, MAX_PATTERN_DICTIONARY_BYTES]));
    end;
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function RenderGBDKCompressedPattern(Name: String;
  const Encoded: TEncodedPattern): String;
var
  I, NoteRecord: Integer;
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Add('static const unsigned char ' + Name + '[] = {');
    for I := 0 to Length(Encoded) - 1 do begin
      if Encoded[I] <= 127 then begin
        if Encoded[I] >= 0 then
          SL.Add('    ' + IntToStr(Encoded[I]) + ',')
        else begin
          NoteRecord := -Encoded[I] - 1;
          SL.Add('    dn_literal(' +
            NoteRecordDNArgs(NoteRecord, '0x', True) + '),');
        end;
      end
      else begin
        SL.Add(Format('    pattern_call(%d),',
          [Encoded[I] - PATTERN_CALL_BASE]));
      end;
    end;
    SL.Add('};');
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function RenderRGBDSCompressedPattern(Name: String;
  const Encoded: TEncodedPattern): String;
var
  I, NoteRecord: Integer;
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Add(Name + ':');
    for I := 0 to Length(Encoded) - 1 do begin
      if Encoded[I] <= 127 then begin
        if Encoded[I] >= 0 then
          SL.Add(' db ' + IntToStr(Encoded[I]))
        else begin
          NoteRecord := -Encoded[I] - 1;
          SL.Add(' dn_literal ' +
            NoteRecordDNArgs(NoteRecord, '$', False));
        end;
      end
      else begin
        SL.Add(Format(' pattern_call %d',
          [Encoded[I] - PATTERN_CALL_BASE]));
      end;
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
  Encoding := BuildSongEncoding(Song, UsedStuff, NoteCatalogs);

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
        Encoding.Channels[Channel].Patterns[EntryIndex].Pattern));
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
    'order4, note_catalog1, note_catalog2, note_catalog3, note_catalog4,'+
    ' %s, %s, %s, %s,'+
    ' duty_instruments, wave_instruments, noise_instruments, NULL, waves};',
    [DescriptorName,
     Song.TicksPerRow[0], Song.TicksPerRow[1], Song.TicksPerRow[2], Song.TicksPerRow[3],
     OrderCount(Song)*2,
     ChannelPatternDictionaryName(Encoding, chDuty1),
     ChannelPatternDictionaryName(Encoding, chDuty2),
     ChannelPatternDictionaryName(Encoding, chWave),
     ChannelPatternDictionaryName(Encoding, chNoise)
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
  Encoding := BuildSongEncoding(Song, UsedStuff, NoteCatalogs);

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
  OutSL.Add(Format('dw %s, %s, %s, %s',
    [ChannelPatternDictionaryName(Encoding, chDuty1),
     ChannelPatternDictionaryName(Encoding, chDuty2),
     ChannelPatternDictionaryName(Encoding, chWave),
     ChannelPatternDictionaryName(Encoding, chNoise)]));
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
    for EntryIndex := 0 to Length(Encoding.Channels[Channel].Patterns) - 1 do
      OutSL.Add(RenderRGBDSCompressedPattern(
        ChannelPatternName(Channel,
          Encoding.Channels[Channel].Patterns[EntryIndex].PatternKey),
        Encoding.Channels[Channel].Patterns[EntryIndex].Pattern));

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
  Encoding := BuildSongEncoding(Song, UsedStuff, NoteCatalogs);

  if not DirectoryExists(ConcatPaths([CacheDir, 'render'])) then
    CreateDir(ConcatPaths([CacheDir, 'render']));

  FilePath := Filename;
  Filename := ConcatPaths([CacheDir, 'render', ExtractFileNameWithoutExt(ExtractFileNameOnly(Filename))]);

  WriteHTT(ConcatPaths([CacheDir, 'render', 'wave.htt']), RenderWaveforms(Song.Waves, UsedStuff.HighestWaveform));
  WriteHTT(ConcatPaths([CacheDir, 'render', 'order.htt']),
    RenderOrderTable(OrderMatrix));
  WriteHTT(ConcatPaths([CacheDir, 'render', 'duty_instrument.htt']),  RenderInstruments(Song.Instruments.Duty, UsedStuff.HighestDutyInst));
  WriteHTT(ConcatPaths([CacheDir, 'render', 'wave_instrument.htt']),  RenderInstruments(Song.Instruments.Wave, UsedStuff.HighestWaveInst));
  WriteHTT(ConcatPaths([CacheDir, 'render', 'noise_instrument.htt']), RenderInstruments(Song.Instruments.Noise, UsedStuff.HighestNoiseInst));
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
        Encoding.Channels[Channel].Patterns[EntryIndex].Pattern));

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
    // The bundled driver descriptor does not consume the four new dictionary
    // pointers yet, but they are part of the exported format's true size.
    if LastGeneratedSongSize >= 0 then
      Inc(LastGeneratedSongSize, PATTERN_DICTIONARY_POINTER_BYTES);

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
