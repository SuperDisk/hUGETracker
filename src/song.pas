unit Song;

{$mode objfpc}{$H+}

// TODO: Lots of duplicated code in here. At some point this should switch over
// to a more parsable format, like NBT, JSON, or XML or something.

interface

uses Classes, HugeDatatypes, instruments, Constants, math, sysutils, LazLoggerBase;

type
  ESongVersionException = class(Exception);
  EPatternSetException = class(Exception);

  TSongV1 = packed record
    Version: Integer;

    Name: ShortString;
    Artist: ShortString;
    Comment: ShortString;

    Instruments: TInstrumentBankV1;
    Waves: TWaveBankV1;

    TicksPerRow: Integer;

    Patterns: TPatternMapV1;
    OrderMatrix: TOrderMatrix;
  end;

  TSongV2 = packed record
    Version: Integer;

    Name: ShortString;
    Artist: ShortString;
    Comment: ShortString;

    Instruments: TInstrumentBankV1;
    Waves: TWaveBankV1;

    TicksPerRow: Integer;

    Patterns: TPatternMapV1;
    OrderMatrix: TOrderMatrix;

    Routines: TRoutineBank;
  end;

  TSongV3 = packed record
    Version: Integer;

    Name: ShortString;
    Artist: ShortString;
    Comment: ShortString;

    Instruments: TInstrumentCollectionV1;
    Waves: TWaveBank;

    TicksPerRow: Integer;

    Patterns: TPatternMapV1;
    OrderMatrix: TOrderMatrix;

    Routines: TRoutineBank;
  end;

  TSongV4 = packed record
    Version: Integer;

    Name: ShortString;
    Artist: ShortString;
    Comment: ShortString;

    Instruments: TInstrumentCollectionV2;
    Waves: TWaveBank;

    TicksPerRow: Integer;

    Patterns: TPatternMapV1;
    OrderMatrix: TOrderMatrix;

    Routines: TRoutineBank;
  end;

  TSongV5 = TSongV4; // no structural differences

  TSongV6 = packed record
    Version: Integer;

    Name: ShortString;
    Artist: ShortString;
    Comment: ShortString;

    Instruments: TInstrumentCollection;
    Waves: TWaveBank;

    TicksPerRow: Integer;

    TimerEnabled: Boolean;
    TimerDivider: Integer;

    Patterns: TPatternMap;
    OrderMatrix: TOrderMatrix;

    Routines: TRoutineBank;
  end;

  TSongV7 = packed record
    Version: Integer;

    Name: ShortString;
    Artist: ShortString;
    Comment: ShortString;

    Instruments: TInstrumentCollection;
    Waves: TWaveBank;

    TicksPerRow: packed array[0..3] of Integer;
    PatternLength: Integer;

    TimerEnabled: Boolean;
    TimerDivider: Integer;

    Patterns: TPatternMap;
    PatternSets: TPatternSetMap;
    Order: TOrder;

    Routines: TRoutineBank;
  end;

  { TSong }

  TSong = TSongV7;

procedure WriteSongToStream(S: TStream; const ASong: TSong);
procedure ReadSongFromStream(S: TStream; out ASong: TSong);
procedure InitializeSong(out S: TSong);
procedure LoadDefaultInstruments(var S: TSong);
procedure DestroySong(var S: TSong);

function UpgradeSong(S: TSongV1): TSong; overload;
function UpgradeSong(S: TSongV2): TSong; overload;
function UpgradeSong(S: TSongV3): TSong; overload;
function UpgradeSong(S: TSongV4): TSong; overload;
function UpgradeSong(S: TSongV6): TSong; overload;

function PatternSetExists(const S: TSong; PatternSetID: Integer): Boolean;
function GetPatternSet(const S: TSong; PatternSetID: Integer): TPatternSet;
function EnsurePatternSet(var S: TSong; PatternSetID: Integer): TPatternSet;
function CreatePatternSet(var S: TSong): Integer;
function ClonePatternSet(var S: TSong; SourcePatternSetID: Integer): Integer;
procedure SetOrderFromOrderMatrix(var S: TSong; const OrderMatrix: TOrderMatrix;
  DetachSharedPatterns: Boolean = True);
function BuildOrderMatrix(const S: TSong; OptimizePatterns: Boolean = False): TOrderMatrix;
function OrderCount(const Song: TSong): Integer;

implementation

uses Utils;

// Thanks to WP on the FreePascal forums for this code!
// https://forum.lazarus.freepascal.org/index.php/topic,47892.msg344152.html#msg344152

procedure ReadSongFromStreamV1(S: TStream; out ASong: TSongV1);
var
  i, n: Integer;
  pat: PPatternV1;
begin
  // Read the fixed elements first
  n := SizeOf(TSongV1) - SizeOf(TPatternMapV1) - SizeOf(TOrderMatrix);
  S.Read(ASong, n);

  // Create the patterns
  ASong.Patterns := TPatternMapV1.Create;
  // Read the pattern count
  S.Read(n, SizeOf(Integer));
  for i:=0 to n - 1 do begin
    // Allocate memory for each pattern ...
    New(pat);
    // and read the pattern content
    S.Read(pat^, SizeOf(TPatternV1));
    // Add the pattern to the list
    ASong.Patterns.Add(i, pat);
  end;

  // Read the OrderMatrix
  for i := 0 to 3 do
  begin
    // Read length of each OrderMatrix array
    S.Read(n, SizeOf(Integer));
    // Allocate memory for it
    SetLength(ASong.OrderMatrix[i], n);
    // Read content of OrderMatrix array
    S.Read(ASong.OrderMatrix[i, 0], n*SizeOf(Integer));
  end;
end;

procedure ReadSongFromStreamV2(S: TStream; out ASong: TSongV2);
var
  i, n: Integer;
  pat: PPatternV1;
begin
  // Read the fixed elements first
  n := SizeOf(TSongV2)
     - SizeOf(TPatternMapV1)
     - SizeOf(TOrderMatrix)
     - SizeOf(TRoutineBank);

  S.Read(ASong, n);

  // Create the patterns
  ASong.Patterns := TPatternMapV1.Create;
  // Read the pattern count
  S.Read(n, SizeOf(Integer));
  for i:=0 to n - 1 do begin
    // Allocate memory for each pattern ...
    New(pat);
    // and read the pattern content
    S.Read(pat^, SizeOf(TPatternV1));
    // Add the pattern to the list
    ASong.Patterns.Add(i, pat);
  end;

  // Read the OrderMatrix
  for i := 0 to 3 do
  begin
    // Read length of each OrderMatrix array
    S.Read(n, SizeOf(Integer));
    // Allocate memory for it
    SetLength(ASong.OrderMatrix[i], n);
    // Read content of OrderMatrix array
    S.Read(ASong.OrderMatrix[i, 0], n*SizeOf(Integer));
  end;

  for I := Low(TRoutineBank) to High(TRoutineBank) do
    ASong.Routines[I] := S.ReadAnsiString;
end;

procedure ReadSongFromStreamV3(S: TStream; out ASong: TSongV3);
var
  i, n: Integer;
  pat: PPatternV1;
begin
  // Read the fixed elements first
  n := SizeOf(TSongV3)
     - SizeOf(TPatternMapV1)
     - SizeOf(TOrderMatrix)
     - SizeOf(TRoutineBank);

  S.Read(ASong, n);

  // Create the patterns
  ASong.Patterns := TPatternMapV1.Create;
  // Read the pattern count
  S.Read(n, SizeOf(Integer));
  for i:=0 to n - 1 do begin
    // Allocate memory for each pattern ...
    New(pat);
    // and read the pattern content
    S.Read(pat^, SizeOf(TPatternV1));
    // Add the pattern to the list
    ASong.Patterns.Add(i, pat);
  end;

  // Read the OrderMatrix
  for i := 0 to 3 do
  begin
    // Read length of each OrderMatrix array
    S.Read(n, SizeOf(Integer));
    // Allocate memory for it
    SetLength(ASong.OrderMatrix[i], n);
    // Read content of OrderMatrix array
    S.Read(ASong.OrderMatrix[i, 0], n*SizeOf(Integer));
  end;

  for I := Low(TRoutineBank) to High(TRoutineBank) do
    ASong.Routines[I] := S.ReadAnsiString;
end;

procedure ReadSongFromStreamV4(S: TStream; out ASong: TSongV4);
var
  i, n: Integer;
  pat: PPatternV1;
begin
  // Read the fixed elements first
  n := SizeOf(TSongV4)
     - SizeOf(TPatternMapV1)
     - SizeOf(TOrderMatrix)
     - SizeOf(TRoutineBank);

  S.Read(ASong, n);

  // Create the patterns
  ASong.Patterns := TPatternMapV1.Create;
  // Read the pattern count
  S.Read(n, SizeOf(Integer));
  for i:=0 to n - 1 do begin
    // Allocate memory for each pattern ...
    New(pat);
    // and read the pattern content
    S.Read(pat^, SizeOf(TPatternV1));
    // Add the pattern to the list
    ASong.Patterns.Add(i, pat);
  end;

  // Read the OrderMatrix
  for i := 0 to 3 do
  begin
    // Read length of each OrderMatrix array
    S.Read(n, SizeOf(Integer));
    // Allocate memory for it
    SetLength(ASong.OrderMatrix[i], n);
    // Read content of OrderMatrix array
    S.Read(ASong.OrderMatrix[i, 0], n*SizeOf(Integer));
  end;

  for I := Low(TRoutineBank) to High(TRoutineBank) do
    ASong.Routines[I] := S.ReadAnsiString;
end;

procedure ReadSongFromStreamV5(S: TStream; out ASong: TSongV5);
var
  i, n, PatKey: Integer;
  pat: PPatternV1;
begin
  // Read the fixed elements first
  n := SizeOf(TSongV5)
     - SizeOf(TPatternMapV1)
     - SizeOf(TOrderMatrix)
     - SizeOf(TRoutineBank);

  S.Read(ASong, n);

  // Create the patterns
  ASong.Patterns := TPatternMapV1.Create;
  // Read the pattern count
  S.Read(n, SizeOf(Integer));
  for i:=0 to n - 1 do begin
    // Read pattern key
    S.Read(PatKey, SizeOf(Integer));
    // Allocate memory for each pattern ...
    New(pat);
    // and read the pattern content
    S.Read(pat^, SizeOf(TPatternV1));
    // Add the pattern to the list
    ASong.Patterns.Add(PatKey, pat);
  end;

  // Read the OrderMatrix
  for i := 0 to 3 do
  begin
    // Read length of each OrderMatrix array
    S.Read(n, SizeOf(Integer));
    // Allocate memory for it
    SetLength(ASong.OrderMatrix[i], n);
    // Read content of OrderMatrix array
    S.Read(ASong.OrderMatrix[i, 0], n*SizeOf(Integer));
  end;

  for I := Low(TRoutineBank) to High(TRoutineBank) do
    ASong.Routines[I] := S.ReadAnsiString;
end;

procedure ReadSongFromStreamV6(S: TStream; out ASong: TSongV6);
var
  i, n, PatKey: Integer;
  pat: PPattern;
begin
  // Read the fixed elements first
  n := SizeOf(TSongV6)
     - SizeOf(TPatternMap)
     - SizeOf(TOrderMatrix)
     - SizeOf(TRoutineBank);

  S.Read(ASong, n);

  // Create the patterns
  ASong.Patterns := TPatternMap.Create;
  // Read the pattern count
  S.Read(n, SizeOf(Integer));
  for i:=0 to n - 1 do begin
    // Read pattern key
    S.Read(PatKey, SizeOf(Integer));
    // Allocate memory for each pattern ...
    New(pat);
    // and read the pattern content
    S.Read(pat^, SizeOf(TPattern));
    // Add the pattern to the list
    ASong.Patterns.Add(PatKey, pat);
  end;

  // Read the OrderMatrix
  for i := 0 to 3 do
  begin
    // Read length of each OrderMatrix array
    S.Read(n, SizeOf(Integer));
    // Allocate memory for it
    SetLength(ASong.OrderMatrix[i], n);
    // Read content of OrderMatrix array
    S.Read(ASong.OrderMatrix[i, 0], n*SizeOf(Integer));
  end;

  for I := Low(TRoutineBank) to High(TRoutineBank) do
    ASong.Routines[I] := S.ReadAnsiString;
end;

procedure ReadSongFromStreamV7(S: TStream; out ASong: TSongV7);
var
  I, N, Key: Integer;
  Pat: PPattern;
  PatternSet: TPatternSet;
begin
  N := SizeOf(TSongV7)
     - SizeOf(TPatternMap)
     - SizeOf(TPatternSetMap)
     - SizeOf(TOrder)
     - SizeOf(TRoutineBank);

  S.Read(ASong, N);
  ASong.PatternLength := EnsureRange(ASong.PatternLength, 1,
    Length(TPattern));

  ASong.Patterns := TPatternMap.Create;
  S.Read(N, SizeOf(Integer));
  for I := 0 to N - 1 do begin
    S.Read(Key, SizeOf(Integer));
    New(Pat);
    S.Read(Pat^, SizeOf(TPattern));
    ASong.Patterns.Add(Key, Pat);
  end;

  ASong.PatternSets := TPatternSetMap.Create;
  S.Read(N, SizeOf(Integer));
  for I := 0 to N - 1 do begin
    S.Read(Key, SizeOf(Integer));
    S.Read(PatternSet, SizeOf(TPatternSet));
    ASong.PatternSets.Add(Key, PatternSet);
  end;

  S.Read(N, SizeOf(Integer));
  SetLength(ASong.Order, N);
  if N > 0 then
    S.Read(ASong.Order[0], N * SizeOf(Integer));

  for I := Low(TRoutineBank) to High(TRoutineBank) do
    ASong.Routines[I] := S.ReadAnsiString;
end;

procedure WriteSongToStream(S: TStream; const ASong: TSong);
var
  I, N, Key: Integer;
  PatternSet: TPatternSet;
begin
  // Write the fixed record elements first
  N := SizeOf(TSong)
     - SizeOf(TPatternMap)
     - SizeOf(TPatternSetMap)
     - SizeOf(TOrder)
     - SizeOf(TRoutineBank);
  S.Write(ASong, N);

  // Write the pattern count
  S.Write(ASong.Patterns.Count, SizeOf(Integer));
  // Write the patterns
  for I := 0 to ASong.Patterns.Count-1 do
  begin
    Key := ASong.Patterns.Keys[I];
    S.Write(Key, SizeOf(Integer));
    S.Write(ASong.Patterns.Data[I]^, SizeOf(TPattern));
  end;

  S.Write(ASong.PatternSets.Count, SizeOf(Integer));
  for I := 0 to ASong.PatternSets.Count - 1 do begin
    Key := ASong.PatternSets.Keys[I];
    PatternSet := ASong.PatternSets.Data[I];
    S.Write(Key, SizeOf(Integer));
    S.Write(PatternSet, SizeOf(TPatternSet));
  end;

  N := Length(ASong.Order);
  S.Write(N, SizeOf(Integer));
  if N > 0 then
    S.Write(ASong.Order[0], N * SizeOf(Integer));

  // Write the routines
  for I := Low(TRoutineBank) to High(TRoutineBank) do
    S.WriteAnsiString(ASong.Routines[I]);
end;

procedure ReadSongFromStream(S: TStream; out ASong: TSong);
var
  Version: Integer;
  SV1: TSongV1;
  SV2: TSongV2;
  SV3: TSongV3;
  SV4: TSongV4;
  SV5: TSongV5;
  SV6: TSongV6;
begin
  S.Read(Version, SizeOf(Integer));
  S.Seek(0, soBeginning);
  case Version of
    0..1: begin
      ReadSongFromStreamV1(S, SV1);
      ASong := UpgradeSong(SV1);
    end;
    2: begin
      ReadSongFromStreamV2(S, SV2);
      ASong := UpgradeSong(SV2);
    end;
    3: begin
      ReadSongFromStreamV3(S, SV3);
      ASong := UpgradeSong(SV3);
    end;
    4: begin
      ReadSongFromStreamV4(S, SV4);
      ASong := UpgradeSong(SV4);
    end;
    5: begin
      ReadSongFromStreamV5(S, SV5);
      ASong := UpgradeSong(SV5);
    end;
    6: begin
      ReadSongFromStreamV6(S, SV6);
      ASong := UpgradeSong(SV6);
    end;
    7: begin
      ReadSongFromStreamV7(S, ASong);
    end
    else begin
      raise ESongVersionException.Create(IntToStr(Version));
    end;
  end;
end;

procedure InitializeSong(out S: TSong);
var
  I, J: Integer;
begin
  with S do begin
    Version := UGE_FORMAT_VERSION;
    Name := '';
    Artist := '';
    Comment := '';
  end;

  for I := Low(S.Instruments.All) to High(S.Instruments.All) do begin
    S.Instruments.All[I] := Default(TInstrument);
    BlankPattern(@S.Instruments.All[I].Subpattern);
  end;

  for I := Low(S.Instruments.Duty) to High(S.Instruments.Duty) do begin
    with S.Instruments.Duty[I] do begin
      Type_ := itSquare;
      Length := 0;
      LengthEnabled := False;
      InitialVolume := High(TEnvelopeVolume);
      VolSweepDirection := stDown;
      VolSweepAmount := 0;

      SweepTime := 0;
      SweepIncDec := stDown;
      SweepShift := 0;

      Duty := 2;

      OutputLevel := 1;
    end;
  end;

  for I := Low(S.Instruments.Wave) to High(S.Instruments.Wave) do
    with S.Instruments.Wave[I] do begin
      Type_ := itWave;
      Length := 0;
      LengthEnabled := False;
      OutputLevel := 1;
      Waveform := I-1;
    end;

  for I := Low(S.Instruments.Noise) to High(S.Instruments.Noise) do
    with S.Instruments.Noise[I] do begin
      Type_ := itNoise;
      Length := 0;
      LengthEnabled := False;
      InitialVolume := High(TEnvelopeVolume);
      VolSweepDirection := stDown;
      VolSweepAmount := 0;
      CounterStep := swFifteen;
    end;

  for I := Low(S.Waves) to High(S.Waves) do begin
    for J := Low(TWave) to High(TWave) do
      S.Waves[I][J] := random($F);
  end;

  for I := Low(TRoutineBank) to High(TRoutineBank) do
    S.Routines[I] := '';

  S.TicksPerRow[0] := 7;
  S.TicksPerRow[1] := 7;
  S.TicksPerRow[2] := 7;
  S.TicksPerRow[3] := 7;
  S.PatternLength := Length(TPattern);
  S.TimerDivider := 0;
  S.TimerEnabled := False;
  S.Patterns := TPatternMap.Create;
  S.PatternSets := TPatternSetMap.Create;
  SetLength(S.Order, 1);
  S.Order[0] := CreatePatternSet(S);
end;

procedure LoadDefaultInstruments(var S: TSong);
var
  I: Integer;
begin
  with S.Instruments do begin
    Wave[1].Name := 'Square wave 12.5%';
    Wave[2].Name := 'Square wave 25%';
    Wave[3].Name := 'Square wave 50%';
    Wave[4].Name := 'Square wave 75%';
    Wave[5].Name := 'Sawtooth wave';
    Wave[6].Name := 'Triangle wave';
    Wave[7].Name := 'Sine wave';
    Wave[8].Name := 'Toothy';
    Wave[9].Name := 'Triangle Toothy';
    Wave[10].Name := 'Pointy';
    Wave[11].Name := 'Strange';

    Duty[1].Name := 'Duty 12.5%';
    Duty[1].Duty := 0;

    Duty[2].Name := 'Duty 25%';
    Duty[2].Duty := 1;

    Duty[3].Name := 'Duty 50%';
    Duty[3].Duty := 2;

    Duty[4].Name := 'Duty 75%';
    Duty[4].Duty := 3;

    Duty[5].Name := 'Duty 12.5% plink';
    Duty[5].Duty := 0;
    Duty[5].VolSweepAmount := 1;

    Duty[6].Name := 'Duty 25% plink';
    Duty[6].Duty := 1;
    Duty[6].VolSweepAmount := 1;

    Duty[7].Name := 'Duty 50% plink';
    Duty[7].Duty := 2;
    Duty[7].VolSweepAmount := 1;

    Duty[8].Name := 'Duty 75% plink';
    Duty[8].Duty := 3;
    Duty[8].VolSweepAmount := 1;
  end;

  for I := Low(DefaultWaves) to High(DefaultWaves) do
    S.Waves[I] := DefaultWaves[I];
end;

procedure DestroySong(var S: TSong);
begin
  S.PatternSets.Free;
  S.Patterns.Free;
end;

function UpgradeSong(S: TSongV1): TSong;
var
  SV2: TSongV2;
begin
  SV2 := Default(TSongV2);
  Move(S, SV2, SizeOf(TSongV1) - SizeOf(TPatternMap) - SizeOf(TOrderMatrix));

  // Gotta preserve reference count so can't include it in the move...
  SV2.Patterns := S.Patterns;
  SV2.OrderMatrix := S.OrderMatrix;
  Inc(SV2.Version); // Bump version

  Result := UpgradeSong(SV2);
end;

function UpgradeSong(S: TSongV2): TSong;
var
  SV3: TSongV3;
  I: Integer;
begin
  SV3.Version:=3;
  SV3.Name:=S.Name;
  SV3.Artist:=S.Artist;
  SV3.Comment:=S.Comment;
  SV3.TicksPerRow:=S.TicksPerRow;
  SV3.Patterns:=S.Patterns;
  SV3.OrderMatrix:=S.OrderMatrix;
  SV3.Routines:=S.Routines;

  // AWFUL!!!!
  for I := Low(SV3.Instruments.All) to High(SV3.Instruments.All) do
    SV3.Instruments.All[I] := Default(TInstrumentV1);

  for I := Low(SV3.Instruments.Duty) to High(SV3.Instruments.Duty) do begin
    with SV3.Instruments.Duty[I] do begin
      Type_ := itSquare;
      Length := 0;
      LengthEnabled := False;
      InitialVolume := High(TEnvelopeVolume);
      VolSweepDirection := stDown;
      VolSweepAmount := 0;

      SweepTime := 0;
      SweepIncDec := stDown;
      SweepShift := 0;

      Duty := 2;

      OutputLevel := 1;
    end;
  end;

  for I := Low(SV3.Instruments.Wave) to High(SV3.Instruments.Wave) do
    with SV3.Instruments.Wave[I] do begin
      Type_ := itWave;
      Length := 0;
      LengthEnabled := False;
      OutputLevel := 1;
      Waveform := I-1;
    end;

  for I := Low(SV3.Instruments.Noise) to High(SV3.Instruments.Noise) do
    with SV3.Instruments.Noise[I] do begin
      Type_ := itNoise;
      Length := 0;
      LengthEnabled := False;
      InitialVolume := High(TEnvelopeVolume);
      VolSweepDirection := stDown;
      VolSweepAmount := 0;
      ShiftClockFreq := 0;
      DividingRatio := 0;
      CounterStep := swFifteen;
    end;

  for I := Low(S.Instruments) to High(S.Instruments) do begin
    case S.Instruments[I].Type_ of
      itSquare: SV3.Instruments.Duty[I] := S.Instruments[I];
      itWave: SV3.Instruments.Wave[I]   := S.Instruments[I];
      itNoise: SV3.Instruments.Noise[I] := S.Instruments[I];
    end;
  end;

  for I := Low(TWaveBank) to High(TWaveBank) do
    Move(S.Waves[I], SV3.Waves[I], SizeOf(TWaveV2));

  Result := UpgradeSong(SV3);
end;

function UpgradeSong(S: TSongV3): TSong;
var
  SV4: TSongV4;
  I, K: Integer;
  Pat: PPattern;

  function UsedInCH4(PatternIndex: Integer): Boolean;
  var
    J: Integer;
  begin
    for J := Low(SV4.OrderMatrix[3]) to High(SV4.OrderMatrix[3])-1 do // off by one error....
      if SV4.OrderMatrix[3, J] = PatternIndex then
        Exit(True);

    Result := False;
  end;

  procedure ConvertPattern(var Pat: TPatternV2);
  var
    I: Integer;
    Regs: TRegisters;
    PolyCounter: TPolynomialCounterRegister absolute Regs.NR43;
    Ch4Freq: Integer;
    RealR: Double;
  begin
    for I := Low(Pat) to High(Pat) do begin
      if (Pat[I].Instrument = 0) or (Pat[I].Note = NO_NOTE) then Continue;

      Regs := NoiseInstrumentToRegisters(NotesToFreqs.KeyData[Pat[I].Note], False, Result.Instruments.Noise[Pat[I].Instrument]);
      if PolyCounter.DividingRatio = 0 then
        RealR := 0.5
      else
        RealR := PolyCounter.DividingRatio;

      Ch4Freq := Trunc((524288 / RealR) / 2**(PolyCounter.ShiftClockFrequency+1));
      if not Ch4FreqToNoteCodeMap.TryGetData(Ch4Freq, Pat[I].Note) then
        DebugLn(['[DEBUG] Note value ', Pat[I].Note, ' not found.']);
    end;
  end;
begin
  SV4.Version:=4;
  SV4.Name:=S.Name;
  SV4.Artist:=S.Artist;
  SV4.Comment:=S.Comment;
  SV4.TicksPerRow:=S.TicksPerRow;
  SV4.Patterns:=S.Patterns;
  SV4.OrderMatrix:=S.OrderMatrix;
  SV4.Routines:=S.Routines;
  SV4.Waves := S.Waves;

  // Create a blank noise macro for all noise instruments
  for I := Low(S.Instruments.All) to High(S.Instruments.All) do begin
    Move(S.Instruments.All[I], SV4.Instruments.All[I], SizeOf(TInstrumentV1));
    SV4.Instruments.All[I].NoiseMacro := Default(TNoiseMacro);
  end;

  Result := UpgradeSong(SV4);

  // Rewrite noise patterns to accomodate the new noise instruments...
  for I := 0 to Result.Patterns.Count-1 do
    if UsedInCH4(Result.Patterns.Keys[I]) then begin
      ConvertPattern(Result.Patterns.Data[I]^);
    end;
end;

function UpgradeSong(S: TSongV4): TSong;
var
  SV6: TSongV6;
  I, K: Integer;

  function ConvertPattern(Pat: PPatternV1): PPattern;
  var
    J: Integer;
  begin
    New(Result);
    for J := Low(TPatternV1) to High(TPatternV1) do begin
      Result^[J].Instrument := Pat^[J].Instrument;
      Result^[J].EffectCode := Pat^[J].EffectCode;
      Result^[J].EffectParams.Value := Pat^[J].EffectParams.Value;
      Result^[J].Note := Pat^[J].Note;
      Result^[J].Volume := 0;
    end;
  end;

  function ConvertNoiseMacro(NoiseMacro: TNoiseMacro): TPattern;
  var
    J: Integer;
    WrapPoint: Integer;
  begin
    BlankPattern(@Result);
    for J := Low(NoiseMacro) to High(NoiseMacro) do
      Result[J+1].Note := NoiseMacro[J] + 36;

    WrapPoint := Min(S.TicksPerRow, 7);
    Result[WrapPoint-1].Volume := WrapPoint; // hold on last row
  end;
begin
  SV6.Version:=6;

  SV6.Name:=S.Name;
  SV6.Artist:=S.Artist;
  SV6.Comment:=S.Comment;

  for I := Low(S.Instruments.All) to High(S.Instruments.All) do begin
    SV6.Instruments.All[I].Type_ := S.Instruments.All[I].Type_;
    SV6.Instruments.All[I].Name := S.Instruments.All[I].Name;
    SV6.Instruments.All[I].Length := S.Instruments.All[I].Length;
    SV6.Instruments.All[I].LengthEnabled := S.Instruments.All[I].LengthEnabled;
    SV6.Instruments.All[I].InitialVolume := S.Instruments.All[I].InitialVolume;
    SV6.Instruments.All[I].VolSweepDirection := S.Instruments.All[I].VolSweepDirection;
    SV6.Instruments.All[I].VolSweepAmount := S.Instruments.All[I].VolSweepAmount;
    SV6.Instruments.All[I].SweepTime := S.Instruments.All[I].SweepTime;
    SV6.Instruments.All[I].SweepIncDec := S.Instruments.All[I].SweepIncDec;
    SV6.Instruments.All[I].SweepShift := S.Instruments.All[I].SweepShift;
    SV6.Instruments.All[I].Duty := S.Instruments.All[I].Duty;
    SV6.Instruments.All[I].OutputLevel := S.Instruments.All[I].OutputLevel;
    SV6.Instruments.All[I].Waveform := S.Instruments.All[I].Waveform;
    SV6.Instruments.All[I].CounterStep := S.Instruments.All[I].CounterStep;
    SV6.Instruments.All[I].SubpatternEnabled := False;
    BlankPattern(@SV6.Instruments.All[I].Subpattern);
  end;

  for I := Low(S.Instruments.Noise) to High(S.Instruments.Noise) do begin
    SV6.Instruments.Noise[I].Subpattern := ConvertNoiseMacro(S.Instruments.Noise[I].NoiseMacro);
    for K := Low(TNoiseMacro) to High(TNoiseMacro) do begin
      if S.Instruments.Noise[I].NoiseMacro[K] <> 0 then
        SV6.Instruments.Noise[I].SubpatternEnabled := True;
    end;
  end;

  SV6.Waves := S.Waves;

  SV6.TicksPerRow:=S.TicksPerRow;
  SV6.TimerEnabled := False;
  SV6.TimerDivider := 0;

  SV6.OrderMatrix:=S.OrderMatrix;

  SV6.Routines:=S.Routines;

  SV6.Patterns := TPatternMap.Create;
  // Update patterns to new format
  for I := 0 to S.Patterns.Count-1 do
    SV6.Patterns.Add(S.Patterns.Keys[I], ConvertPattern(S.Patterns.Data[I]));

  Result := UpgradeSong(SV6);
end;

function UpgradeSong(S: TSongV6): TSong;
var
  SV7: TSongV7;
begin
  SV7.Version := 7;

  SV7.Name := S.Name;
  SV7.Artist := S.Artist;
  SV7.Comment := S.Comment;

  SV7.Instruments := S.Instruments;
  SV7.Waves := S.Waves;

  SV7.TicksPerRow[0] := S.TicksPerRow;
  SV7.TicksPerRow[1] := S.TicksPerRow;
  SV7.TicksPerRow[2] := S.TicksPerRow;
  SV7.TicksPerRow[3] := S.TicksPerRow;
  SV7.PatternLength := Length(TPattern);

  SV7.TimerEnabled := S.TimerEnabled;
  SV7.TimerDivider := S.TimerDivider;

  SV7.Patterns := S.Patterns;
  SV7.PatternSets := TPatternSetMap.Create;

  SV7.Routines := S.Routines;

  SetOrderFromOrderMatrix(SV7, S.OrderMatrix, True);
  Result := SV7;
end;

function PatternSetExists(const S: TSong; PatternSetID: Integer): Boolean;
begin
  Result := Assigned(S.PatternSets) and
    (S.PatternSets.IndexOf(PatternSetID) <> -1);
end;

function GetPatternSet(const S: TSong; PatternSetID: Integer): TPatternSet;
var
  Index: Integer;
begin
  if not Assigned(S.PatternSets) then
    raise EPatternSetException.Create('The song has no pattern-set map.');

  Index := S.PatternSets.IndexOf(PatternSetID);
  if Index = -1 then
    raise EPatternSetException.CreateFmt('Pattern %d does not exist.',
      [PatternSetID]);

  Result := S.PatternSets.Data[Index];
end;

function EnsurePatternSet(var S: TSong; PatternSetID: Integer): TPatternSet;
var
  Channel: TChannel;
  PatternKey: Integer;
begin
  if PatternSetID < 0 then
    raise EPatternSetException.Create('Pattern numbers cannot be negative.');

  if PatternSetExists(S, PatternSetID) then
    Exit(GetPatternSet(S, PatternSetID));

  Result := Default(TPatternSet);
  for Channel := Low(TChannel) to High(TChannel) do begin
    PatternKey := S.Patterns.MaxKey;
    S.Patterns.GetOrCreateNew(PatternKey);
    Result.PatternKeys[Channel] := PatternKey;
  end;
  S.PatternSets.Add(PatternSetID, Result);
end;

function CreatePatternSet(var S: TSong): Integer;
begin
  Result := S.PatternSets.MaxKey;
  EnsurePatternSet(S, Result);
end;

function ClonePatternSet(var S: TSong; SourcePatternSetID: Integer): Integer;
var
  Channel: TChannel;
  SourceSet, DestSet: TPatternSet;
begin
  SourceSet := GetPatternSet(S, SourcePatternSetID);
  Result := CreatePatternSet(S);
  DestSet := GetPatternSet(S, Result);

  for Channel := Low(TChannel) to High(TChannel) do
    S.Patterns.KeyData[DestSet.PatternKeys[Channel]]^ :=
      S.Patterns.KeyData[SourceSet.PatternKeys[Channel]]^;
end;

procedure SetOrderFromOrderMatrix(var S: TSong; const OrderMatrix: TOrderMatrix;
  DetachSharedPatterns: Boolean);
type
  TLegacyPatternSet = record
    PatternSet: TPatternSet;
    PatternSetID: Integer;
  end;
var
  LegacyPatternSets: array of TLegacyPatternSet;
  UsedPatternKeys: array of Integer;
  Channel: TChannel;
  PatternSet, OwnedPatternSet: TPatternSet;
  OrderLength, OrderRow, I, PatternSetID, PatternKey, NewPatternKey: Integer;
  SourcePattern, DestPattern: PPattern;

  function PatternSetsEqual(const A, B: TPatternSet): Boolean;
  var
    C: TChannel;
  begin
    for C := Low(TChannel) to High(TChannel) do
      if A.PatternKeys[C] <> B.PatternKeys[C] then Exit(False);
    Result := True;
  end;

  function FindLegacyPatternSet(const APatternSet: TPatternSet): Integer;
  var
    J: Integer;
  begin
    for J := 0 to Length(LegacyPatternSets) - 1 do
      if PatternSetsEqual(LegacyPatternSets[J].PatternSet, APatternSet) then
        Exit(LegacyPatternSets[J].PatternSetID);
    Result := -1;
  end;

  function PatternKeyIsUsed(AKey: Integer): Boolean;
  var
    J: Integer;
  begin
    for J := 0 to Length(UsedPatternKeys) - 1 do
      if UsedPatternKeys[J] = AKey then Exit(True);
    Result := False;
  end;

  procedure MarkPatternKeyUsed(AKey: Integer);
  begin
    SetLength(UsedPatternKeys, Length(UsedPatternKeys) + 1);
    UsedPatternKeys[High(UsedPatternKeys)] := AKey;
  end;

begin
  LegacyPatternSets := nil;
  UsedPatternKeys := nil;
  S.PatternSets.Clear;
  SetLength(S.Order, 0);

  OrderLength := 0;
  for Channel := Low(TChannel) to High(TChannel) do
    OrderLength := Max(OrderLength,
      Max(Length(OrderMatrix[Ord(Channel)]) - 1, 0));

  SetLength(S.Order, OrderLength);
  for OrderRow := 0 to OrderLength - 1 do begin
    for Channel := Low(TChannel) to High(TChannel) do begin
      I := Ord(Channel);
      if OrderRow < Length(OrderMatrix[I]) - 1 then
        PatternSet.PatternKeys[Channel] := OrderMatrix[I, OrderRow]
      else
        PatternSet.PatternKeys[Channel] := 0;
    end;

    PatternSetID := FindLegacyPatternSet(PatternSet);
    if PatternSetID = -1 then begin
      PatternSetID := S.PatternSets.MaxKey;
      OwnedPatternSet := PatternSet;

      for Channel := Low(TChannel) to High(TChannel) do begin
        PatternKey := PatternSet.PatternKeys[Channel];
        SourcePattern := S.Patterns.GetOrCreateNew(PatternKey);

        if DetachSharedPatterns and PatternKeyIsUsed(PatternKey) then begin
          NewPatternKey := S.Patterns.MaxKey;
          DestPattern := S.Patterns.GetOrCreateNew(NewPatternKey);
          DestPattern^ := SourcePattern^;
          OwnedPatternSet.PatternKeys[Channel] := NewPatternKey;
        end;

        MarkPatternKeyUsed(OwnedPatternSet.PatternKeys[Channel]);
      end;

      S.PatternSets.Add(PatternSetID, OwnedPatternSet);
      SetLength(LegacyPatternSets, Length(LegacyPatternSets) + 1);
      LegacyPatternSets[High(LegacyPatternSets)].PatternSet := PatternSet;
      LegacyPatternSets[High(LegacyPatternSets)].PatternSetID := PatternSetID;
    end;

    S.Order[OrderRow] := PatternSetID;
  end;

  if Length(S.Order) = 0 then begin
    SetLength(S.Order, 1);
    S.Order[0] := CreatePatternSet(S);
  end;
end;

function BuildOrderMatrix(const S: TSong; OptimizePatterns: Boolean): TOrderMatrix;
var
  Channel: TChannel;
  OrderRow, PatternKey: Integer;
  PatternSet: TPatternSet;

  function FindMatchingPattern(AKey: Integer): Integer;
  var
    K, PatternIndex: Integer;
    Pattern: PPattern;
  begin
    PatternIndex := S.Patterns.IndexOf(AKey);
    if PatternIndex = -1 then
      raise EPatternSetException.CreateFmt(
        'Pattern set refers to nonexistent channel pattern %d.', [AKey]);

    Pattern := S.Patterns.Data[PatternIndex];
    for K := 0 to S.Patterns.Count - 1 do
      if CompareByte(S.Patterns.Data[K]^, Pattern^, SizeOf(TPattern)) = 0 then
        Exit(S.Patterns.Keys[K]);
    Result := AKey;
  end;
begin
  for Channel := Low(TChannel) to High(TChannel) do
    SetLength(Result[Ord(Channel)], Length(S.Order) + 1);

  for OrderRow := 0 to Length(S.Order) - 1 do begin
    PatternSet := GetPatternSet(S, S.Order[OrderRow]);
    for Channel := Low(TChannel) to High(TChannel) do begin
      PatternKey := PatternSet.PatternKeys[Channel];
      if OptimizePatterns then PatternKey := FindMatchingPattern(PatternKey);
      Result[Ord(Channel), OrderRow] := PatternKey;
    end;
  end;

  // hUGEDriver's existing order-table renderer expects one unused terminal
  // entry. Keep that compatibility detail out of the editor's order model.
  for Channel := Low(TChannel) to High(TChannel) do
    if Length(S.Order) > 0 then
      Result[Ord(Channel), Length(S.Order)] :=
        Result[Ord(Channel), Length(S.Order) - 1]
    else
      Result[Ord(Channel), 0] := 0;
end;

function OrderCount(const Song: TSong): Integer;
begin
  Result := Length(Song.Order);
end;

end.
