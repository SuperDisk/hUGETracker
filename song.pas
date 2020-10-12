unit Song;

{$mode objfpc}{$H+}

// TODO: Lots of duplicated code in here. At some point this should switch over
// to a more parsable format, like NBT, JSON, or XML or something.

interface

uses Classes, HugeDatatypes, instruments, Constants, waves, math;

type
  TSongV1 = packed record
    Version: Integer;

    Name: ShortString;
    Artist: ShortString;
    Comment: ShortString;

    Instruments: TInstrumentBankV1;
    Waves: TWaveBankV1;

    TicksPerRow: Integer;

    Patterns: TPatternMap;
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

    Patterns: TPatternMap;
    OrderMatrix: TOrderMatrix;

    Routines: TRoutineBank;
  end;

  TSongV3 = packed record
    Version: Integer;

    Name: ShortString;
    Artist: ShortString;
    Comment: ShortString;

    Instruments: TInstrumentCollectionV1;
    Waves: TWaveBankV2;

    TicksPerRow: Integer;

    Patterns: TPatternMap;
    OrderMatrix: TOrderMatrix;

    Routines: TRoutineBank;
  end;

  TSongV4 = packed record
    Version: Integer;

    Name: ShortString;
    Artist: ShortString;
    Comment: ShortString;

    Instruments: TInstrumentCollectionV2;
    Waves: TWaveBankV2;

    TicksPerRow: Integer;

    Patterns: TPatternMap;
    OrderMatrix: TOrderMatrix;

    Routines: TRoutineBank;
  end;

  { TSong }

  TSong = TSongV4;

procedure WriteSongToStream(S: TStream; const ASong: TSong);
procedure ReadSongFromStream(S: TStream; out ASong: TSong);
procedure InitializeSong(var S: TSong);
procedure LoadDefaultInstruments(var S: TSong);
procedure DestroySong(var S: TSong);

function UpgradeSong(S: TSongV1): TSong; overload;
function UpgradeSong(S: TSongV2): TSong; overload;
function UpgradeSong(S: TSongV3): TSong; overload;
function UpgradeSong(S: TSongV4): TSong; overload;

function OptimizeSong(const S: TSong): TSong;
function PatternIsUsed(Idx: Integer; const Song: TSong): Boolean;

implementation

// Thanks to WP on the FreePascal forums for this code!
// https://forum.lazarus.freepascal.org/index.php/topic,47892.msg344152.html#msg344152

procedure ReadSongFromStreamV1(S: TStream; out ASong: TSongV1);
var
  i, n: Integer;
  pat: PPattern;
begin
  // Read the fixed elements first
  n := SizeOf(TSongV1) - SizeOf(TPatternMap) - SizeOf(TOrderMatrix);
  S.Read(ASong, n);

  // Create the patterns
  ASong.Patterns := TPatternMap.Create;
  // Read the pattern count
  S.Read(n, SizeOf(Integer));
  for i:=0 to n - 1 do begin
    // Allocate memory for each pattern ...
    New(pat);
    // and read the pattern content
    S.Read(pat^, SizeOf(TPattern));
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
  pat: PPattern;
begin
  // Read the fixed elements first
  n := SizeOf(TSongV2)
     - SizeOf(TPatternMap)
     - SizeOf(TOrderMatrix)
     - SizeOf(TRoutineBank);

  S.Read(ASong, n);

  // Create the patterns
  ASong.Patterns := TPatternMap.Create;
  // Read the pattern count
  S.Read(n, SizeOf(Integer));
  for i:=0 to n - 1 do begin
    // Allocate memory for each pattern ...
    New(pat);
    // and read the pattern content
    S.Read(pat^, SizeOf(TPattern));
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
  pat: PPattern;
begin
  // Read the fixed elements first
  n := SizeOf(TSongV3)
     - SizeOf(TPatternMap)
     - SizeOf(TOrderMatrix)
     - SizeOf(TRoutineBank);

  S.Read(ASong, n);

  // Create the patterns
  ASong.Patterns := TPatternMap.Create;
  // Read the pattern count
  S.Read(n, SizeOf(Integer));
  for i:=0 to n - 1 do begin
    // Allocate memory for each pattern ...
    New(pat);
    // and read the pattern content
    S.Read(pat^, SizeOf(TPattern));
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
  pat: PPattern;
begin
  // Read the fixed elements first
  n := SizeOf(TSongV4)
     - SizeOf(TPatternMap)
     - SizeOf(TOrderMatrix)
     - SizeOf(TRoutineBank);

  S.Read(ASong, n);

  // Create the patterns
  ASong.Patterns := TPatternMap.Create;
  // Read the pattern count
  S.Read(n, SizeOf(Integer));
  for i:=0 to n - 1 do begin
    // Allocate memory for each pattern ...
    New(pat);
    // and read the pattern content
    S.Read(pat^, SizeOf(TPattern));
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

procedure WriteSongToStream(S: TStream; const ASong: TSong);
var
  i, n: Integer;
  pPat: PPattern;
begin
  // Write the fixed record elements first
  n := SizeOf(TSongV4)
     - SizeOf(TPatternMap)
     - SizeOf(TOrderMatrix)
     - SizeOf(TRoutineBank);
  S.Write(ASong, n);

  // Write the pattern count
  S.Write(ASong.Patterns.Count, SizeOf(Integer));
  // Write the patterns
  for i := 0 to ASong.Patterns.Count-1 do
  begin
    pPat := ASong.Patterns[i];
    S.Write(pPat^, SizeOf(pPat^));
  end;

  // Write the OrderMatrix arrays
  for i := 0 to 3 do
  begin
    n := Length(ASong.OrderMatrix[i]);
    S.Write(n, SizeOf(Integer));
    S.Write(ASong.OrderMatrix[i][0], n*SizeOf(Integer));
  end;

  // Write the routines
  for i := Low(TRoutineBank) to High(TRoutineBank) do
    S.WriteAnsiString(ASong.Routines[i]);
end;

procedure ReadSongFromStream(S: TStream; out ASong: TSong);
var
  Version: Integer;
  SV1: TSongV1;
  SV2: TSongV2;
  SV3: TSongV3;
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
    4: ReadSongFromStreamV4(S, ASong);
  end;
end;

procedure InitializeSong(var S: TSong);
var
  I, J: Integer;
begin
  with S do begin
    Version := UGE_FORMAT_VERSION;
    Name := '';
    Artist := '';
    Comment := '';
  end;

  for I := Low(S.Instruments.All) to High(S.Instruments.All) do
    S.Instruments.All[I] := Default(TInstrument);

  for I := Low(S.Instruments.Duty) to High(S.Instruments.Duty) do begin
    with S.Instruments.Duty[I] do begin
      Type_ := itSquare;
      Length := 0;
      LengthEnabled := False;
      InitialVolume := High(TEnvelopeVolume);
      VolSweepDirection := Down;
      VolSweepAmount := 0;

      SweepTime := 0;
      SweepIncDec := Down;
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
      VolSweepDirection := Down;
      VolSweepAmount := 0;
      NoiseMacro := Default(TNoiseMacro);
      ShiftClockFreq := 0;
      DividingRatio := 0;
      CounterStep := swFifteen;
    end;

  for I := Low(S.Waves) to High(S.Waves) do begin
    for J := Low(TWave) to High(TWave) do
      S.Waves[I][J] := random($F);
  end;

  for I := Low(TOrderMatrix) to High(TOrderMatrix) do begin
    SetLength(S.OrderMatrix[I], 2);
    S.OrderMatrix[I, 0] := I;
  end;

  S.TicksPerRow := 7;
  S.Patterns := TPatternMap.Create;
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
var
  I: Integer;
begin
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
      VolSweepDirection := Down;
      VolSweepAmount := 0;

      SweepTime := 0;
      SweepIncDec := Down;
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
      VolSweepDirection := Down;
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

  procedure ConvertPattern(var Pat: TPattern);
  var
    I: Integer;
    Regs: TRegisters;
    PolyCounter: TPolynomialCounterRegister absolute Regs.NR43;
    Ch4Freq: Integer;
    RealR: Double;
  begin
    for I := Low(Pat) to High(Pat) do begin
      if (Pat[I].Instrument = 0) or (Pat[I].Note = NO_NOTE) then Continue;

      Regs := NoiseInstrumentToRegisters(NotesToFreqs.KeyData[Pat[I].Note], False, SV4.Instruments.Noise[Pat[I].Instrument]);
      if PolyCounter.DividingRatio = 0 then
        RealR := 0.5
      else
        RealR := PolyCounter.DividingRatio;

      Ch4Freq := Trunc((524288 / RealR) / 2**(PolyCounter.ShiftClockFrequency+1));
      if not Ch4FreqToNoteCodeMap.TryGetData(Ch4Freq, Pat[I].Note) then
        writeln('[DEBUG] Note value ', Pat[I].Note, ' not found.');
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

  // Rewrite noise patterns to accomodate the new noise instruments...
  for I := 0 to SV4.Patterns.Count-1 do
    if UsedInCH4(SV4.Patterns.Keys[I]) then begin
      ConvertPattern(SV4.Patterns.Data[I]^);
    end;

  Result := UpgradeSong(SV4);
end;

function UpgradeSong(S: TSongV4): TSong;
begin
  Result := S;
end;

function OptimizeSong(const S: TSong): TSong;
var
  I, J: Integer;

  function FindMatchingPattern(const P: TPattern): Integer;
  var
    K: Integer;
  begin
    for K := 0 to S.Patterns.Count-1 do
      if CompareByte(S.Patterns.KeyData[S.Patterns.Keys[K]]^, P, SizeOf(TPattern)) = 0 then
        Exit(S.Patterns.Keys[K]);
  end;
begin
  Result := S;

  // Uniquify the order matrix (so modifying the original doesn't affect this one)
  for I := Low(Result.OrderMatrix) to High(Result.OrderMatrix) do
    SetLength(Result.OrderMatrix[I], Length(Result.OrderMatrix[I]));

  // De-duplicate order matrix such that only unique numbers remain
  for I := Low(Result.OrderMatrix) to High(Result.OrderMatrix) do
    for J := Low(Result.OrderMatrix[I]) to High(Result.OrderMatrix[I])-1 do
      Result.OrderMatrix[I, J] := FindMatchingPattern(Result.Patterns[Result.OrderMatrix[I, J]]^);
end;

function PatternIsUsed(Idx: Integer; const Song: TSong): Boolean;
var
  I, J: Integer;
begin
  for I := Low(Song.OrderMatrix) to High(Song.OrderMatrix) do
    for J := Low(Song.OrderMatrix[I]) to High(Song.OrderMatrix[I])-1 do
      if Song.OrderMatrix[I, J] = Idx then Exit(True);

  Result := False;
end;

end.

