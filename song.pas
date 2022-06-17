unit Song;

{$mode objfpc}{$H+}

// TODO: Lots of duplicated code in here. At some point this should switch over
// to a more parsable format, like NBT, JSON, or XML or something.

interface

uses Classes, HugeDatatypes, instruments, Constants, math, sysutils;

type
  ESongVersionException = class(Exception);

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

  { TSong }

  TSong = TSongV6;

procedure WriteSongToStream(S: TStream; const ASong: TSong);
procedure ReadSongFromStream(S: TStream; out ASong: TSong);
procedure InitializeSong(out S: TSong);
procedure LoadDefaultInstruments(var S: TSong);
procedure DestroySong(var S: TSong);

function UpgradeSong(S: TSongV1): TSong; overload;
function UpgradeSong(S: TSongV2): TSong; overload;
function UpgradeSong(S: TSongV3): TSong; overload;
function UpgradeSong(S: TSongV4): TSong; overload;
//function UpgradeSong(S: TSongV5): TSong; overload;

function OptimizeSong(const S: TSong): TSong;
function PatternIsUsed(Idx: Integer; const Song: TSong): Boolean;

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

procedure WriteSongToStream(S: TStream; const ASong: TSong);
var
  i, n: Integer;
begin
  // Write the fixed record elements first
  n := SizeOf(TSong)
     - SizeOf(TPatternMap)
     - SizeOf(TOrderMatrix)
     - SizeOf(TRoutineBank);
  S.Write(ASong, n);

  // Write the pattern count
  S.Write(ASong.Patterns.Count, SizeOf(Integer));
  // Write the patterns
  for i := 0 to ASong.Patterns.Count-1 do
  begin
    S.Write(ASong.Patterns.Keys[i], SizeOf(Integer));
    S.Write(ASong.Patterns.Data[i]^, SizeOf(TPattern));
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
  SV4: TSongV4;
  SV5: TSongV5;
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
      ReadSongFromStreamV6(S, ASong);
    end;
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

  for I := Low(TOrderMatrix) to High(TOrderMatrix) do begin
    SetLength(S.OrderMatrix[I], 2);
    S.OrderMatrix[I, 0] := I;
  end;

  for I := Low(TRoutineBank) to High(TRoutineBank) do
    S.Routines[I] := '';

  S.TicksPerRow := 7;
  S.TimerDivider := 0;
  S.TimerEnabled := False;
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
        writeln(StdErr, '[DEBUG] Note value ', Pat[I].Note, ' not found.');
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
  I: Integer;

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
  // TODO: Port over noise macro
  for I := Low(S.Instruments.Noise) to High(S.Instruments.Noise) do begin
    SV6.Instruments.Noise[I].Subpattern := ConvertNoiseMacro(S.Instruments.Noise[I].NoiseMacro);
    SV6.Instruments.Noise[I].SubpatternEnabled := True;
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

  Result := SV6;
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
    for J := Low(Result.OrderMatrix[I]) to High(Result.OrderMatrix[I])-1 do begin
      if Result.Patterns.IndexOf(Result.OrderMatrix[I, J]) = -1 then
        WriteLn(StdErr, '[ERROR] Nonexistent pattern number in order table: ', Result.OrderMatrix[I, J], '!!!');
      Result.OrderMatrix[I, J] := FindMatchingPattern(Result.Patterns.GetOrCreateNew(Result.OrderMatrix[I, J])^);
    end;
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

