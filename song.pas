unit Song;

{$mode delphi}

interface

uses Classes, HugeDatatypes, instruments;

const
  CURRENT_VERSION = 2;

type
  TSongV1 = packed record
    Version: Integer;

    Name: ShortString;
    Artist: ShortString;
    Comment: ShortString;

    Instruments: TInstrumentBank;
    Waves: TWaveBank;

    TicksPerRow: Integer;

    Patterns: TPatternMap;
    OrderMatrix: TOrderMatrix;
  end;

  TSongV2 = packed record
    Version: Integer;

    Name: ShortString;
    Artist: ShortString;
    Comment: ShortString;

    Instruments: TInstrumentBank;
    Waves: TWaveBank;

    TicksPerRow: Integer;

    Patterns: TPatternMap;
    OrderMatrix: TOrderMatrix;

    Routines: TRoutineBank;
  end;

  { TSong }

  TSong = TSongV2;

procedure WriteSongToStream(S: TStream; const ASong: TSong);
procedure ReadSongFromStream(S: TStream; out ASong: TSong);
procedure InitializeSong(var S: TSong);
procedure DestroySong(var S: TSong);

function UpgradeSong(S: TSongV1): TSong; overload;
function UpgradeSong(S: TSongV2): TSong; overload;

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

  // ASong.Patterns.Clear;

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

  // ASong.Patterns.Clear;

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
  n := SizeOf(TSongV2)
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
  Version: Integer = 1;
  SV1: TSongV1;
begin
  S.Read(Version, SizeOf(Integer));
  S.Seek(0, soBeginning);
  case Version of
    0..1: begin
      ReadSongFromStreamV1(S, SV1);
      ASong := UpgradeSong(SV1);
    end;
    2: ReadSongFromStreamV2(S, ASong);
  end;
end;

procedure InitializeSong(var S: TSong);
var
  I, J: Integer;
begin
  with S do begin
    Version := CURRENT_VERSION;
    Name := '';
    Artist := '';
    Comment := '';
  end;
  for I := Low(S.Instruments) to High(S.Instruments) do
    with S.Instruments[I] do begin
      Type_ := Square;
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
  for I := Low(S.Waves) to High(S.Waves) do begin
    for J := 0 to 32 do
      S.Waves[I][J] := random($F);
  end;

  for I := Low(TOrderMatrix) to High(TOrderMatrix) do begin
    SetLength(S.OrderMatrix[I], 2);
    S.OrderMatrix[I, 0] := I;
  end;

  S.TicksPerRow := 7;
  S.Patterns := TPatternMap.Create;
end;

procedure DestroySong(var S: TSong);
var
  I: Integer;
begin
  S.Patterns.Free;
  for I := Low(TOrderMatrix) to High(TOrderMatrix) do
    SetLength(S.OrderMatrix[I], 0);
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
begin
  Result := S;
end;

end.

