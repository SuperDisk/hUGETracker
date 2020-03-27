unit Utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Song, Waves, HugeDatatypes, Constants, gHashSet, fgl, instruments;

type

  { TIntegerHash }

  TIntegerHash = class
    class function hash(I: Integer; N: Integer): Integer;
  end;

  TUsedMap = specialize THashSet<Integer, TIntegerHash>;

  { TSongUsageReport }

  TSongUsageReport = record
    UsedInstruments: TUsedMap;
    UsedRoutines: TUsedMap;
    UsedPatterns: TUsedMap;

    {UnusedInstruments: TUsedMap;
    UnusedRoutines: TUsedMap;}
    UnusedPatterns: TUsedMap;
  end;

function Lerp(v0, v1, t: Double): Double;
function ConvertWaveform(Waveform: TWave): T4bitWave;
procedure BlankPattern(Pat: PPattern);
procedure BlankCell(var Cell: TCell);
function EffectCodeToStr(Code: Integer; Params: TEffectParams): String;
function EffectToExplanation(Code: Integer; Params: TEffectParams): String;

function ModInst(Inst: Integer): Integer;
function UnmodInst(Bank: TInstrumentType; Inst: Integer): Integer;
function InstBankName(Bank: TInstrumentType): String;

function GetUsageReport(Song: TSong): TSongUsageReport;
procedure FreeUsageReport(Report: TSongUsageReport);

implementation

{ TIntegerHash }

class function TIntegerHash.hash(I: Integer; N: Integer): Integer;
begin
  Result := I mod N;
end;

function Lerp(v0, v1, t: Double): Double;
begin
  Result := ((1 - t) * v0) + (t * v1);
end;

function ConvertWaveform(Waveform: TWave): T4bitWave;
var
  I: Integer;
  J: Integer;
begin
  for I := 0 to 15 do begin
    J := I*2;
    Result[I] := (Waveform[J] shl 4) or (Waveform[J+1]);
  end;
end;

procedure BlankPattern(Pat: PPattern);
var
  I: Integer;
begin
  for I := 0 to High(Pat^) do begin
    Pat^[I] := Default(TCell);
    Pat^[I].Note := NO_NOTE;
  end;
end;

procedure BlankCell(var Cell: TCell);
begin
  Cell := Default(TCell);
  Cell.Note := NO_NOTE;
end;

function EffectCodeToStr(Code: Integer; Params: TEffectParams): String;
begin
  Result := HexStr((Code shl 8) or Params.Value, 3);
end;

function EffectToExplanation(Code: Integer; Params: TEffectParams): String;
var
  P: String;
begin
  Result := 'No explanation';
  P := IntToStr(Params.Value);
  case Code of
    $0: Result := 'Arpeggiate by +'+IntToStr(Params.Param1)+', +'+IntToStr(Params.Param2)+' semitones';
    $1: Result := 'Slide up by '+P+' units';
    $2: Result := 'Slide down by '+P+' units';
    $3: Result := 'Tone portamento by '+P+' units';
    $5: Result := 'Set Left speaker vol to '+IntToStr(Params.Param1)+', Right speaker vol to '+IntToStr(Params.Param2);
    $6: Result := 'Call routine #'+P;
    $7: Result := 'Delay note by '+P+' ticks';
    $A: Result := 'Increase volume by '+IntToStr(Params.Param1)+' units, decrease volume by '+IntToStr(Params.Param2)+' units';
    $B: Result := 'Jump to order '+P;
    $C: Result := 'Set volume to '+P+'/15';
    $D: Result := 'Jump to row '+P+' on the next pattern';
    $E: Result := 'Cut note after '+P+' ticks';
    $F: Result := 'Set speed to '+P+' ticks';
  end;
end;

function ModInst(Inst: Integer): Integer;
begin
  Result := ((Inst-1) mod 15)+1;
end;

function UnmodInst(Bank: TInstrumentType; Inst: Integer): Integer;
begin
  case Bank of
    itSquare: Result := Inst+(15*0);
    itWave: Result := Inst+(15*1);
    itNoise: Result := Inst+(15*2);
  end;
end;

function InstBankName(Bank: TInstrumentType): String;
begin
  case Bank of
    itSquare: Result := 'Square';
    itWave: Result := 'Wave';
    itNoise: Result := 'Noise';
  end;
end;

function GetUsageReport(Song: TSong): TSongUsageReport;
var
  I, J, X, Y: Integer;
  Pat: PPattern;
begin
  Result.UsedPatterns := TUsedMap.Create;
  Result.UsedInstruments := TUsedMap.Create;
  Result.UsedRoutines := TUsedMap.Create;
  Result.UnusedPatterns := TUsedMap.Create;
  {Result.UnusedInstruments := TUsedMap.Create;
  Result.UnusedRoutines := TUsedMap.Create;}

  for X := Low(Song.OrderMatrix) to High(Song.OrderMatrix) do
    for Y := Low(Song.OrderMatrix[X]) to High(Song.OrderMatrix[X]) do
      Result.UsedPatterns.insert(Song.OrderMatrix[X, Y]);

  for I := 0 to Song.Patterns.Count-1 do begin
    if not Result.UsedPatterns.contains(Song.Patterns.Keys[I])
      then Continue;

    Pat := Song.Patterns[Song.Patterns.Keys[I]];

    for J := Low(Pat^) to High(Pat^) do
      with Pat^[J] do begin
        if Instrument <> 0 then Result.UsedInstruments.insert(Instrument);
        if EffectCode = $6 then // Call routine
          Result.UsedRoutines.insert(EffectParams.Value);
      end;
  end;

  for X := 0 to Song.Patterns.Count-1 do
    if not Result.UsedPatterns.contains(Song.Patterns.Keys[X]) then
      Result.UnusedPatterns.insert(Song.Patterns.Keys[X]);

  {for I := Low(TInstrumentIndex) to High(TInstrumentIndex) do
    Result.UnusedInstruments.insert(I);

  for I in Result.UsedInstruments.Iterator do
    Result.UnusedInstruments.delete(I);

  for I := Low(TRoutineIndex) do High(TRoutineIndex) do
    Result.UnusedRoutines.insert(I);

  for I in Result.UsedRoutines do
    Result.UnusedRoutines.delete(I);}
end;

procedure FreeUsageReport(Report: TSongUsageReport);
begin
  Report.UsedPatterns.Free;
  Report.UsedInstruments.Free;
  Report.UsedRoutines.Free;

  Report.UnusedPatterns.Free;
  //Report.UnusedInstruments.Free;
  //Report.UnusedRoutines.Free;
end;

end.

