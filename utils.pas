unit Utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Waves, HugeDatatypes, Constants;

type
  { TOrderMapHelper }

  TOrderMapHelper = class helper for TPatternMap
    function GetOrCreateNew(Key: Integer): PPattern;
    function CreateNewPattern(Key: Integer): PPattern;
    function MaxKey: Integer;
  end;

function Lerp(v0, v1, t: Double): Double;
function ConvertWaveform(Waveform: TWave): T4bitWave;
procedure BlankPattern(Pat: PPattern);
procedure BlankCell(var Cell: TCell);
function EffectCodeToStr(Code: Integer; Params: TEffectParams): String;
function EffectToExplanation(Code: Integer; Params: TEffectParams): String;

implementation

{ TOrderMapHelper }

function TOrderMapHelper.GetOrCreateNew(Key: Integer): PPattern;
begin
  if Self.IndexOf(Key) <> -1 then
    Result := Self.KeyData[Key]
  else begin
    New(Result);
    BlankPattern(Result);
    Self.Add(Key, Result);
  end;
end;

function TOrderMapHelper.CreateNewPattern(Key: Integer): PPattern;
begin
  if IndexOf(Key) <> -1 then Exit(KeyData[Key]);

  New(Result);
  BlankPattern(Result);
  Self.Add(Key, Result);
end;

function TOrderMapHelper.MaxKey: Integer;
var
  X: Integer;
begin
  Result := 0;
  for X := 0 to Self.Count-1 do
    if Self.Keys[X] > Result then Result := Self.Keys[X];
  Inc(Result);
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

end.

