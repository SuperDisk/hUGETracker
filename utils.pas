unit Utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Waves, HugeDatatypes, Constants;

type
  { TOrderMapHelper }

  TOrderMapHelper = class helper for TPatternMap
    function GetOrCreateNew(Key: Integer): PPattern;
    procedure CreateNewPattern(Key: Integer);
    function MaxKey: Integer;
  end;

function ConvertWaveform(Waveform: TWave): T4bitWave;
procedure BlankPattern(Pat: PPattern);
procedure BlankCell(var Cell: TCell);
function EffectCodeToStr(Code: Integer; Params: TEffectParams): String;

implementation

{ TOrderMapHelper }

function TOrderMapHelper.GetOrCreateNew(Key: Integer): PPattern;
var
  NewPat: PPattern;
begin
  if Self.IndexOf(Key) <> -1 then
    Result := Self.KeyData[Key]
  else begin
    New(NewPat);
    BlankPattern(NewPat);
    Self.Add(Key, NewPat);
    Result := NewPat;
  end;
end;

procedure TOrderMapHelper.CreateNewPattern(Key: Integer);
var
  NewPat: PPattern;
begin
  if IndexOf(Key) <> -1 then Exit;

  New(NewPat);
  BlankPattern(NewPat);
  Self.Add(Key, NewPat);
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

end.

