unit Utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Song, HugeDatatypes, Constants, gHashSet, fgl, instruments, math;

type
  TSubpatternBytes = array[0..((3*32)-1)] of Byte;

function Lerp(v0, v1, t: Double): Double;
function Snap(Value, Every: Integer): Integer;
function ReMap(Value, Istart, Istop, Ostart, Ostop: Double): Double;
function IntReMap(Value, Istart, Istop, Ostart, Ostop: Integer): Integer;
function ConvertWaveform(Waveform: TWave): T4bitWave;
function UnconvertWaveform(Waveform: T4bitWave): TWave;
procedure BlankPattern(Pat: PPattern);
procedure BlankCell(var Cell: TCell);
function EffectCodeToStr(Code: Integer; Params: TEffectParams): String;
function EffectToExplanation(Code: Integer; Params: TEffectParams): String;

procedure DN(Note: Integer; Instrument: Integer; Effect: Integer; out B1, B2, B3: Byte);
function SubpatternToBytes(Pat: TPattern): TSubpatternBytes;

function ModInst(Inst: Integer): Integer;
function UnmodInst(Bank: TInstrumentType; Inst: Integer): Integer;
function InstBankName(Bank: TInstrumentType): String;

implementation

function Lerp(v0, v1, t: Double): Double;
begin
  Result := ((1 - t) * v0) + (t * v1);
end;

function Snap(Value, Every: Integer): Integer;
begin
  Result := Trunc(Value/Every)*Every;
end;

function ReMap(Value, Istart, Istop, Ostart, Ostop: Double): Double;
begin
  Result := ostart + ((ostop - ostart) * ((value - istart) / (istop - istart)));
end;

function IntReMap(Value, Istart, Istop, Ostart, Ostop: Integer): Integer;
begin
  Result := Trunc(ReMap(Value, Istart, Istop, Ostart, Ostop));
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

function UnconvertWaveform(Waveform: T4bitWave): TWave;
var
  I: Integer;
begin
  for I := Low(T4BitWave) to High(T4BitWave) do begin
    Result[I*2] := hi(Waveform[I]);
    Result[(I*2)+1] := lo(Waveform[I]);
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
    $C: begin
      if (Params.Value < 16) then Result := 'Keep envelope, Set volume to '+IntToStr(Params.Param2)+'/15'
      else if (Params.Value < 128) then Result := 'Envelope Down '+IntToStr(Params.Param1)+'/64Hz, Set volume to '+IntToStr(Params.Param2)+'/15'
      else if (Params.Value < 144) then Result := 'Envelope Off, Set volume to '+IntToStr(Params.Param2)+'/15'
      else Result := 'Envelope Up '+IntToStr(Params.Param1-8)+'/64Hz, Set volume to '+IntToStr(Params.Param2)+'/15'
    end;
    $D: Result := 'Jump to row '+P+' on the next pattern';
    $E: Result := 'Cut note after '+P+' ticks';
    $F: Result := 'Set speed to '+P+' ticks';
  end;
end;

procedure DN(Note: Integer; Instrument: Integer; Effect: Integer; out B1, B2, B3: Byte);
begin
  B1 := Byte(Note or ((Instrument and $10) shl 3));
  B2 := Byte((((Instrument shl 4) and $FF) or (Effect shr 8)));
  B3 := Byte(Effect and $FF);
end;

function SubpatternToBytes(Pat: TPattern): TSubpatternBytes;
var
  I: Integer;
  B1, B2, B3: Byte;
begin
  for I := 0 to 31 do begin
    with Pat[I] do begin
      DN(Note, IfThen(I = 31, 1, Volume.Value), (EffectCode shl 8) or EffectParams.Value, B1, B2, B3);
      Result[(I*3) + 0] := B1;
      Result[(I*3) + 1] := B2;
      Result[(I*3) + 2] := B3;
    end;
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

end.

