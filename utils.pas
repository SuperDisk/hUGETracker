unit Utils;

{$mode objfpc}

interface

uses
  Classes, SysUtils, instruments;

type
  TRegisters = record
    case Type_: TInstrumentType of
      Noise: (NR41, NR42, NR43, NR44: Byte);
  end;

function BuildEnvelope(
  InitialVol: TEnvelopeVolume;
  Direction: TSweepType;
  Amount: TEnvelopeSweepAmount): Byte;

function BuildHighByte(
  Initial: Boolean;
  LengthEnabled: Boolean;
  Frequency: Integer): Byte; overload;

function BuildHighByte(
  Initial: Boolean;
  LengthEnabled: Boolean): Byte; overload;

function NoiseInstrumentToRegisters(
  Initial: Boolean;
  Instr: TInstrument): TRegisters;

implementation

function BuildEnvelope(
  InitialVol: TEnvelopeVolume;
  Direction: TSweepType;
  Amount: TEnvelopeSweepAmount): Byte;
var
  DirectionNumber: Integer;
begin
  if Direction = Up then DirectionNumber := 1
  else DirectionNumber := 0;
  Result := (InitialVol shl 4);
  Result := Result or (DirectionNumber shl 2);
  Result := Result or Amount;
end;

function BuildHighByte(
  Initial: Boolean;
  LengthEnabled: Boolean;
  Frequency: Integer): Byte;
begin
  Result := 0;
  if Initial then Result := %10000000;
  if LengthEnabled then Result := Result or %01000000;
  Result := Result or Frequency;
end;

function BuildHighByte(
  Initial: Boolean;
  LengthEnabled: Boolean): Byte;
begin
  BuildHighByte(Initial, LengthEnabled, 0);
end;

function NoiseInstrumentToRegisters(
  Initial: Boolean;
  Instr: TInstrument): TRegisters;
function BuildPolynomialCounter(
  ShiftClockFreq: Integer;
  CounterStep: TStepWidth;
  DividingRatio: Integer): Byte;
begin
  Result := (ShiftClockFreq shl 4);
  if CounterStep = Seven then
    Result := Result or %00001000;
  Result := Result or DividingRatio;
end;

begin
  with Instr do
  begin
    Result.NR41 := Length;
    Result.NR42 := BuildEnvelope(InitialVolume, VolSweepDirection, VolSweepAmount);
    Result.NR43 := BuildPolynomialCounter(ShiftClockFreq, CounterStep, DividingRatio);
    Result.NR44 := BuildHighByte(Initial, LengthEnabled);
  end;
end;

end.

