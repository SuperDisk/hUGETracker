unit Utils;

{$mode objfpc}

interface

uses
  Classes, SysUtils, instruments;

type
  TRegisters = record
    case Type_: TInstrumentType of
      Square: (NR10, NR11, NR12, NR13, NR14: Byte);
      Wave: (NR30, NR31, NR32, NR33, NR34: Byte);
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

function SquareInstrumentToRegisters(
  Frequency: Word;
  Initial: Boolean;
  Instr: TInstrument): TRegisters;

implementation

function BuildEnvelope(
  InitialVol: TEnvelopeVolume;
  Direction: TSweepType;
  Amount: TEnvelopeSweepAmount): Byte;
begin
  Result := (InitialVol shl 4);
  if Direction = Up then
    Result := Result or %00001000;
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
  Result := Result or ((Frequency and %0000011100000000) shr 8);
end;

function BuildHighByte(
  Initial: Boolean;
  LengthEnabled: Boolean): Byte;
begin
  Result := BuildHighByte(Initial, LengthEnabled, 0);
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

function SquareInstrumentToRegisters(
  Frequency: Word;
  Initial: Boolean;
  Instr: TInstrument): TRegisters;

function BuildSweep(
  SweepTime: Integer;
  SweepIncDec: TSweepType;
  SweepShift: Integer): Byte;
begin
  Result := (SweepTime shl 4);
  if SweepIncDec = Down then
      Result := Result or %00001000;
  Result := Result or SweepShift;
end;

function BuildLengthDuty(
  Duty: TDutyType;
  Length: Integer): Byte;
begin
  Result := Duty shl 6;
  Result := Result or Length;
end;

begin
  with Instr do begin
      Result.NR10 := BuildSweep(SweepTime, SweepIncDec, SweepShift);
      Result.NR11 := BuildLengthDuty(Duty, Length);
      Result.NR12 := BuildEnvelope(InitialVolume, VolSweepDirection, VolSweepAmount);
      Result.NR13 := (Frequency and %0000000011111111);
      Result.NR14 := BuildHighByte(Initial, LengthEnabled, Frequency);
  end;
end;

end.

