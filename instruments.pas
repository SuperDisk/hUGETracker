unit instruments;

{$mode objfpc}

interface

uses
  Classes, SysUtils;

type
  TInstrumentType = (Square, Wave, Noise);
  TDutyType = 0..3;
  TSweepType = (Up, Down);
  TStepWidth = (Fifteen, Seven);
  TEnvelopeVolume = 0..63;
  TEnvelopeSweepAmount = 0..7;

  TInstrument = record
    Type_: TInstrumentType;

    Name: String;
    Length: Integer;
    // Highmask
    LengthEnabled: Boolean;

    InitialVolume: TEnvelopeVolume;
    VolSweepDirection: TSweepType;
    VolSweepAmount: TEnvelopeSweepAmount;

    // Square
    // NR10
    SweepTime: Integer;
    SweepIncDec: TSweepType;
    SweepShift: Integer;

    // NR11
    Duty: TDutyType;

    // Wave

    // NR32
    OutputLevel: Integer;
    // Wave
    Waveform: Integer;

    // Noise
    // NR42
    ShiftClockFreq: Integer;
    CounterStep: TStepWidth;
    DividingRatio: Integer;
  end;

implementation

end.

