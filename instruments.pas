unit instruments;

{$mode objfpc}

interface

uses
  Classes, SysUtils;

{TInstrument = class
  Length: Integer;
  LengthEnabled: Integer;

  EnvInitialVolume: Integer;
  EnvDirection: TSweepType;
  EnvAmount: Integer;
end;

TSquareInstrument = class(TInstrument)
  // NR10
  SweepTime: Integer;
  SweepIncDec: TSweepType;
  SweepShift: Integer;

  // NR11
  Duty: TDutyType;
end;

TWaveInstrument = class(TInstrument)
  // NR32
  OutputLevel: Integer;
  // Wave
  Waveform: Integer;
end;

TNoiseInstrument = class(TInstrument)
  // NR42
  ShiftClockFreq: Integer;
  CounterStep: TStepWidth;
  DividingRatio: Integer;
end;}

type
  TInstrumentType = (Square, Wave, Noise);
  TDutyType = 0..3;
  TSweepType = (Up, Down);
  TStepWidth = (Fifteen, Seven);
  TEnvelopeVolume = 0..63;
  TEnvelopeSweepAmount = 0..7;

  TInstrument = record
    Name: String;
    Length: Integer;
    // Highmask
    LengthEnabled: Boolean;

    InitialVolume: TEnvelopeVolume;
    VolSweepDirection: TSweepType;
    VolSweepAmount: TEnvelopeSweepAmount;

    case Type_: TInstrumentType of
      Square: (
        // NR10
        SweepTime: Integer;
        SweepIncDec: TSweepType;
        SweepShift: Integer;

        // NR11
        Duty: TDutyType;
      );
      Wave: (
        // NR32
        OutputLevel: Integer;
        // Wave
        Waveform: Integer;
      );
      Noise: (
        // NR42
        ShiftClockFreq: Integer;
        CounterStep: TStepWidth;
        DividingRatio: Integer;
      );
  end;

implementation

end.

