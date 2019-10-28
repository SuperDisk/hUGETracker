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

  Bit = Boolean;
  TwoBits = 0..3;
  ThreeBits = 0..7;
  FourBits = 0..17;
  FiveBits = 0..31;
  SixBits = 0..63;
  SevenBits = 0..127;
  EightBits = Byte;

  TSweepRegister = bitpacked record
    case Boolean of
      True: (
        Shift: ThreeBits;
        IncDec: Bit;
        SweepTime: ThreeBits;
        _: Bit;
      );
      False: (ByteValue: Byte);
  end;

  TSquareLengthRegister = bitpacked record
    case Boolean of
    True: (
      Length: SixBits;
      Duty: TwoBits;
    );
    False: (ByteValue: Byte);
  end;

  TEnvelopeRegister = bitpacked record
    case Boolean of
      True: (
        SweepNumber: ThreeBits;
        Direction: Bit;
        InitialVolume: FourBits;
      );
      False: (ByteValue: Byte);
  end;

  TLowByteRegister = Byte;

  THighByteRegister = bitpacked record
    case Boolean of
      True: (
        FrequencyBits: ThreeBits;
        _Padding: ThreeBits;
        UseLength: Bit;
        Initial: Bit;
      );
      False: (ByteValue: Byte);
  end;

  TCh3SoundOnOffRegister = bitpacked record
    case Boolean of
      True: (
        _Padding: SevenBits;
        Playback: Bit;
      );
      False: (ByteValue: Byte);
  end;

  TCh3SoundLengthRegister = Byte;

  TCh3OutputLevelRegister = bitpacked record
    case Boolean of
      True: (
        _Padding: FiveBits;
        OutputLevel: TwoBits;
        _Padding2: Bit;
      );
      False: (ByteValue: Byte);
  end;

  TCh4SoundLengthRegister = 0..63;

  TPolynomialCounterRegister = bitpacked record
    case Boolean of
      True: (
        DividingRatio: ThreeBits;
        SevenBitCounter: Bit;
        ShiftClockFrequency: FourBits;
      );
      False: (ByteValue: Byte);
  end;

  TCh4HighByteRegister = bitpacked record
    case Boolean of
      True: (
        _Padding: SixBits;
        UseLength: Bit;
        Initial: Bit;
      );
      False: (ByteValue: Byte);
  end;

function NoiseInstrumentToRegisters(
  Initial: Boolean;
  Instr: TInstrument): TRegisters;

function SquareInstrumentToRegisters(
  Frequency: Word;
  Initial: Boolean;
  Instr: TInstrument): TRegisters;

implementation

function NoiseInstrumentToRegisters(
  Initial: Boolean;
  Instr: TInstrument): TRegisters;
var
  NR41: TCh4SoundLengthRegister;
  NR42: TEnvelopeRegister;
  NR43: TPolynomialCounterRegister;
  NR44: TCh4HighByteRegister;
begin
  NR41 := Default(TCh4SoundLengthRegister);
  NR42 := Default(TEnvelopeRegister);
  NR43 := Default(TPolynomialCounterRegister);
  NR44 := Default(TCh4HighByteRegister);

  NR41 := Instr.Length;

  NR42.InitialVolume := Instr.InitialVolume;
  NR42.SweepNumber := Instr.VolSweepAmount;
  NR42.Direction := Instr.VolSweepDirection = Up;

  NR43.DividingRatio:= Instr.DividingRatio;
  NR43.SevenBitCounter:=Instr.CounterStep = Seven;
  NR43.ShiftClockFrequency:= Instr.ShiftClockFreq;

  NR44.Initial:= True;
  NR44.UseLength:=Instr.LengthEnabled;

  Result.NR41 := NR41;
  Result.NR42 := NR42.ByteValue;
  Result.NR43 := NR43.ByteValue;
  Result.NR44 := NR44.ByteValue;

end;

function SquareInstrumentToRegisters(
  Frequency: Word;
  Initial: Boolean;
  Instr: TInstrument): TRegisters;
var
  NR10: TSweepRegister;
  NR11: TSquareLengthRegister;
  NR12: TEnvelopeRegister;
  NR13: TLowByteRegister;
  NR14: THighByteRegister;
begin
  NR10 := Default(TSweepRegister);
  NR11 := Default(TSquareLengthRegister);
  NR12 := Default(TEnvelopeRegister);
  NR13 := Default(TLowByteRegister);
  NR14 := Default(THighByteRegister);

  NR10.SweepTime := Instr.SweepTime;
  NR10.Shift := Instr.SweepShift;
  NR10.IncDec := Instr.SweepIncDec = Down;

  NR11.Duty := Instr.Duty;
  NR11.Length := Instr.Length;

  NR12.Direction := Instr.VolSweepDirection = Up;
  NR12.InitialVolume := Instr.InitialVolume;
  NR12.SweepNumber := Instr.VolSweepAmount;

  NR13 := Frequency and %11111111;

  NR14.FrequencyBits := (Frequency and %0000011100000000) shr 8;
  NR14.Initial := True;
  NR14.UseLength := Instr.LengthEnabled;

  Result.Type_ := Square;
  Result.NR10:=NR10.ByteValue;
  Result.NR11:=NR11.ByteValue;
  Result.NR12:=NR12.ByteValue;
  Result.NR13:=NR13;
  Result.NR14:=NR14.ByteValue;
end;

end.

