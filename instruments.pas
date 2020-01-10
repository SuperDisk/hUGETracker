unit instruments;

{$mode objfpc}{$H+}

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

  TInstrument = packed record
    Type_: TInstrumentType;

    Name: ShortString;
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
    ShiftClockFreq: Integer; // Unused
    CounterStep: TStepWidth;
    DividingRatio: Integer;
  end;

  TAsmInstrument = array[0..3] of Byte;

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
  Frequency: Word;
  Initial: Boolean;
  Instr: TInstrument): TRegisters;

function SquareInstrumentToRegisters(
  Frequency: Word;
  Initial: Boolean;
  Instr: TInstrument): TRegisters;

function WaveInstrumentToRegisters(
  Frequency: Word;
  Initial: Boolean;
  Instr: TInstrument): TRegisters;

function InstrumentToBytes(Instrument: TInstrument): TAsmInstrument;

implementation

function SquareInstrumentToBytes(Instrument: TInstrument): TAsmInstrument;
var
  Regs: TRegisters;
begin
  Regs := SquareInstrumentToRegisters(0, True, Instrument);
  Result[0] := Regs.NR10;
  Result[1] := Regs.NR11;
  Result[2] := Regs.NR12;
  Result[3] := Regs.NR14
end;

function WaveInstrumentToBytes(Instrument: TInstrument): TAsmInstrument;
var
  Regs: TRegisters;
begin
  Regs := WaveInstrumentToRegisters(0, True, Instrument);
  Result[0] :=Regs.NR31;
  Result[1] :=Regs.NR32;
  Result[2] :=Instrument.Waveform;
  Result[3] :=Regs.NR34
end;

function NoiseInstrumentToBytes(Instrument: TInstrument): TAsmInstrument;
var
  Regs: TRegisters;
begin
  Regs := NoiseInstrumentToRegisters(0, True, Instrument);
  Result[0] := Regs.NR41;
  Result[1] := Regs.NR42;
  Result[2] := Regs.NR43 and %00001111; // remove shift clock freq
  Result[3] := Regs.NR44;
end;

function InstrumentToBytes(Instrument: TInstrument): TAsmInstrument;
begin
  case Instrument.Type_ of
    Square: Result := SquareInstrumentToBytes(Instrument);
    Wave: Result := WaveInstrumentToBytes(Instrument);
    Noise: Result := NoiseInstrumentToBytes(Instrument);
  end;
end;

function WaveInstrumentToRegisters(
  Frequency: Word;
  Initial: Boolean;
  Instr: TInstrument): TRegisters;
var
  NR30: TCh3SoundOnOffRegister;
  NR31: TCh3SoundLengthRegister;
  NR32: TCh3OutputLevelRegister;
  NR33: TLowByteRegister;
  NR34: THighByteRegister;
begin
  NR30 := Default(TCh3SoundOnOffRegister);
  NR31 := Default(TCh3SoundLengthRegister);
  NR32 := Default(TCh3OutputLevelRegister);
  NR33 := Default(TLowByteRegister);
  NR34 := Default(THighByteRegister);

  NR30.Playback := True;

  NR31 := Instr.Length;

  NR32.OutputLevel := Instr.OutputLevel;

  NR33 := Frequency and %11111111;

  NR34.Initial := Initial;
  NR34.FrequencyBits := (Frequency and %0000011100000000) shr 8;
  NR34.UseLength := Instr.LengthEnabled;

  Result.Type_ := Wave;
  Result.NR30 := NR30.ByteValue;
  Result.NR31 := NR31;
  Result.NR32 := NR32.ByteValue;
  Result.NR33 := NR33;
  Result.NR34 := NR34.ByteValue;
end;

function NoiseInstrumentToRegisters(
  Frequency: Word;
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

  NR43.ShiftClockFrequency := $F - (Frequency shr 7); // Quantize CH4 note

  NR43.DividingRatio:= Instr.DividingRatio;
  NR43.SevenBitCounter:=Instr.CounterStep = Seven;

  NR44.Initial:= Initial;
  NR44.UseLength:=Instr.LengthEnabled;

  Result.Type_ := Noise;
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
  NR14.Initial := Initial;
  NR14.UseLength := Instr.LengthEnabled;

  Result.Type_ := Square;
  Result.NR10:=NR10.ByteValue;
  Result.NR11:=NR11.ByteValue;
  Result.NR12:=NR12.ByteValue;
  Result.NR13:=NR13;
  Result.NR14:=NR14.ByteValue;
end;

end.

