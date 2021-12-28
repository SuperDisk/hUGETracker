unit HugeDatatypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type
  Nibble = $0..$F;

  TRoutine = string;

  TEffectParams = bitpacked record
    case Boolean of
      True: (Param2, Param1: Nibble);
      False: (Value: Byte);
  end;

  TCellV1 = packed record
    Note: Integer;
    Instrument: Integer;
    EffectCode: Integer;
    EffectParams: TEffectParams;
  end;

  TCellV2 = packed record
    Note: Integer;
    Instrument: Integer;
    Volume: Integer;
    EffectCode: Integer;
    EffectParams: TEffectParams;
  end;

  TCell = TCellV2;
  PCell = ^TCell;

  TPatternV1 = packed array[0..63] of TCellV1;
  PPatternV1 = ^TPatternV1;

  TPatternV2 = packed array[0..63] of TCellV2;
  TPattern = TPatternV2;
  PPattern = ^TPatternV2;

  TPatternMapV1 = specialize TFPGMap<Integer, PPatternV1>;
  TPatternMap = class(specialize TFPGMap<Integer, PPattern>)
    function GetOrCreateNew(Key: Integer): PPattern;
    function MaxKey: Integer;

    procedure DeletePattern(Key: Integer);

    destructor Destroy; override;
  end;

  TWaveV1 = packed array[0..32] of Byte;
  TWaveV2 = packed array[0..31] of Byte;
  TWave = TWaveV2;
  T4bitWave = packed array[0..15] of Byte;

  TInstrumentType = (itSquare = 0, itWave = 1, itNoise = 2);
  TDutyType = 0..3;
  TSweepType = (stUp, stDown);
  TStepWidth = (swFifteen, swSeven);
  TEnvelopeVolume = 0..15;
  TEnvelopeSweepAmount = 0..7;
  TNoiseMacro = array[0..5] of -31..32;

  TInstrumentV1 = packed record
    Type_: TInstrumentType;

    Name: ShortString;
    Length: Integer;
    // Highmask
    LengthEnabled: Boolean;

    InitialVolume: TEnvelopeVolume;
    VolSweepDirection: TSweepType;
    VolSweepAmount: TEnvelopeSweepAmount;

    // itSquare
    // NR10
    SweepTime: Integer;
    SweepIncDec: TSweepType;
    SweepShift: Integer;

    // NR11
    Duty: TDutyType;

    // itWave

    // NR32
    OutputLevel: Integer;
    // itWave
    Waveform: Integer;

    // itNoise
    // NR42
    ShiftClockFreq: Integer; // Unused
    CounterStep: TStepWidth;
    DividingRatio: Integer;
  end;

  TInstrumentV2 = packed record
    Type_: TInstrumentType;

    Name: ShortString;
    Length: Integer;
    // Highmask
    LengthEnabled: Boolean;

    InitialVolume: TEnvelopeVolume;
    VolSweepDirection: TSweepType;
    VolSweepAmount: TEnvelopeSweepAmount;

    // itSquare
    // NR10
    SweepTime: Integer;
    SweepIncDec: TSweepType;
    SweepShift: Integer;

    // NR11
    Duty: TDutyType;

    // itWave

    // NR32
    OutputLevel: Integer;
    // itWave
    Waveform: Integer;

    // itNoise
    // NR42
    ShiftClockFreq: Integer; // Unused
    CounterStep: TStepWidth;
    DividingRatio: Integer;
    NoiseMacro: TNoiseMacro;
  end;

  TInstrumentV3 = packed record
    Type_: TInstrumentType;

    Name: ShortString;
    Length: Integer;
    // Highmask
    LengthEnabled: Boolean;

    InitialVolume: TEnvelopeVolume;
    VolSweepDirection: TSweepType;
    VolSweepAmount: TEnvelopeSweepAmount;

    // itSquare
    // NR10
    SweepTime: Integer;
    SweepIncDec: TSweepType;
    SweepShift: Integer;

    // NR11
    Duty: TDutyType;

    // itWave

    // NR32
    OutputLevel: Integer;
    // itWave
    Waveform: Integer;

    // itNoise
    // NR42
    CounterStep: TStepWidth;
    Subpattern: TPattern;
  end;

  TInstrument = TInstrumentV3;

  TAsmInstrument = array[0..3] of Byte;

  TRegisters = record
    case Type_: TInstrumentType of
      itSquare: (NR10, NR11, NR12, NR13, NR14: Byte);
      itWave: (NR30, NR31, NR32, NR33, NR34: Byte);
      itNoise: (NR41, NR42, NR43, NR44: Byte);
  end;

  Bit = Boolean;
  TwoBits = 0..%11;
  ThreeBits = 0..%111;
  FourBits = 0..%1111;
  FiveBits = 0..%11111;
  SixBits = 0..%111111;
  SevenBits = 0..%1111111;
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

  TInstrumentBankV1 = packed array[1..15] of TInstrumentV1;
  TInstrumentCollectionV1 = packed record
    case Boolean of
      False: (Duty, Wave, Noise: TInstrumentBankV1);
      True: (All: packed array[1..45] of TInstrumentV1);
  end;

  TInstrumentBankV2 = packed array[1..15] of TInstrumentV2;
  TInstrumentCollectionV2 = packed record
    case Boolean of
      False: (Duty, Wave, Noise: TInstrumentBankV2);
      True: (All: packed array[1..45] of TInstrumentV2);
  end;

  TInstrumentBankV3 = packed array[1..15] of TInstrumentV3;
  TInstrumentCollectionV3 = packed record
    case Boolean of
      False: (Duty, Wave, Noise: TInstrumentBankV3);
      True: (All: packed array[1..45] of TInstrumentV3);
  end;

  TInstrumentBank = TInstrumentBankV3;
  TInstrumentCollection = TInstrumentCollectionV3;

  TWaveBankV1 = packed array[0..15] of TWaveV1;
  TWaveBankV2 = packed array[0..15] of TWaveV2;
  TWaveBank = TWaveBankV2;
  TRoutineBank = packed array[0..15] of TRoutine;

  TOrderMatrix = packed array[0..3] of array of Integer;

  TCellPart = (
    cpNote = 0,
    cpInstrument = 1,
    cpVolume = 2,
    cpEffectCode = 3,
    cpEffectParams = 4
  );

  TSelectionPos = record
    X, Y: Integer;
    SelectedPart: TCellPart
  end;

  TSelectedCell = record
    Cell: TCell;
    Parts: set of TCellPart;
  end;

  TSelectionRow = array of TSelectedCell;
  TSelection = array of TSelectionRow;

operator > (L, R: TSelectionPos): Boolean;
operator < (L, R: TSelectionPos): Boolean;
operator >= (L, R: TSelectionPos): Boolean;
operator <= (L, R: TSelectionPos): Boolean;
operator = (L, R: TSelectionPos): Boolean;

procedure IncSelectionPos(var SP: TSelectionPos);
procedure DecSelectionPos(var SP: TSelectionPos);

const
  DefaultWaves: array[0..10] of TWave =
    (($0,$0,$0,$0,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f),
     ($0,$0,$0,$0,$0,$0,$0,$0,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f),
     ($0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f),
     ($0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$f,$f,$f,$f,$f,$f,$f,$f),
     ($0,$0,$0,$1,$1,$2,$2,$3,$3,$4,$4,$5,$5,$6,$6,$7,$7,$8,$8,$9,$9,$a,$a,$b,$b,$c,$c,$d,$d,$e,$e,$f),
     ($f,$e,$d,$c,$b,$a,$9,$8,$7,$6,$5,$4,$3,$2,$1,$0,$1,$2,$3,$4,$5,$6,$7,$8,$9,$a,$b,$c,$d,$e,$f,$f),
     ($7,$a,$c,$d,$d,$b,$7,$5,$2,$1,$1,$3,$6,$8,$b,$d,$d,$c,$9,$7,$4,$1,$0,$1,$4,$7,$9,$c,$d,$d,$b,$8),
     ($0,$f,$0,$f,$0,$f,$0,$f,$0,$f,$0,$f,$0,$f,$0,$f,$0,$f,$0,$f,$0,$f,$0,$f,$0,$f,$0,$f,$0,$f,$0,$f),
     ($f,$e,$f,$c,$f,$a,$f,$8,$f,$6,$f,$4,$f,$2,$f,$0,$f,$2,$f,$4,$f,$6,$f,$8,$f,$a,$f,$c,$f,$e,$f,$f),
     ($f,$e,$d,$d,$c,$c,$b,$b,$a,$a,$9,$9,$8,$8,$7,$7,$8,$a,$b,$d,$f,$1,$2,$4,$5,$7,$8,$a,$b,$d,$e,$e),
     ($8,$4,$1,$1,$6,$1,$e,$d,$5,$7,$4,$7,$5,$a,$a,$d,$c,$e,$a,$3,$1,$7,$7,$9,$d,$d,$2,$0,$0,$3,$4,$7));

implementation

uses Utils;

operator>(L, R: TSelectionPos): Boolean;
begin
  if L.X > R.X then
    Result := True
  else if (L.X = R.X) and (L.SelectedPart > R.SelectedPart) then
    Result := True
  else
    Result := False;
end;

operator<(L, R: TSelectionPos): Boolean;
begin
  if L.X < R.X then
    Result := True
  else if (L.X = R.X) and (L.SelectedPart < R.SelectedPart) then
    Result := True
  else
    Result := False;
end;

operator>=(L, R: TSelectionPos): Boolean;
begin
  Result := (L > R) or (L = R);
end;

operator<=(L, R: TSelectionPos): Boolean;
begin
  Result := (L < R) or (L = R);
end;

operator=(L, R: TSelectionPos): Boolean;
begin
  Result := (L.X = R.X) and (L.SelectedPart = R.SelectedPart);
end;

procedure IncSelectionPos(var SP: TSelectionPos);
begin
  if SP.SelectedPart = High(TCellPart) then begin
    SP.SelectedPart := Low(TCellPart);
    Inc(SP.X);
  end
  else Inc(SP.SelectedPart);
end;

procedure DecSelectionPos(var SP: TSelectionPos);
begin
  if SP.SelectedPart = Low(TCellPart) then begin
    SP.SelectedPart := High(TCellPart);
    Dec(SP.X);
  end
  else Dec(SP.SelectedPart);
end;

{ TPatternMap }

function TPatternMap.GetOrCreateNew(Key: Integer): PPattern;
begin
  if Self.IndexOf(Key) <> -1 then
    Result := Self.KeyData[Key]
  else begin
    New(Result);
    BlankPattern(Result);
    Self.Add(Key, Result);
  end;
end;

function TPatternMap.MaxKey: Integer;
var
  X: Integer;
begin
  Result := 0;
  for X := 0 to Self.Count-1 do
    if Self.Keys[X] > Result then Result := Self.Keys[X];
  Inc(Result);
end;

procedure TPatternMap.DeletePattern(Key: Integer);
begin
  Dispose(KeyData[Key]);
  Self.Remove(Key);
end;

destructor TPatternMap.Destroy;
var
  I: Integer;
begin
  for I := 0 to Self.Count-1 do
    Dispose(Self.Data[I]);

  inherited;
end;

end.

