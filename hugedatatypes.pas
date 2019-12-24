unit HugeDatatypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, instruments, waves;

type
  Nibble = $0..$F;

  TInstrumentBank = array[1..15] of TInstrument;
  TWaveBank = array[0..15] of TWave;

  TEffectParams = bitpacked record
    case Boolean of
      True: (Param2, Param1: Nibble);
      False: (Value: Word);
  end;

  TCell = record
    Note: Integer;
    Instrument: Integer;
    EffectCode: Integer;
    EffectParams: TEffectParams;
  end;

  TPattern = array[0..63] of TCell;
  PPattern = ^TPattern;

  TPatternMap = specialize TFPGMap<Integer, PPattern>;
  TOrderMatrix = array[0..3] of array of Integer;

  TSelection = array of array of TCell;

implementation

end.

