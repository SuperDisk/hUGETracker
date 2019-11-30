unit HugeDatatypes;

{$mode objfpc}

interface

uses
  Classes, SysUtils, fgl;

type
  TEffectParams = packed record
    case Boolean of
      True: (Param1, Param2: Byte);
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

