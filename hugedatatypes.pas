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

  TOrderMap = specialize TFPGMap<Integer, PPattern>;

  TSelection = array of array of TCell;

implementation

end.

