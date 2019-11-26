unit HugeDatatypes;

{$mode delphi}

interface

uses
  Classes, SysUtils;

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

  TSelection = array of array of TCell;

implementation

end.

