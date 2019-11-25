unit HugeDatatypes;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TCell = record
    Note: Integer;
    Instrument: Integer;
    EffectCode: Integer;
    EffectParams: Integer;
  end;

  TSelection = array of array of TCell;

implementation

end.

