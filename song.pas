unit Song;

{$mode delphi}

interface

uses HugeDatatypes;

type
  { TSong }

  TSong = record
    Name: ShortString;
    Artist: ShortString;
    Comment: ShortString;

    Instruments: TInstrumentBank;
    Waves: TWaveBank;

    Patterns: TPatternMap;
    OrderMatrix: TOrderMatrix;

    TicksPerRow: Integer;
  end;

implementation

end.

