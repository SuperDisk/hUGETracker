unit Song;

{$mode delphi}

interface

uses instruments, waves, patterns, HugeDatatypes;

type
  { TSong }

  TSong = record
    Name: ShortString;
    Artist: ShortString;
    Comment: ShortString;

    Instruments: TInstrumentBank;
    Waves: TWaveBank;

    Patterns: array of TPattern;
    OrderMatrix: array of integer;

    TicksPerRow: Integer;
  end;

implementation

end.

