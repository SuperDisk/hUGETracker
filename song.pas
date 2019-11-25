unit Song;

{$mode delphi}

interface

uses instruments, waves, patterns;

type

  { TSong }

  TSong = record
    Name: ShortString;
    Artist: ShortString;
    Comment: ShortString;

    Instruments: array[1..15] of TInstrument;
    Waves: array[0..15] of TWave;

    Patterns: array of TPattern;
    OrderMatrix: array of integer;

    TicksPerRow: Integer;
  end;

implementation

end.

