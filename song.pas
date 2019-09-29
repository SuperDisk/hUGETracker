unit Song;

{$mode delphi}

interface

uses instruments;

type
  TSong = record
    Name: String;
    Artist: String;
    Comment: String;

    Instruments: array[1..15] of TInstrument;
  end;

implementation

end.

