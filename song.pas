unit Song;

{$mode delphi}

interface

uses instruments, waves;

type
  TSong = record
    Name: String;
    Artist: String;
    Comment: String;

    Instruments: array[1..15] of TInstrument;
    Waves: array[0..15] of TWave;
  end;

implementation

end.

