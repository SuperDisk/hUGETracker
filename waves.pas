unit Waves;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TWaveV1 = packed array[0..32] of Byte;
  TWaveV2 = packed array[0..31] of Byte;
  TWave = TWaveV2;
  T4bitWave = packed array[0..15] of Byte;

implementation

end.

