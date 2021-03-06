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

const
  DefaultWaves: array[0..10] of TWave =
    (($0,$0,$0,$0,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f),
     ($0,$0,$0,$0,$0,$0,$0,$0,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f),
     ($0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f),
     ($0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$f,$f,$f,$f,$f,$f,$f,$f),
     ($0,$0,$0,$1,$1,$2,$2,$3,$3,$4,$4,$5,$5,$6,$6,$7,$7,$8,$8,$9,$9,$a,$a,$b,$b,$c,$c,$d,$d,$e,$e,$f),
     ($f,$e,$d,$c,$b,$a,$9,$8,$7,$6,$5,$4,$3,$2,$1,$0,$1,$2,$3,$4,$5,$6,$7,$8,$9,$a,$b,$c,$d,$e,$f,$f),
     ($7,$a,$c,$d,$d,$b,$7,$5,$2,$1,$1,$3,$6,$8,$b,$d,$d,$c,$9,$7,$4,$1,$0,$1,$4,$7,$9,$c,$d,$d,$b,$8),
     ($0,$f,$0,$f,$0,$f,$0,$f,$0,$f,$0,$f,$0,$f,$0,$f,$0,$f,$0,$f,$0,$f,$0,$f,$0,$f,$0,$f,$0,$f,$0,$f),
     ($f,$e,$f,$c,$f,$a,$f,$8,$f,$6,$f,$4,$f,$2,$f,$0,$f,$2,$f,$4,$f,$6,$f,$8,$f,$a,$f,$c,$f,$e,$f,$f),
     ($f,$e,$d,$d,$c,$c,$b,$b,$a,$a,$9,$9,$8,$8,$7,$7,$8,$a,$b,$d,$f,$1,$2,$4,$5,$7,$8,$a,$b,$d,$e,$e),
     ($8,$4,$1,$1,$6,$1,$e,$d,$5,$7,$4,$7,$5,$a,$a,$d,$c,$e,$a,$3,$1,$7,$7,$9,$d,$d,$2,$0,$0,$3,$4,$7));

implementation

end.

