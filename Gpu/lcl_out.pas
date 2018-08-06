unit lcl_out;

{$mode delphi}

interface

uses
  Graphics, BGRABitmap;

procedure UpdateCanvas(Width, Height: Integer; Canvas: TCanvas);

implementation

uses
  BGRABitmapTypes,
  vars, ddraw_out,
  classes;

var
  bmp: TBGRABitmap;

//TODO: Screen tearing issues-- probably double buffer related
procedure UpdateCanvas(Width, Height: Integer; Canvas: TCanvas);
var
  stretched: TBGRABitmap;
  X, Y: Integer;
  N: Integer;
  p: PBGRAPixel;
  Idx: Integer;
begin
  if skip_asm then
    exit;

  p := bmp.Data;

  for Y := 144-1 downto 0 do
    for X := 0 to 160-1 do
      begin
        Idx := ((Y*160)+X)*4;
        p^.red := dx_buffer[Idx + 0];
        p^.blue := dx_buffer[Idx + 1];
        p^.green := dx_buffer[idx + 2];
        inc(p);
      end;
  bmp.InvalidateBitmap;

  //bmp.Draw(Canvas, 0, 0);
  Canvas.StretchDraw(Rect(0, 0, Width, Height), bmp.Bitmap);
  //stretched := bmp.Resample(Canvas.Width, Canvas.Height, rmSimpleStretch) as TBGRABitmap;
  //stretched.Draw(Canvas,0,0);
  //stretched.Free;
end;

begin
  bmp := TBGRABitmap.Create(160, 144);
end.

