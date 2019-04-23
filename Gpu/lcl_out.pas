unit lcl_out;

{$mode delphi}

interface

uses
  Graphics, BGRABitmap;

procedure UpdateCanvas(Width, Height: Integer; Canvas: TCanvas);
procedure StartLCL;

implementation

uses
  BGRABitmapTypes,
  vars,
  classes;

var
  bmp: TBGRABitmap;

procedure StartLCL;
begin
  dx_r := $FF0000;
  dx_g := $00FF00;
  dx_b := $0000FF;

  //TODO: Move these somewhere better.
  dx_bits := 4;
  dx_pitch := 640;
  cmov := 160;

  dx_sr := finde(dx_r);
  dx_sg := finde(dx_g);
  dx_sb := finde(dx_b);
  dx_r := zaehle(dx_r);
  dx_g := zaehle(dx_g);
  dx_b := zaehle(dx_b);
end;

//TODO: Screen tearing issues-- probably double buffer related
procedure UpdateCanvas(Width, Height: Integer; Canvas: TCanvas);
var
  stretched: TBGRABitmap;
  X, Y: Integer;
  N: Integer;
  p: PBGRAPixel;
  Idx: Integer;
begin
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

