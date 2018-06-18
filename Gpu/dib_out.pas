{+-----------------------------------------------------------------------------+
 | Author:      Christian Hackbart
 | Description: DirectDIB :) display functions
 | Copyright (c) 2001 Christian Hackbart
 | Stand: 18.1.2001
 |
 | Bug: - The Display is flipped?
 |      - 8 & 16 bit are not working properly
 |      - horribly sloooooooooooooooow uhhhh what a mess!!
 | http://www.tu-ilmenau.de/~hackbart
 +----------------------------------------------------------------------------+}

unit dib_out;

{$MODE Delphi}

interface uses windows;

procedure start_Dib(Handle:Hwnd;Bits:Byte);
procedure UpdateDIB(Handle:Hwnd);

implementation uses vars,ddraw_out;

type
     TPalBitmapInfo=record
      bmiHeader:TBitmapInfoHeader;
      bmiColors:Array[0..15] of Word;
     end;

var DibInfo:TPalBitmapInfo;


procedure start_Dib(Handle:Hwnd;Bits:Byte);
var i:byte;
begin
  frames:=0;
  with DIBInfo do begin
  with bmiHeader do begin
    bisize:=sizeof(bmiheader);
    biwidth:=160;
    biheight:=144;
    biplanes:=1;{4}
    bibitcount:=bits;
    bicompression:=BI_RGB;
    bisizeimage:=0;
    bixpelspermeter:=0;
    biypelspermeter:=0;
    biclrused:=16; //colors in bmiColors
    biclrimportant:=0;
   end;
   for i:=0 to 15 do bmiColors[i]:=i;
  end;

  {!!!!}

  case dibinfo.bmiHeader.biBitCount of
   8: begin
       dx_r:=0;
       dx_g:=0;
       dx_b:=0;
      end;
   16: begin
        dx_r:=$F800;
        dx_g:=$07e0;
        dx_b:=$001f;
       end;
   32: begin
        dx_r:=$FF0000;
        dx_g:=$00FF00;
        dx_b:=$0000FF;
      end;
  end;

  dx_bits:=dibinfo.bmiHeader.biBitCount div 8;
  dx_pitch:=dx_bits*DIBInfo.bmiHeader.biWidth;
  cmov:=dx_pitch div 4;

  dx_sr:=finde(dx_r);
  dx_sg:=finde(dx_g);
  dx_sb:=finde(dx_b);
  dx_r:=zaehle(dx_r);
  dx_g:=zaehle(dx_g);
  dx_b:=zaehle(dx_b);

end;

procedure UpdateDIB(Handle:HWnd);
 var
  DC:HDC;
  destRect:TRect;

begin
 if skip_asm then exit;

 DC:=GetDC(Handle);
 MyGetClientRect(Handle, destRect);

 StretchDIBits(
  DC,	// handle of device context
  destrect.left,     // x-coordinate of upper-left corner of dest. rect.
  destrect.top,	     // y-coordinate of upper-left corner of dest. rect.
  destrect.Right,    // width of destination rectangle
  destrect.Bottom,   // height of destination rectangle
  0,	// x-coordinate of upper-left corner of source rect.
  0,	// y-coordinate of upper-left corner of source rect.
  160,	// width of source rectangle
  144,	// height of source rectangle
  @dx_buffer,	// address of bitmap bits
  pBitmapInfo(@DIBInfo)^,	// address of bitmap data
  dib_rgb_colors,	// usage
  SRCCOPY 	// raster operation code
  );
  ReleaseDC(Handle,DC);
end;

end.
