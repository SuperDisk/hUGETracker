{+-----------------------------------------------------------------------------+
 | Author:      Christian Hackbart
 | Description: DirectDraw display functions
 | Copyright (c) 2000 Christian Hackbart
 | Stand: 31.10.2000
 |
 | http://www.tu-ilmenau.de/~hackbart
 +----------------------------------------------------------------------------+}

unit ddraw_out;

{$MODE Delphi}

interface uses windows,graphics;

const frameskip:Byte=1;
      screen_updated:DWord=0;
      StatBottom:Byte=0;
      Frames:Byte=0;

function InitFail(awnd:HWND;hRet:HRESULT;szError:LPCTSTR):HResult;
procedure UpdateDDraw(handle:HWND);
procedure ReleaseAllObjects;

function start_dx(Handle:HWND):integer;


function skip_asm:Boolean;
procedure MyGetClientRect(Handle:Hwnd;var rect:Trect);

implementation uses DirectDraw,vars,sysutils;
const
    g_pDD:IDIRECTDRAW4=NIL;
    g_pDDSPrimary:IDIRECTDRAWSURFACE4=NIL;
    g_pDDSOne:IDIRECTDRAWSURFACE4 =NIL;
    g_pClipper:IDIRECTDRAWCLIPPER =NIL;
    g_pDDPal:IDIRECTDRAWPALETTE =NIL;

procedure ReleaseAllObjects;
begin
if assigned(g_pDD) then
 begin
  g_pDDSPrimary:=NIL;
  g_pDDSOne:=NIL;
  g_pDDPal:=NIL;
  g_pDD:=NIL;
 end;
end;

function InitFail(awnd:HWND;hRet:HRESULT;szError:LPCTSTR):HResult;
begin
 ReleaseAllObjects;
 MessageBox(aWnd,szerror,'DDraw error',MB_OK);
 DestroyWindow(aWnd);
 result:=hRet;
end;

function skip_asm:Boolean;assembler;
asm
 inc screen_updated

 mov al,frames
 inc al
 cmp al,frameskip
 jl @ende
 xor al,al
 @ende:
 mov frames,al
 mov result,al
end;

procedure MyGetClientRect(Handle:Hwnd;var rect:Trect);
var count: integer;
begin
 getclientrect(handle,rect);
 count:=rect.Bottom-STATbottom;
 if count>0 then rect.Bottom:=count else
                 rect.bottom:=rect.top;
end;

procedure UpdateDDraw(Handle:Hwnd);
var
 rcRect,destRect:TRect;
 hRet:HRESULT;
 pt:TPOINT;
 ddsd:TDDSURFACEDESC2;

begin
 if skip_asm then exit;

 rcRect.left :=0;
 rcRect.top := 0;
 rcRect.right := 160;
 rcRect.bottom := 144;
 MyGetClientRect(Handle, destRect);

// destRect :=rcRect;
 {+-------------------------------------------------------+
  | Testen, ob wir überhaupt etwas sehen, wenn nicht dann |
  | zeichnen wir nix !!!                                  |
  +-------------------------------------------------------+}

 with destrect do if (Right=left) or (top=bottom) then exit;

 pt.x:=0;pt.y:=0;
 ClientToScreen(Handle,pt);
 OffsetRect(destRect,pt.x,pt.y);
 g_pClipper.SetHWnd(0,Handle);
 zeromemory(@ddsd,sizeof(ddsd));
 ddsd.dwSize:=sizeof(ddsd);
 g_pDDSOne.Lock(NIL,ddsd,DDLOCK_WAIT or DDLOCK_SURFACEMEMORYPTR,0);

 if ddsd.lpSurface<>NIL then
  move(dx_buffer[0],ddsd.lpSurface^,ddsd.lPitch*36 shl 2);

 g_pDDSOne.Unlock(NIL);
 hRet:=g_pDDSPrimary.Blt(@destRect,g_pDDSOne,@rcRect,0,NIL);
//hRet:=g_pDDSPrimary.BltFast(0,0,g_pDDSOne,@RcRect,0);//@destRect,g_pDDSOne,@rcRect,0,NIL);
 case hRet of
   DD_OK: exit;
   DDERR_SURFACELOST: begin
                       while (g_pDDSPrimary._Restore<>DD_OK) and (g_pDDSOne._Restore<>DD_OK) do;
                       initfail(handle,hret,pchar(dderrorstring(hret)+#0));
                       exit;
                      end;
   else begin
          initfail(handle,hret,pchar(dderrorstring(hret)+#0));
          exit;
        end
 end;

end;


function start_dx(Handle:HWND):integer;
var
  pdd:IDIRECTDRAW;
  ddsd3:TDDSURFACEDESC2;
  ddsd:TDDSURFACEDESC2;
  ddsd2:TDDSURFACEDESC2;
  hret:hresult;
begin
 frames:=0;

  hRet:=DirectDrawCreate(NIL,pDD,nil);
  if hRet<>DD_OK then
    begin
     result:=InitFail(handle,hRet,'DirectDrawCreate FAILED');
     exit;
    end;
  hRet:=pDD.QueryInterface(IID_IDirectDraw4,g_pDD);
  if hRet<>DD_OK then
   begin
    result:=InitFail(handle,hRet,'QueryInterface FAILED');
    exit;
   end;
  hRet:=g_pDD.SetCooperativeLevel(handle,DDSCL_NORMAL);
  if hRet<>DD_OK then
  begin
   result:=InitFail(handle,hRet,'SetCooperativeLevel FAILED');
   exit;
  end;
  ZeroMemory(@ddsd,sizeof(ddsd));
  ddsd.dwSize:=sizeof(ddsd);
  ddsd.dwFlags:=DDSD_CAPS;
  ddsd.ddsCaps.dwCaps:=DDSCAPS_PRIMARYSURFACE;
  hRet:=g_pDD.CreateSurface(ddsd,g_pDDSPrimary,nil);
  if hRet<>DD_OK then
  begin
   result:=InitFail(handle,hRet,'CreateSurface FAILED');
   exit;
  end;
  hRet:=g_pDD.CreateClipper(0,g_pClipper,nil);
  if hRet<>DD_OK then
  begin
   result:=InitFail(handle,hRet,'CreateClipper FAILED');
   exit;
  end;
  hRet:=g_pClipper.SetHWnd(0,handle);
  if hRet<>DD_OK then
   begin
    result:=InitFail(handle,hRet,'SetHWnd FAILED');
    exit;
   end;
  hRet:=g_pDDSPrimary.SetClipper(g_pClipper);
  if hRet<>DD_OK then
  begin
   result:=InitFail(handle,hRet,'SetClipper FAILED');
   exit;
  end;
  ZeroMemory(@ddsd3, sizeof(ddsd));
  ddsd3.dwSize:=sizeof(ddsd);
  ddsd3.dwFlags:=DDSD_CAPS or DDSD_HEIGHT or DDSD_WIDTH;
  ddsd3.ddsCaps.dwCaps:=DDSCAPS_OFFSCREENPLAIN;
  ddsd3.dwWidth:=160;
  ddsd3.dwHeight:=200;
  if(g_pDD.CreateSurface(ddsd3,g_pDDSOne,nil)<>DD_OK) then
   begin
    result:=InitFail(handle,hRet,'CBuffer FAILED');
    exit;
  end;
  zeromemory(@ddsd2,sizeof(ddsd2));
  ddsd2.dwSize:=sizeof(ddsd2);
  ddsd2.dwFlags:=DDSD_PIXELFORMAT ;
  Sleep(500);//*MAGIC*
  hret:=g_pDDSOne.Lock(nil,ddsd2,DDLOCK_SURFACEMEMORYPTR or DDLOCK_WAIT,0);
  if hret<>0 then
   begin
    result:=initfail(handle,hret,pchar(dderrorstring(hret)+#0));
    exit;
   end;
  dx_bits:=ddsd2.ddpfPixelFormat.dwRGBBitCount;
  dx_r:=ddsd2.ddpfPixelFormat.dwRBitMask;
  dx_g:=ddsd2.ddpfPixelFormat.dwGBitMask;
  dx_b:=ddsd2.ddpfPixelFormat.dwBBitMask;
  dx_pitch:=ddsd2.lPitch;
  cmov:=ddsd2.lPitch div 4;
  dx_bits:=dx_bits div 8;
  dx_sr:=finde(dx_r);
  dx_sg:=finde(dx_g);
  dx_sb:=finde(dx_b);
  dx_r:=zaehle(dx_r);
  dx_g:=zaehle(dx_g);
  dx_b:=zaehle(dx_b);
  g_pDDSOne.Unlock(nil);
  result:=0;
end;


end.
