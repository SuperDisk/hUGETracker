// GBColor does´nt work yet
unit UnitMapview;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls;

type
  TfrmMapview = class(TForm)
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    Address: TLabel;
    R0: TLabel;
    G0: TLabel;
    R1: TLabel;
    G1: TLabel;
    R2: TLabel;
    G2: TLabel;
    R3: TLabel;
    G3: TLabel;
    Number: TLabel;
    HFlip: TLabel;
    VFlip: TLabel;
    ZoomView: TGroupBox;
    PaintBox2: TPaintBox;
    TabControl1: TTabControl;
    Map: TGroupBox;
    PaintBox1: TPaintBox;
    B3: TLabel;
    B2: TLabel;
    B1: TLabel;
    B0: TLabel;
    procedure PaintBox1Paint(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    { Private-Deklarationen}

  public
    { Public-Deklarationen}
    procedure DoUpdate;
  end;

var
  frmMapview: TfrmMapview;

implementation uses vars;

const mapBase:word=$1800;
      vidBase=$8000;


{$R *.lfm}

procedure TfrmMapview.PaintBox1Paint(Sender: TObject);
begin
 DoUpdate;
end;

procedure TfrmMapview.DoUpdate;
var bmp:TBitmap;

    i,j,x,y:byte; // Tilepositions
    n: Word;

    pal:byte;

    tileptr:Array[0..1] of byte;
    bmpPtr0:pointer;

    tile_addr:Word;
    bmpPtr0_index:word;

    bit,bit0,bit1:byte;
    c:Word;
    vflip,hflip:boolean;
begin

   case m_iram[$FF40] and $48 of
   $00: begin
         TabControl1.Tabs.Strings[0]:='$9800 (BG/Wnd)';
         TabControl1.Tabs.Strings[1]:='$9C00';
        end;
   $08: begin
         TabControl1.Tabs.Strings[0]:='$9800 (Wnd)';
         TabControl1.Tabs.Strings[1]:='$9C00 (BG)';
        end;
   $40: begin
         TabControl1.Tabs.Strings[0]:='$9800 (BG)';
         TabControl1.Tabs.Strings[1]:='$9C00 (Wnd)';
        end;
   $48: begin
         TabControl1.Tabs.Strings[0]:='$9800';
         TabControl1.Tabs.Strings[1]:='$9C00 (BG/Wnd)';
        end;
   end;

   bmp:=TBitmap.Create;
   bmp.Width:=8;
   bmp.Height:=8;
   bmp.PixelFormat:=pf24bit;
   for y:=0 to 31 do
        for x:=0 to 31 do
        begin
            vflip:=false;
            hflip:=false;
            if m_Iram[$FF40] and 16>0 then
                n:=m_iram[vidbase+mapBase+y*32+x]
            else
                n:=256+shortint(m_iram[vidbase+mapBase+y*32+x]);


            if gb_mode=cgb then
            begin
             if m_iram[vidbase+mapBase+y*32+x+$2000] and 8>0 then inc(n,512);
             vflip:=m_iram[vidbase+mapBase+y*32+x+$2000] and $40>0;
             hflip:=m_iram[vidbase+mapBase+y*32+x+$2000] and $20>0;
            end;
            pal:=m_iram[vidbase+mapBase+y*32+x+$2000] and 7;

            for j:=0 to 7 do
            begin
             if vflip then
              i:=7-j else i:=j;

                if n>=512 then
                 tile_Addr:=vidbase+$2000+(n-512)*16+i*2 else
                 tile_Addr:=vidbase+n*16+i*2;

                tilePtr[0]:=m_iram[tile_Addr];
                tilePtr[1]:=m_iram[tile_Addr+1];

                bmpPtr0:=bmp.ScanLine[j];bmpPtr0_index:=0;

                if hflip then bit:=1 else bit:=128;
                for i:=0 to 7 do
                begin
                 if tilePtr[0] and bit>0 then bit0:=1 else bit0:=0;
                 if tilePtr[1] and bit>0 then bit1:=2 else bit1:=0;
                 if hflip then bit:=bit shl 1 else bit:=bit shr 1;

                 if gb_mode<>cgb then
                    begin
                        case bit1 or bit0 of
                         0: c:=3-m_Iram[$FF47] and 3;
                         1: c:=3-(m_Iram[$FF47] shr 2) and 3;
                         2: c:=3-(m_Iram[$FF47] shr 4) and 3;
                         3: c:=3-(m_Iram[$FF47] shr 6) and 3;
                        end;
                        c:=c*255 div 3;

                        byte(pchar(bmpPtr0)[bmpPtr0_index]):=c;inc(bmpPtr0_index);
                        byte(pchar(bmpPtr0)[bmpPtr0_index]):=c;inc(bmpPtr0_index);
                        byte(pchar(bmpPtr0)[bmpPtr0_index]):=c;inc(bmpPtr0_index);

                    end
                    else
                    begin
                        c:=bit1 or bit0;
                        byte(pchar(bmpPtr0)[bmpPtr0_index]):= (palramb[(pal shl 2)+c] and 31) shl 3;inc(bmpPtr0_index);
                        byte(pchar(bmpPtr0)[bmpPtr0_index]):=((palramb[(pal shl 2)+c] shr 5) and 31) shl 3;inc(bmpPtr0_index);
                        byte(pchar(bmpPtr0)[bmpPtr0_index]):=((palramb[(pal shl 2)+c] shr 10) and 31) shl 3;inc(bmpPtr0_index);
                    end;

                end;
            end;
            PaintBox1.Canvas.Pen.Color:=clSilver;
            PaintBox1.Canvas.Brush.Color:=clSilver;
            PaintBox1.Canvas.Rectangle(x*9,y*9,x*9+9,y*9+9);
            PaintBox1.Canvas.Draw(x*9,y*9,bmp);
        end;
end;

procedure TfrmMapview.TabControl1Change(Sender: TObject);
begin
  if TabControl1.TabIndex=0 then mapBase:=$1800 else mapBase:=$1c00;
  DoUpdate;
end;

procedure TfrmMapview.PaintBox1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var i,j,n:integer;
    vflp,hflp:Boolean;
    tile_Addr: Word;
    str:String;
    p:integer;
    tilePtr:Array[0..1] of byte;
    bit0,bit1,bit,c:Byte;
    rect:TRect;
    r,g,b:byte;
begin
    x:=X div 9;
    y:=Y div 9;

    vflp:=false;
    hflp:=false;

    if m_iram[$FF40] and 16>0 then
        n:=m_iram[vidbase+mapBase+y*32+x]
    else
        n:=256+shortint(m_iram[vidbase+mapBase+y*32+x]);
        
    if gb_mode=cgb then
    begin
     if m_iram[vidbase+mapBase+y*32+x+$2000] and 8>0 then inc(n,512);

     vflp:=m_iram[vidbase+mapBase+y*32+x+$2000] and $40>0;
     hflp:=m_iram[vidbase+mapBase+y*32+x+$2000] and $20>0;
    end;

    p:=m_iram[vidbase+mapBase+y*32+x+$2000] and 7;
    Number.Caption:='Tile number = '+inttostr(m_iram[vidbase+mapBase+y*32+x]);
    if (n<512) then
        Address.Caption:='Tile address = 0:'+inttostr(n*16+$8000)
    else
        Address.Caption:='Tile address = 1:'+inttostr((n-512)*16+$8000);

    if HFlp then HFlip.Caption:='HFlip=Yes' else HFlip.Caption:='HFlip=No';
    if vFlp then vFlip.Caption:='VFlip=Yes' else vFlip.Caption:='VFlip=No';

    for j:=0 to 7 do
    begin
     if vflp then i:=7-j else i:=j;

     if n>=512 then
     tile_Addr:=vidbase+$2000+(n-512)*16+i*2 else
     tile_Addr:=vidbase+n*16+i*2;

     tilePtr[0]:=m_iram[tile_Addr];
     tilePtr[1]:=m_iram[tile_Addr+1];

      if hflp then bit:=1 else bit:=128;
      for i:=0 to 7 do
      begin
       if tilePtr[0] and bit>0 then bit0:=1 else bit0:=0;
       if tilePtr[1] and bit>0 then bit1:=2 else bit1:=0;
       if hflp then bit:=bit shl 1 else bit:=bit shr 1;

       if gb_mode<>cgb then
        begin
         case bit1 or bit0 of
           0: c:=3-m_Iram[$FF47] and 3;
           1: c:=3-(m_Iram[$FF47] shr 2) and 3;
           2: c:=3-(m_Iram[$FF47] shr 4) and 3;
           3: c:=3-(m_Iram[$FF47] shr 6) and 3;
         end;

         Shape1.Brush.Color:=((((3-m_iram[$FF47] and 3)*255 div 3) shl 16) or (((3-m_iram[$FF47] and 3)*255 div 3) shl 8) or ((3-m_iram[$FF47] and 3)*255 div 3));
         Shape2.Brush.Color:=((((3-(m_iram[$FF47] shr 2) and 3)*255 div 3) shl 16) or (((3-(m_iram[$FF47] shr 2) and 3)*255 div 3) shl 8) or ((3-(m_iram[$FF47] shr 2) and 3)*255 div 3));
         Shape3.Brush.Color:=((((3-(m_iram[$FF47] shr 4) and 3)*255 div 3) shl 16) or (((3-(m_iram[$FF47] shr 4) and 3)*255 div 3) shl 8) or ((3-(m_iram[$FF47] shr 4) and 3)*255 div 3));
         Shape4.Brush.Color:=((((3-(m_iram[$FF47] shr 6) and 3)*255 div 3) shl 16) or (((3-(m_iram[$FF47] shr 6) and 3)*255 div 3) shl 8) or ((3-(m_iram[$FF47] shr 6) and 3)*255 div 3));

         R0.Caption:='R='+inttostr((3-m_iram[$FF47] and 3)*31 div 3);
         G0.Caption:='G='+inttostr((3-m_iram[$FF47] and 3)*31 div 3);
         B0.Caption:='B='+inttostr((3-m_iram[$FF47] and 3)*31 div 3);
         R1.Caption:='R='+inttostr((3-(m_iram[$FF47] shr 2) and 3)*31 div 3);
         G1.Caption:='G='+inttostr((3-(m_iram[$FF47] shr 2) and 3)*31 div 3);
         B1.Caption:='B='+inttostr((3-(m_iram[$FF47] shr 2) and 3)*31 div 3);
         R2.Caption:='R='+inttostr((3-(m_iram[$FF47] shr 4) and 3)*31 div 3);
         G2.Caption:='G='+inttostr((3-(m_iram[$FF47] shr 4) and 3)*31 div 3);
         B2.Caption:='B='+inttostr((3-(m_iram[$FF47] shr 4) and 3)*31 div 3);
         R3.Caption:='R='+inttostr((3-(m_iram[$FF47] shr 6) and 3)*31 div 3);
         G3.Caption:='G='+inttostr((3-(m_iram[$FF47] shr 6) and 3)*31 div 3);
         B3.Caption:='B='+inttostr((3-(m_iram[$FF47] shr 6) and 3)*31 div 3);

         c:=c*255 div 3;

         PaintBox2.Canvas.Brush.Color:=((c shl 16) or (c shl 8) or c);

         with rect do
          begin
           Left:=16*i; Right:=16*i+15;
           Top:=16*j; Bottom:=16*j+15;
          end;
         PaintBox2.Canvas.FillRect(rect);
        end
        else
        begin
         c:=bit1 or bit0;
         Shape1.Brush.Color:=(((palramb[(p shl 2)+0] and 31) shl 19) or (((palramb[(p shl 2)+0] shr 5) and 31) shl 11) or (((palramb[(p shl 2)+0] shr 10) and 31) shl 3));
         Shape2.Brush.Color:=(((palramb    [(p shl 2)+1] and 31) shl 19) or (((palramb[(p shl 2)+1] shr 5) and 31) shl 11) or (((palramb[(p shl 2)+1] shr 10) and 31) shl 3));
         Shape3.Brush.Color:=(((palramb[(p shl 2)+2] and 31) shl 19) or (((palramb[(p shl 2)+2] shr 5) and 31) shl 11) or (((palramb[(p shl 2)+2] shr 10) and 31) shl 3));
         Shape4.Brush.Color:=(((palramb[(p shl 2)+3] and 31) shl 19) or (((palramb[(p shl 2)+3] shr 5) and 31) shl 11) or (((palramb[(p shl 2)+3] shr 10) and 31) shl 3));
         R0.Caption:='R='+inttostr(palramb[(p shl 2)+0] and 31);
         G0.Caption:='G='+inttostr(palramb[(p shl 2)+1] and 31);
         B0.Caption:='B='+inttostr(palramb[(p shl 2)+2] and 31);
         R1.Caption:='R='+inttostr(palramb[(p shl 2)+3] and 31);
         G1.Caption:='G='+inttostr((palramb[(p shl 2)+0] shr 5) and 31);
         B1.Caption:='B='+inttostr((palramb[(p shl 2)+1] shr 5) and 31);
         R2.Caption:='R='+inttostr((palramb[(p shl 2)+2] shr 5) and 31);
         G2.Caption:='G='+inttostr((palramb[(p shl 2)+3] shr 5) and 31);
         B2.Caption:='B='+inttostr((palramb[(p shl 2)+0] shr 10) and 31);
         R3.Caption:='R='+inttostr((palramb[(p shl 2)+1] shr 10) and 31);
         G3.Caption:='G='+inttostr((palramb[(p shl 2)+2] shr 10) and 31);
         B3.Caption:='B='+inttostr((palramb[(p shl 2)+3] shr 10) and 31);

         r:=(palramb[(p shl 2)+c] and 31) shl 3;
         g:=((palramb[(p shl 2)+c] shr 5) and 31) shl 3;
         b:=((palramb[(p shl 2)+c] shr 10) and 31) shl 3;
         PaintBox2.Canvas.Brush.Color:=((r shl 16) or (g shl 8) or b);
         with rect do
          begin
           Left:=16*i; Right:=16*i+15;
           Top:=16*j; Bottom:=16*j+15;
          end;
         PaintBox2.Canvas.FillRect(rect);
        end;
      end;
  end;
end;

end.
//---------------------------------------------------------------------------

void __fastcall TMapWnd::PaintBox1MouseMove(TObject *Sender,
      TShiftState Shift, int X, int Y)
{
    int x=X/9;
    int y=Y/9;
    int n;
    int vflip=0;
    int hflip=0;
    if (m_iram[$FF40] and 16)
        n={vid} m_iram[mapBase+y*32+x];
    else
        n=256+(int)((char){vid} m_iram[mapBase+y*32+x]);
    if (cgb)
    {
        n+=({vid} m_iram[mapBase+y*32+x+$2000] and 8)?512:0;
        vflip={vid} m_iram[mapBase+y*32+x+$2000] and $40;
        hflip={vid} m_iram[mapBase+y*32+x+$2000] and $20;
    }
    int p={vid} m_iram[mapBase+y*32+x+$2000] and 7;
    char str[32];
    sprintf(str,'Tile number = $%2.2X',{vid} m_iram[mapBase+y*32+x]);
    Number.Caption=str;
    if (n<512)
        sprintf(str,'Tile address = 0:$%4.4X',n*16+$8000);
    else
        sprintf(str,'Tile address = 1:$%4.4X',(n-512)*16+$8000);
    Address.Caption=str;
    sprintf(str,'HFlip=%s',hflip?'Yes':'No');
    HFlip.Caption=str;
    sprintf(str,'VFlip=%s',vflip?'Yes':'No');
    VFlip.Caption=str;
    for (int j=0;j<8;j++)
    {
        char* tilePtr=(n>=512)?( and {vid} m_iram[$2000+(n-512)*16+(vflip?(7-j):j)*2]):( and {vid} m_iram[n*16+(vflip?(7-j):j)*2]);
        for (int i=0,bit=hflip?1:128;i<8;i++,hflip?bit shl =1:bit shr =1)
        {
            int b0=(tilePtr[0] and bit)?1:0;
            int b1=(tilePtr[1] and bit)?2:0;
            if (!cgb)
            {
                int c=b1 or b0;
                switch (c)
                {
                    case 0: c=3-m_iram[$FF47] and 3; break;
                    case 1: c=3-(m_iram[$FF47] shr 2) and 3; break;
                    case 2: c=3-(m_iram[$FF47] shr 4) and 3; break;
                    case 3: c=3-(m_iram[$FF47] shr 6) and 3; break;
                }
                Shape1.Brush.Color=(TColor)((((3-m_iram[$FF47] and 3)*255/3) shl 16) or (((3-m_iram[$FF47] and 3)*255/3) shl 8) or ((3-m_iram[$FF47] and 3)*255/3));
                Shape2.Brush.Color=(TColor)((((3-(m_iram[$FF47] shr 2) and 3)*255/3) shl 16) or (((3-(m_iram[$FF47] shr 2) and 3)*255/3) shl 8) or ((3-(m_iram[$FF47] shr 2) and 3)*255/3));
                Shape3.Brush.Color=(TColor)((((3-(m_iram[$FF47] shr 4) and 3)*255/3) shl 16) or (((3-(m_iram[$FF47] shr 4) and 3)*255/3) shl 8) or ((3-(m_iram[$FF47] shr 4) and 3)*255/3));
                Shape4.Brush.Color=(TColor)((((3-(m_iram[$FF47] shr 6) and 3)*255/3) shl 16) or (((3-(m_iram[$FF47] shr 6) and 3)*255/3) shl 8) or ((3-(m_iram[$FF47] shr 6) and 3)*255/3));
                char str[8];
                sprintf(str,'R=%d',((3-m_iram[$FF47] and 3)*31/3));
                R0.Caption=str;
                sprintf(str,'G=%d',((3-m_iram[$FF47] and 3)*31/3));
                G0.Caption=str;
                sprintf(str,'B=%d',((3-m_iram[$FF47] and 3)*31/3));
                B0.Caption=str;
                sprintf(str,'R=%d',((3-(m_iram[$FF47] shr 2) and 3)*31/3));
                R1.Caption=str;
                sprintf(str,'G=%d',((3-(m_iram[$FF47] shr 2) and 3)*31/3));
                G1.Caption=str;
                sprintf(str,'B=%d',((3-(m_iram[$FF47] shr 2) and 3)*31/3));
                B1.Caption=str;
                sprintf(str,'R=%d',((3-(m_iram[$FF47] shr 4) and 3)*31/3));
                R2.Caption=str;
                sprintf(str,'G=%d',((3-(m_iram[$FF47] shr 4) and 3)*31/3));
                G2.Caption=str;
                sprintf(str,'B=%d',((3-(m_iram[$FF47] shr 4) and 3)*31/3));
                B2.Caption=str;
                sprintf(str,'R=%d',((3-(m_iram[$FF47] shr 6) and 3)*31/3));
                R3.Caption=str;
                sprintf(str,'G=%d',((3-(m_iram[$FF47] shr 6) and 3)*31/3));
                G3.Caption=str;
                sprintf(str,'B=%d',((3-(m_iram[$FF47] shr 6) and 3)*31/3));
                B3.Caption=str;
                c*=255;
                c/=3;
                PaintBox2.Canvas.Brush.Color=(TColor)((c shl 16) or (c shl 8) or c);
                TRect r;
                r.Left=16*i; r.Right=16*i+15;
                r.Top=16*j; r.Bottom=16*j+15;
                PaintBox2.Canvas.FillRect(r);
            }
            else
            {
                int c=b1 or b0;
                Shape1.Brush.Color=(TColor)(((palramb[(p shl 2)+0] and 31) shl 19) or (((palramb[(p shl 2)+0] shr 5) and 31) shl 11) or (((palramb[(p shl 2)+0] shr 10) and 31) shl 3));
                Shape2.Brush.Color=(TColor)(((palramb[(p shl 2)+1] and 31) shl 19) or (((palramb[(p shl 2)+1] shr 5) and 31) shl 11) or (((palramb[(p shl 2)+1] shr 10) and 31) shl 3));
                Shape3.Brush.Color=(TColor)(((palramb[(p shl 2)+2] and 31) shl 19) or (((palramb[(p shl 2)+2] shr 5) and 31) shl 11) or (((palramb[(p shl 2)+2] shr 10) and 31) shl 3));
                Shape4.Brush.Color=(TColor)(((palramb[(p shl 2)+3] and 31) shl 19) or (((palramb[(p shl 2)+3] shr 5) and 31) shl 11) or (((palramb[(p shl 2)+3] shr 10) and 31) shl 3));
                char str[8];
                sprintf(str,'B=%d',((palramb[(p shl 2)+0] and 31)));
                B0.Caption=str;
                sprintf(str,'B=%d',((palramb[(p shl 2)+1] and 31)));
                B1.Caption=str;
                sprintf(str,'B=%d',((palramb[(p shl 2)+2] and 31)));
                B2.Caption=str;
                sprintf(str,'B=%d',((palramb[(p shl 2)+3] and 31)));
                B3.Caption=str;
                sprintf(str,'G=%d',((palramb[(p shl 2)+0] shr 5) and 31));
                G0.Caption=str;
                sprintf(str,'G=%d',((palramb[(p shl 2)+1] shr 5) and 31));
                G1.Caption=str;
                sprintf(str,'G=%d',((palramb[(p shl 2)+2] shr 5) and 31));
                G2.Caption=str;
                sprintf(str,'G=%d',((palramb[(p shl 2)+3] shr 5) and 31));
                G3.Caption=str;
                sprintf(str,'R=%d',((palramb[(p shl 2)+0] shr 10) and 31));
                R0.Caption=str;
                sprintf(str,'R=%d',((palramb[(p shl 2)+1] shr 10) and 31));
                R1.Caption=str;
                sprintf(str,'R=%d',((palramb[(p shl 2)+2] shr 10) and 31));
                R2.Caption=str;
                sprintf(str,'R=%d',((palramb[(p shl 2)+3] shr 10) and 31));
                R3.Caption=str;
                int r=(palramb[(p shl 2)+c] and 31) shl 3;
                int g=((palramb[(p shl 2)+c] shr 5) and 31) shl 3;
                int b=((palramb[(p shl 2)+c] shr 10) and 31) shl 3;
                PaintBox2.Canvas.Brush.Color=(TColor)((r shl 16) or (g shl 8) or b);
                TRect rect;
                rect.Left=16*i; rect.Right=16*i+15;
                rect.Top=16*j; rect.Bottom=16*j+15;
                PaintBox2.Canvas.FillRect(rect);
            }
        }
    }
}
