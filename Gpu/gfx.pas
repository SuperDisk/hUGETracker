{+-----------------------------------------------------------------------------+
 | Author:      Christian Hackbart
 | Description: Tile renderer
 | last changes: 31.10.2000
 |
 | http://www.tu-ilmenau.de/~hackbart
 +----------------------------------------------------------------------------+}

 { - some games like Tomb Raider are not displayed correct }

unit gfx;

{$MODE Delphi}

interface

procedure make_line_tile(nr:Byte;offset:Word);
procedure make_line_finish(nr:Byte);
var tc:Word;

implementation uses windows,vars;

procedure make_line_tile(nr:Byte;offset:Word);
var t_temp:DWord;
    tilemap,tile_addr,w1,w2,w3:Word;
    buff_addr:DWord;
    tile_nr,attr,tile_fx:Byte;
    nr_y,nr_x:Byte;
    start_tiles:Word;
    tempo,lop:integer;


begin
tile_addr:=0;
 if (nr>143) or (offset>21) then exit;
 tc:=offset;

  buff_addr:=dx_bits*offset*8+100;
  nr_y:=(m_iram[$ff42]+nr);
  nr_x:=m_iram[$ff43];
  start_tiles:=((nr_x shr 3)+offset) and 31;
  if m_iram[$ff40] and 8>0 then tilemap:=$9c00
  else tilemap:=$9800;
  tile_nr:=m_iram[(tilemap+start_tiles+((nr_y and 248) shl 2))];

  attr:=m_iram2[(tilemap+start_tiles+((nr_y and 248) shl 2))];

  if m_iram[$ff40] and 16>0 then
  asm
   mov al,tile_nr
   movzx ax,al
   shl ax,4
   add ax,$8000
   mov tile_addr,ax
  end
  else
   asm
    mov al,tile_nr
    movsx ax,al
    sal ax,4
    add ax,$9000
    mov tile_addr,ax
   end;

  if (attr and 64)>0 then
   tile_addr:=tile_addr+(7-(nr_y-8*(nr_y shr 3)))*2
  else
   tile_addr:=tile_addr+(nr_y-8*(nr_y shr 3))*2;

  if attr and 8=0 then
  begin
   w1:=m_iram[tile_addr];inc(tile_addr);
   w2:=m_iram[tile_addr];
  end
  else
  begin
   w1:=m_iram2[tile_addr];inc(tile_addr);
   w2:=m_iram2[tile_addr];
  end;


  if attr and 32>0 then tile_fx:=1 else tile_fx:=0;

  w3:=(w2 shl 8) or w1;
  for lop:=0 to 7 do
  begin
   tempo:=pix[w3][lop][tile_fx];
   line_b_mask[buff_addr]:=tempo;
   t_temp:=pal_dx_b[attr and 7][tempo];
    case dx_bits of
     2:asm
           mov eax,t_temp
           mov edi,buff_addr
           mov word ptr line_b[edi],ax
           lea edi,[edi+2]
           mov buff_addr,edi
       end;
     3: asm
           mov eax,t_temp
           mov ebx,eax
           shr eax,16
           mov edi,buff_addr
           mov byte(line_b[edi]),bl
           mov byte(line_b[edi+1]),bh
           mov byte(line_b[edi+2]),al
           lea edi,[edi+3]
           mov buff_addr,edi
         end;
     4: asm
           mov eax,t_temp
           mov edi,buff_addr
           mov dword ptr line_b[edi],eax
           lea edi,[edi+4]
           mov buff_addr,edi
         end;
   end;
 end;

//  start_tiles:=(start_tiles+1) and 31;
end;

procedure make_line_finish(nr:Byte);
var w1,w2,w3:Word;
    t_temp:DWOrd;
    window_line,window_start:Word;
    window_tile_map,temp_v,qpoi:WORD;
    buff_addr:DWORD;
    tile_nr:Byte;
    attr:Byte;
    tile_addr:WORD;
    tile_fx:Byte;
    nr_x:Byte;
    ioi,lop,strt:Word;
    yu:DWord;
    s16,zmp,pix_temp:Byte;

    coordx_s:WorD;
    coordy_s,wx,wy,l_numb,
    sprite_la,flipx,address_w:integer; // signed int
    address:WORD;
    aal:DWORD;
    buffer,ad2:pointer;
begin
 if nr>143 then exit;
 while tc<20 do
  make_line_tile(nr,tc+1);//end;// !!! inc=??

 buff_addr:=0;
 nr_x:=m_iram[$ff43];
 if (m_iram[$ff40] and 32>0) and (m_iram[$ff4a]<=nr) then
 begin
  window_line:=nr-m_iram[$ff4a];
  temp_v:=window_line shr 3;
  window_start:=0;
  if (m_iram[$ff40] and 64)>0 then
   window_tile_map:=$9c00
  else
   window_tile_map:=$9800;

  for qpoi:=0 to 31 do
  begin
   tile_nr:=m_iram[(window_tile_map+(temp_v shl 5)+window_start)];
   attr:=m_iram2[(window_tile_map+(temp_v shl 5)+window_start)];
   if m_iram[$ff40] and 16>0 then
    asm
     mov al,tile_nr
     movzx ax,al
     shl ax,4
         add ax,$8000
     mov tile_addr,ax
    end
   else
    asm
     mov al,tile_nr
     movsx ax,al
     sal ax,4
     add ax,$9000
         mov tile_addr,ax
    end;

   if (attr and 64)>0 then
        tile_addr:=tile_addr+(7-(window_line-8*temp_v))*2
   else
        tile_addr:=tile_addr+(window_line-8*temp_v)*2;
   if attr and 8=0 then
   begin
    w1:=m_iram[tile_addr];inc(tile_addr);
    w2:=m_iram[tile_addr];
   end
   else
   begin
    w1:=m_iram2[tile_addr];inc(tile_addr);
    w2:=m_iram2[tile_addr];
   end;

   w3:=(w2 shl 8) or w1;
   if attr and 32>0 then
        tile_fx:=1
   else
        tile_fx:=0;

   for lop:=0 to 7 do
   begin
     begin
        t_temp:=pal_dx_b[attr and 7][pix[w3][lop][tile_fx]];
        case dx_bits of
         2: asm
             mov eax,t_temp
             mov edi,buff_addr
             mov word ptr window_b[edi],ax
             lea edi,[edi+2]
             mov buff_addr,edi
            end;
         3: asm
             mov eax,t_temp
             mov ebx,eax
             shr eax,16
             mov edi,buff_addr
             mov byte(window_b[edi]),bl
             mov byte(window_b[edi+1]),bh
             mov byte(window_b[edi+2]),al
             lea edi,[edi+3]
             mov buff_addr,edi
            end;
         4: asm
             mov eax,t_temp
             mov edi,buff_addr
             mov dword ptr window_b[edi],eax
             lea edi,[edi+4]
             mov buff_addr,edi
            end;
         end;
       end;
   end;
  inc(window_start);
  window_start:=window_start and 31;
 end;

 strt:=m_iram[$ff4b];
 dec(strt,7);

 if strt<160 then
  for yu:=0 to ((160-strt)*dx_bits)-1 do
  begin
   line_b[(strt+(nr_x and 7))*dx_bits+yu+100]:=window_b[yu];
   line_b_mask[(strt+(nr_x and 7))*dx_bits+yu+100]:=window_b_mask[yu];
  end;
 end;

 if m_iram[$ff40] and 2>0 then
 begin
  s16:=0;
  if (m_iram[$ff40] and 4)>0 then s16:=1;

  for ioi:=39 downto 0 do
  begin
   address:=ioi*4+$fe00;
   wx:=m_iram[address+1];
   wy:=m_iram[address];
   coordx_s:=wx-8;
   coordy_s:=wy-16;
   attr:=m_iram[address+3];
   if(gb_mode<>CGB) then
   begin
           attr:=attr and $f0;
           if attr and $10>0 then attr:=attr or 1;
   end;

   if (wx>0) and (wy>0) and (wx<168) and (coordy_s<=nr) and
      (
       ((s16=0) and (coordy_s>=nr-7)) or
       ((s16>0) and (coordy_s>=nr-15))) then
   begin
    l_numb:=nr-coordy_s;
    if(m_iram[address+3] and 64)>0 then
     if s16>0 then
      l_numb:=15-l_numb
     else
       l_numb:=7-l_numb;
     if s16=0 then
      sprite_la:=$8000+m_iram[address+2]*16+2*l_numb
     else
      sprite_la:=$8000+(m_iram[address+2] and 254)*16+2*l_numb;
     if attr and 32>0 then
      flipx:=1
     else
      flipx:=0;
     if attr and 8>0 then
         begin
          w1:=m_iram2[sprite_la];inc(sprite_la);
          w2:=m_iram2[sprite_la];
         end
         else
         begin
          w1:=m_iram[sprite_la];inc(sprite_la);
          w2:=m_iram[sprite_la];
         end;
     w3:=(w2 shl 8) or w1;
     address_w:=(coordx_s+(nr_x and 7))*dx_bits+100;
     for zmp:=0 to 7 do
     begin
      pix_temp:=pix[w3][zmp][flipx];
      if ((((attr and 128=0)) or ((line_b_mask[address_w]=0))) and (pix_temp>0)) then
      begin
           t_temp:=pal_dx_o[attr and 7][pix_temp];
           case dx_bits of
             2: asm
                   mov eax,t_temp
                   mov edi,address_w
                   mov word ptr line_b[edi],ax
                   lea edi,[edi+2]
                   mov address_w,edi
                end;  
             3: asm
                   mov eax,t_temp
                   mov ebx,eax
                   shr eax,16
                   mov edi,address_w
                   mov byte(line_b[edi]),bl
                   mov byte(line_b[edi+1]),bh
                   mov byte(line_b[edi+2]),al
                   lea edi,[edi+3]
                   mov address_w,edi
                 end;
             4: asm
                   mov eax,t_temp
                   mov edi,address_w
                   mov dword ptr line_b[edi],eax
                   lea edi,[edi+4]
                   mov address_w,edi
                end;
           end;
         end
          else
           inc(address_w,dx_bits);
       end;
    end;
   end;
 end;
 aal:=nr*dx_pitch;
 buffer:=addr(line_b[100+(nr_x and 7)*dx_bits]);
 ad2:=addr(dx_buffer[0]);
 asm
   mov esi,dword ptr [buffer]
   mov edi,dword ptr [ad2]
   add edi,aal
   mov ecx,cmov
   cld
   rep movsd
 end;
end;

end.

