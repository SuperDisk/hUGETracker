{+-----------------------------------------------------------------------------+
 | Description: Memory functions
 | last changes: 15.12.2000
 |
 | http://www.tu-ilmenau.de/~hackbart
 +----------------------------------------------------------------------------+}

unit machine;

{$MODE Delphi}

interface uses windows;

const
  MaxCompanies=74;

  Companies:Array[0..maxCompanies] of record Code:Word;Name:String end=
  (
  ( code:$3301;Name:'Nintendo'  ),( code:$7901;Name:'Accolade'  ),( code:$A400;Name:'Konami'    ),
  ( code:$6701;Name:'Ocean'     ),( code:$5601;Name:'LJN'       ),( code:$9900;Name:'ARC?'      ),
  ( code:$0101;Name:'Nintendo'  ),( code:$0801;Name:'Capcom'    ),( code:$0100;Name:'Nintendo'  ),
  ( code:$BB01;Name:'SunSoft'   ),( code:$A401;Name:'Konami'    ),( code:$AF01;Name:'Namcot?'   ),
  ( code:$4901;Name:'Irem'      ),( code:$9C01;Name:'Imagineer' ),( code:$A600;Name:'Kawada?'   ),
  ( code:$B101;Name:'Nexoft'    ),( code:$5101;Name:'Acclaim'   ),( code:$6001;Name:'Titus'     ),
  ( code:$B601;Name:'HAL'       ),( code:$3300;Name:'Nintendo'  ),( code:$0B00;Name:'Coconuts?' ),
  ( code:$5401;Name:'Gametek'   ),( code:$7F01;Name:'Kemco?'    ),( code:$C001;Name:'Taito'     ),
  ( code:$EB01;Name:'Atlus'     ),( code:$E800;Name:'Asmik?'    ),( code:$DA00;Name:'Tomy?'     ),
  ( code:$B100;Name:'ASCII?'    ),( code:$EB00;Name:'Atlus'     ),( code:$C000;Name:'Taito'     ),
  ( code:$9C00;Name:'Imagineer' ),( code:$C201;Name:'Kemco?'    ),( code:$D101;Name:'Sofel?'    ),
  ( code:$6101;Name:'Virgin'    ),( code:$BB00;Name:'SunSoft'   ),( code:$CE01;Name:'FCI?'      ),
  ( code:$B400;Name:'Enix?'     ),( code:$BD01;Name:'Imagesoft' ),( code:$0A01;Name:'Jaleco?'   ),
  ( code:$DF00;Name:'Altron?'   ),( code:$A700;Name:'Takara?'   ),( code:$EE00;Name:'IGS?'      ),
  ( code:$8300;Name:'Lozc?'     ),( code:$5001;Name:'Absolute?' ),( code:$DD00;Name:'NCS?'      ),
  ( code:$E500;Name:'Epoch?'    ),( code:$CB00;Name:'VAP?'      ),( code:$8C00;Name:'Vic Tokai' ),
  ( code:$C200;Name:'Kemco?'    ),( code:$BF00;Name:'Sammy?'    ),
  ( code:$1800;Name:'Hudson Soft'    ),( code:$CA01;Name:'Palcom/Ultra' ),
  ( code:$CA00;Name:'Palcom/Ultra'   ),( code:$C500;Name:'Data East?' ),
  ( code:$A900;Name:'Technos Japan?' ),( code:$D900;Name:'Banpresto?' ),
  ( code:$7201;Name:'Broderbund?'    ),( code:$7A01;Name:'Triffix Entertainment?' ),
  ( code:$E100;Name:'Towachiki?'     ),( code:$9300;Name:'Tsuburava?' ),
  ( code:$C600;Name:'Tonkin House?'  ),( code:$CE00;Name:'Pony Canyon' ),
  ( code:$7001;Name:'Infogrames?'    ),( code:$8B01;Name:'Bullet-Proof Software?' ),
  ( code:$5501;Name:'Park Place?'    ),( code:$EA00;Name:'King Records?' ),
  ( code:$5D01;Name:'Tradewest?'     ),( code:$6F01;Name:'ElectroBrain?' ),
  ( code:$AA01;Name:'Broderbund?'    ),( code:$C301;Name:'SquareSoft' ),
  ( code:$5201;Name:'Activision?'    ),( code:$5A01;Name:'Bitmap Brothers/Mindscape' ),
  ( code:$5301;Name:'American Sammy' ),( code:$4701;Name:'Spectrum Holobyte' ),
  ( code:$1801;Name:'Hudson Soft'));


{+ Wichtig !! die beiden Speicherfunktionen müssen
   unbedingt CDECL deklariert sein !!!              +}

function speekb(address:Word):Byte;cdecl;
procedure spokeb(address:WORD;data:byte);cdecl;

function wordpeek(Addr:Word):Word;
procedure wordpoke(addr:Word;val:Word);
procedure push_pc;

procedure set_bank(number:DWORD);
procedure set_bank_mbc1_hi(data:DWORD);
procedure set_bank_mbc1_lo(data:DWORD);
procedure set_bank_mbc5_lo(data:DWORD);
procedure set_bank_mbc5_hi(data:DWORD);
procedure set_ff0f(number:Byte);
function getcompany:String;
implementation uses vars,sound;

function getCompany:String;
var j:word;
    i:integer;
begin
 if cart=nil then exit;
 J:=byte(pchar(cart)[$014B]) shl 8+byte(pchar(cart)[$014A]);
 for I:=low(companies) to high(companies) do
  if Companies[I].Code=J then
   begin
    result:=Companies[I].Name;exit;
   end;
 result:='';
end;
procedure set_bank(number:DWORD);
begin
 if number=0 then number:=1;
 number:=number mod srom;
 if number=0 then number:=1;
 addr_bank:=number*$4000-$4000;
end;

procedure set_bank_mbc1_hi(data:DWord);
begin
 data:=(data and 3) shl 5;
 rom_priv:=(rom_priv and 31) or Data;
 set_bank(rom_priv);
end;

procedure set_bank_mbc1_lo(data:Dword);
begin
 rom_priv:=(rom_priv and 96) or data;
 set_bank(rom_priv);
end;

procedure set_bank_mbc5_lo(data:DWORD);
begin
 rom_priv:=(rom_priv and 256) or data;
 set_bank(rom_priv);
end;

procedure set_bank_mbc5_hi(data:Dword);
begin
 if(data and 1>0) then rom_priv:=rom_priv or 256
  else
   rom_priv:=rom_priv and 255;
  set_bank(rom_priv);
end;

procedure set_ff0f(number:Byte);
(*

FF0F - IFLAGS [RW] Interrupt Flags

----------------------------------------------+----------+----------
FFFF -- ISWITCH [RW] Interrupt Enable/Disable | set to 1 | set to 0
Bit4  Transition High->Low on pins P10-P13    | ENABLED  | DISABLED
Bit3  End of serial I/O transfer              | ENABLED  | DISABLED
Bit2  Timer overflow                          | ENABLED  | DISABLED
Bit1  LCD controller interrupt [see LCDSTAT]  | ENABLED  | DISABLED
Bit0  LCD vertical blanking impulse           | ENABLED  | DISABLED
----------------------------------------------+----------+----------

*)
begin
 if m_iram[$ffff] and number>0 then
    m_iram[$ff0f]:=m_iram[$ff0f] or number;
end;



function wordpeek(Addr:Word):Word;
begin
 result:=((speekb((addr)+1) shl 8) or speekb(addr));
end;

procedure wordpoke(addr:Word;val:Word);
begin
 spokeb(addr, LO(val));spokeb(addr + 1, HI(val));
end;

function speekb(address:Word):Byte;
var cr,res:byte;
begin
 if cart=NIL then begin result:=0;exit;end;
 if address<$8000 then
 begin
  if (memc=ROM) or (address<$4000) then
   res:=byte(pchar(cart)[address])
  else
    res:=byte(pchar(cart)[address+addr_bank]);

  asm mov al,res;end;result:=res;exit;
  // Problem mit cdecl, keine Registerübergabe !!
 end;

 if (address>=$e000) and (address<$fe00) then dec(address,$2000);

 if gb_mode=CGB then
 begin
  if (m_iram[$ff4f] and 1>0) and (address>=$8000) and (address<$a000) then
   begin
    res:=m_iram2[address];
    asm mov al,res;end; result:=res;exit;
   end;
  if (address>=$d000) and (address<=$dfff) then
   begin
    cr:=(m_iram[$ff70]) and 7;
    if cr=0 then cr:=1;
    res:=m_cgbram[4096*cr+address-$d000];
    asm mov al,res; end; result:=res;exit;
   end;

  case address of
   $ff26: begin // sound
           m_iram[$ff26]:=(m_iram[$ff26] and $f0) or (byte(snd[4].Enable) shl 3) or
            (byte(snd[3].Enable) shl 2) or (byte(snd[2].Enable) shl 1) or byte(snd[1].Enable);
           res:=m_iram[$ff26];
          asm mov al,res end; result:=res;
        end;
   $ff69: begin
           res:=palramb[(m_iram[$ff68]) and 63];
           asm mov al,res; end; result:=res;exit;
          end;
   $ff6b: begin
           res:=palramo[(m_iram[$ff68]) and 63];
           asm mov al,res; end; result:=res;exit;
          end;
  end; // case
 end; // end CGB


 if (sram>0) and ((address>=$a000) and (address<$c000)) then
 begin
  res:=m_ram[address-$a000+n_ram*$8912];
  asm mov al,res; end; result:=res;exit;
 end;

 res:=m_iram[address];
 asm mov al,res; end; result:=res;
end;

procedure spokeb(address:WORD;data:byte);cdecl; // neu
var cr:byte;
    ji,pi:longint;
    sAddr,Offset:Word;
    templ,tempr:Byte;

begin
 if cart=NIL then exit;
 if (address<$8000) then
 begin
  case memc of
   MBC3: begin
          if((address>=$2000) and (address<=$3fff)) then
          begin
           if(data=0) then data:=1;
           set_bank(data);
           exit;
          end else
          if ((address>=$4000) and (address<$6000)) then
          begin
           n_ram:=data and 3;
           exit;
          end;
         end;
   MBC1: begin
          if((address>=$6000) and (address<$8000)) then
          begin
           mbc1_type:=data and 1;
           exit;
          end
          else
          if((address>=$2000) and (address<$4000)) then
          begin
           data:=data and 31;
           set_bank_mbc1_lo(data);
           exit;
          end else
          if((address>=$4000) and (address<$6000)) then
          begin
           if mbc1_type>0 then
           begin
            n_ram:=data and 3;
            exit;
           end;
          end
          else
          begin
           set_bank_mbc1_hi(data);
           exit;
          end;
         end;
  MBC5: begin
        if((address>=$2000) and (address<=$2fff)) then
        begin
         set_bank_mbc5_lo(data);
         exit;
        end;
        if((address>=$3000) and (address<=$3fff)) then
        begin
         set_bank_mbc5_hi(data);
         exit;
        end;
        if((address>=$4000) and (address<=$5fff)) then
        begin
         n_ram:=data and 15;
         exit;
        end;
        end;
   MBC2: begin
        if((address>=$2000) and (address<$4000) and (address and 256>0)) then
        begin
         data:=data and 15;
         set_bank(data);
         exit;
        end;
       end;
  end;
 end;


 if gb_mode=CGB then
 begin
  if ((address>=$d000) and (address<=$dfff)) then
  begin
   cr:=(m_iram[$ff70]) and 7;
   if(cr=0) then  cr:=1;
   m_cgbram[4096*cr+address-$d000]:=data;
   exit;
  end;

  if (address=$ff55) then
  begin
   if(data and 128=0) then
   begin
        if hdma>0 then hdma:=0  else
        begin
         adr_s:=((m_iram[$ff51]) shl 8) or (m_iram[$ff52] and 240);
         adr_p:=((((m_iram[$ff53]) and 31) shl 8) or (m_iram[$ff54] and 240)) or $8000;
         for ji:=0 to ((data and 127)*16+16)-1 do
          begin
           spokeb(adr_p,speekb(adr_s));
           inc(adr_p);inc(adr_s);
          end;
         inc(cnumber,data*32+928 div gb_speed);
         data:=$ff;
        end
   end
   else
   begin
        data:=data and 127;
        if(m_iram[$ff40] and 128>0) then
        begin
         hdma:=1;
         hdma_count:=data+1;
         adr_s:=((m_iram[$ff51]) shl 8) or (m_iram[$ff52] and 240);
         adr_p:=((((m_iram[$ff53]) and 31) shl 8) or (m_iram[$ff54] and 240)) or $8000;
        end;
   end;
  end;


 if((address>=$8000) and (address<$a000) and ((m_iram[$ff4f] and 1>0))) then
 begin
  m_iram2[address]:=data;
  exit;
 end;

 case address of
  $ff69: begin
          palramb[(m_iram[$ff68]) and 63]:=data;
          if(m_iram[$ff68] and 1)>0 then
          begin
           tempL:=((m_iram[$ff68]) and 56) shr 3;
           tempR:=((m_iram[$ff68]) and 6) shr 1;
           pal_b[templ][tempr][0]:=(data and 124) shl 1;
           pal_b[templ][tempr][1]:=pal_b[templ][tempr][1] and 63;
           pal_b[templ][tempr][1]:=pal_b[templ][tempr][1] or (data shl 6);
          end
          else
          begin
           templ:=((m_iram[$ff68]) and 56) shr 3;
           tempr:=((m_iram[$ff68]) and 6) shr 1;

           pal_b[templ][tempr][2]:=(data and 31) shl 3;
           pal_b[templ][tempr][1]:=pal_b[templ][tempr][1] and 192;
           pal_b[templ][tempr][1]:=pal_b[templ][tempr][1] or ((data and 224) shr 2);
          end;

        pal_dx_b[((m_iram[$ff68]) and 56) shr 3][((m_iram[$ff68]) and 6) shr 1]:=
         (((pal_b[((m_iram[$ff68]) and 56) shr 3][((m_iram[$ff68]) and 6) shr 1][2]) shr (8-dx_r)) shl dx_sr) or (((pal_b[((m_iram[$ff68]) and 56) shr 3][((m_iram[$ff68]) and 6) shr 1][1]) shr (8-dx_g)) shl dx_sg) or (((pal_b[((m_iram[$ff68]) and 56) shr 3][((m_iram[$ff68]) and 6) shr 1][0]) shr (8-dx_b)) shl dx_sb);
        if(m_iram[$ff68] and 128)>0 then
        begin
         m_iram[$ff68]:=m_iram[$ff68] and $3f;
         inc(m_iram[$ff68]);
         m_iram[$ff68]:=m_iram[$ff68] or 128;
        end;
      end;
  $ff6b: begin
   palramo[(m_iram[$ff6a]) and 63]:=data;
   if(m_iram[$ff6a] and 1)>0 then
   begin
    templ:=((m_iram[$ff6a]) and 56) shr 3;
    tempr:=((m_iram[$ff6a]) and 6) shr 1;

    pal_o[templ][tempr][0]:=(data and 124) shl 1;
    pal_o[templ][tempr][1]:=pal_o[templ][tempr][1] and 63;
    pal_o[templ][tempr][1]:=pal_o[templ][tempr][1] or (data shl 6);
   end
   else
   begin
    templ:=((m_iram[$ff6a]) and 56) shr 3;
    tempr:=((m_iram[$ff6a]) and 6) shr 1;

    pal_o[templ][tempr][2]:=(data and 31) shl 3;
    pal_o[templ][tempr][1]:=pal_o[templ][tempr][1] and 192;
    pal_o[templ][tempr][1]:=pal_o[templ][tempr][1] or ((data and 224) shr 2);
   end;
   pal_dx_o[((m_iram[$ff6a]) and 56) shr 3][((m_iram[$ff6a]) and 6) shr 1]:=(((pal_o[((m_iram[$ff6a]) and 56) shr 3][((m_iram[$ff6a]) and 6) shr 1][2]) shr (8-dx_r)) shl dx_sr) or (((pal_o[((m_iram[$ff6a]) and 56) shr 3][((m_iram[$ff6a]) and 6) shr 1][1]) shr (8-dx_g)) shl dx_sg) or (((pal_o[((m_iram[$ff6a]) and 56) shr 3][((m_iram[$ff6a]) and 6) shr 1][0]) shr (8-dx_b)) shl dx_sb);
   if(m_iram[$ff6a] and 128)>0 then
   begin
    m_iram[$ff6a]:=m_iram[$ff6a] and $3f;
    inc(m_iram[$ff6a]);
    m_iram[$ff6a]:=m_iram[$ff6a] or 128;
   end;
   end;
 end;
end;

if ((address>=$e000) and (address<$fe00)) then dec(address,$2000);

if (address>=$ff00) then
begin
case address of
 $ff00: begin
         if (data=3) then data:=$ff else
         if((data and $30)=$30) then data:=$ff
         else
         begin
          if((data and $30)=$20) then
            data:=$20 or (k_down shl 3) or (k_up shl 2) or (k_left shl 1) or k_right
           else
          if((data and $30)=$10) then
           data:=$10 or (k_start shl 3) or (k_select shl 2) or(k_b shl 1) or k_a;
         end;
        data:=data or $c0;
      end;
 $ff02: begin
         if((data and 128>0) and (data and 1>0)) then sio_count:=8;
         data:=data or 126;
         end;
 $ff04: begin
         data:=0;
         n_ff04:=0;
        end;
 $ff07: begin
         if(data and 4)>0 then
         begin
          c_count:=0;
          t_count:=m_iram[$ff06];
         end;
        end;
 $ff40: begin
         if(data and 128=0) then vbioff_count:=1 else
         if(m_iram[$ff40] and 128)=0 then
         begin
          old_mode:=5;
          cycles:=1;
          cur_c:=2;
          nr_t:=0;
         end;
        end;
 $ff0f: begin
         if(data and 16)>0 then set_ff0f(16);
         if(data and 8)>0 then set_ff0f(8);
         if(data and 4)>0 then set_ff0f(4);
         if(data and 2)>0 then set_ff0f(2);
         if(data and 1)>0 then set_ff0f(1);
        end;
 $ff10..$ff3F: begin
                sndRegChange:=true;
                if address=$ff26 then
                begin
                 m_iram[$ff26]:=data;
                 if data and $80=0 then
                 begin
                  snd[1].Enable:=false;
                  snd[2].Enable:=false;
                  snd[3].Enable:=false;
                  snd[4].Enable:=false;
                 end;
                 exit;
                end;
                m_iram[address]:=data;
                exit;
              end;

 $ff47: begin
         if(gb_mode<>CGB) then
         begin
          gb_mode:=CGB;
          spokeb($ff68,128);
          spokeb($ff69,(colors[data and 3]) and $ff);
          spokeb($ff69,(colors[data and 3]) shr 8);
          spokeb($ff69,(colors[(data and $c) shr 2]) and $ff);
          spokeb($ff69,(colors[(data and $c) shr 2]) shr 8);
          spokeb($ff69,(colors[(data and $30) shr 4]) and $ff);
          spokeb($ff69,(colors[(data and $30) shr 4]) shr 8);
          spokeb($ff69,(colors[(data and $c0) shr 6]) and $ff);
          spokeb($ff69,(colors[(data and $c0) shr 6]) shr 8);
          gb_mode:=DMG;
         end;
        end;
 $ff48: begin
         if(gb_mode<>CGB) then
         begin
          gb_mode:=CGB;
          spokeb($ff6a,128);
          spokeb($ff6b,(colors[data and 3]) and $ff);
          spokeb($ff6b,(colors[data and 3]) shr 8);
          spokeb($ff6b,(colors[(data and $c) shr 2]) and $ff);
          spokeb($ff6b,(colors[(data and $c) shr 2]) shr 8);
          spokeb($ff6b,(colors[(data and $30) shr 4]) and $ff);
          spokeb($ff6b,(colors[(data and $30) shr 4]) shr 8);
          spokeb($ff6b,(colors[(data and $c0) shr 6]) and $ff);
          spokeb($ff6b,(colors[(data and $c0) shr 6]) shr 8);
          gb_mode:=DMG;
         end;
        end;
 $ff49: begin
         if gb_mode<>CGB then
         begin
          gb_mode:=CGB;
          spokeb($ff6a,128+8);
          spokeb($ff6b,(colors[data and 3]) and $ff);
          spokeb($ff6b,(colors[data and 3]) shr 8);
          spokeb($ff6b,(colors[(data and $c) shr 2]) and $ff);
          spokeb($ff6b,(colors[(data and $c) shr 2]) shr 8);
          spokeb($ff6b,(colors[(data and $30) shr 4]) and $ff);
          spokeb($ff6b,(colors[(data and $30) shr 4]) shr 8);
          spokeb($ff6b,(colors[(data and $c0) shr 6]) and $ff);
          spokeb($ff6b,(colors[(data and $c0) shr 6]) shr 8);
          gb_mode:=DMG;
         end;
        end;
 $ff41: begin
         if((mode=0) or (mode=1)) then
         begin
          m_iram[$ff0f]:=m_iram[$ff0f] or 2;
          set_ff0f(2);
         end;
        end;
 $ff46: begin
         sAddr:=data shl 8;
         Offset:=$fe00;
         for pi:=0 to $9f do
         begin
          m_iram[Offset]:=speekb(sAddr);
          inc(Offset);inc(sAddr);
         end;
        end;

 end;
 end;

if(sram>0) and (address>=$a000) and (address<$c000) then
 m_ram[address-$a000+n_ram*$8912]:=data
else
 m_iram[address]:=data;
end;


procedure push_pc;assembler;
 asm
  mov ax,pc.w
  dec sp_.w
  xchg ah,al
  push eax
  push dword ptr sp_.w
  call spokeb
  lea esp,[esp+8]
  mov ax,pc.w
  push eax
  dec sp_.w
  push dword ptr sp_.w
  call spokeb
  lea esp,[esp+8]
 end;
(* dec(sp_.W,2);wordpoke(sp_.W,pc.w);*)


end.
