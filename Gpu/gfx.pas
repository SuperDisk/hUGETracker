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

procedure make_line_tile(nr: byte; offset: word);
procedure make_line_finish(nr: byte);

var
  tc: word;

implementation

uses vars;

procedure make_line_tile(nr: byte; offset: word);
var
  t_temp: DWord;
  tilemap, tile_addr, w1, w2, w3: word;
  buff_addr: DWord;
  tile_nr, attr, tile_fx: byte;
  nr_y, nr_x: byte;
  start_tiles: word;
  tempo, lop: integer;

begin
  tile_addr := 0;
  if (nr > 143) or (offset > 21) then
    exit;
  tc := offset;

  buff_addr := dx_bits * offset * 8 + 100;
  nr_y := (m_iram[$ff42] + nr);
  nr_x := m_iram[$ff43];
  start_tiles := ((nr_x shr 3) + offset) and 31;
  if m_iram[$ff40] and 8 > 0 then
    tilemap := $9c00
  else
    tilemap := $9800;
  tile_nr := m_iram[(tilemap + start_tiles + ((nr_y and 248) shl 2))];

  attr := m_iram2[(tilemap + start_tiles + ((nr_y and 248) shl 2))];

  if m_iram[$ff40] and 16 > 0 then
    asm
             MOV     AL,tile_nr
             MOVZX   AX,AL
             SHL     AX,4
             ADD     AX,$8000
             MOV     tile_addr,AX
    end
  else
    asm
             MOV     AL,tile_nr
             MOVSX   AX,AL
             SAL     AX,4
             ADD     AX,$9000
             MOV     tile_addr,AX
    end;

  if (attr and 64) > 0 then
    tile_addr := tile_addr + (7 - (nr_y - 8 * (nr_y shr 3))) * 2
  else
    tile_addr := tile_addr + (nr_y - 8 * (nr_y shr 3)) * 2;

  if attr and 8 = 0 then
  begin
    w1 := m_iram[tile_addr];
    Inc(tile_addr);
    w2 := m_iram[tile_addr];
  end
  else
  begin
    w1 := m_iram2[tile_addr];
    Inc(tile_addr);
    w2 := m_iram2[tile_addr];
  end;


  if attr and 32 > 0 then
    tile_fx := 1
  else
    tile_fx := 0;

  w3 := (w2 shl 8) or w1;
  for lop := 0 to 7 do
  begin
    tempo := pix[w3][lop][tile_fx];
    line_b_mask[buff_addr] := tempo;
    t_temp := pal_dx_b[attr and 7][tempo];
    case dx_bits of
      2: asm
                 MOV   EAX,t_temp
                 MOV   EDI,buff_addr
                 MOV   word ptr line_b[EDI],AX
                 LEA   EDI,[EDI+2]
                 MOV   buff_addr,EDI
        end;
      3: asm
                 MOV   EAX,t_temp
                 MOV   EBX,EAX
                 SHR   EAX,16
                 MOV   EDI,buff_addr
                 MOV   byte(line_b[EDI]),BL
                 MOV   byte(line_b[EDI+1]),BH
                 MOV   byte(line_b[EDI+2]),AL
                 LEA   EDI,[EDI+3]
                 MOV   buff_addr,EDI
        end;
      4: asm
                 MOV   EAX,t_temp
                 MOV   EDI,buff_addr
                 MOV   dword ptr line_b[EDI],EAX
                 LEA   EDI,[EDI+4]
                 MOV   buff_addr,EDI
        end;
    end;
  end;

  //  start_tiles:=(start_tiles+1) and 31;
end;

procedure make_line_finish(nr: byte);
var
  w1, w2, w3: word;
  t_temp: DWOrd;
  window_line, window_start: word;
  window_tile_map, temp_v, qpoi: word;
  buff_addr: DWORD;
  tile_nr: byte;
  attr: byte;
  tile_addr: word;
  tile_fx: byte;
  nr_x: byte;
  ioi, lop, strt: word;
  yu: DWord;
  s16, zmp, pix_temp: byte;

  coordx_s: word;
  coordy_s, wx, wy, l_numb, sprite_la, flipx, address_w: integer; // signed int
  address: word;
  aal: DWORD;
  buffer, ad2: pointer;
begin
  if nr > 143 then
    exit;
  while tc < 20 do
    make_line_tile(nr, tc + 1);//end;// !!! inc=??

  buff_addr := 0;
  nr_x := m_iram[$ff43];
  if (m_iram[$ff40] and 32 > 0) and (m_iram[$ff4a] <= nr) then
  begin
    window_line := nr - m_iram[$ff4a];
    temp_v := window_line shr 3;
    window_start := 0;
    if (m_iram[$ff40] and 64) > 0 then
      window_tile_map := $9c00
    else
      window_tile_map := $9800;

    for qpoi := 0 to 31 do
    begin
      tile_nr := m_iram[(window_tile_map + (temp_v shl 5) + window_start)];
      attr := m_iram2[(window_tile_map + (temp_v shl 5) + window_start)];
      if m_iram[$ff40] and 16 > 0 then
        asm
                 MOV     AL,tile_nr
                 MOVZX   AX,AL
                 SHL     AX,4
                 ADD     AX,$8000
                 MOV     tile_addr,AX
        end
      else
        asm
                 MOV     AL,tile_nr
                 MOVSX   AX,AL
                 SAL     AX,4
                 ADD     AX,$9000
                 MOV     tile_addr,AX
        end;

      if (attr and 64) > 0 then
        tile_addr := tile_addr + (7 - (window_line - 8 * temp_v)) * 2
      else
        tile_addr := tile_addr + (window_line - 8 * temp_v) * 2;
      if attr and 8 = 0 then
      begin
        w1 := m_iram[tile_addr];
        Inc(tile_addr);
        w2 := m_iram[tile_addr];
      end
      else
      begin
        w1 := m_iram2[tile_addr];
        Inc(tile_addr);
        w2 := m_iram2[tile_addr];
      end;

      w3 := (w2 shl 8) or w1;
      if attr and 32 > 0 then
        tile_fx := 1
      else
        tile_fx := 0;

      for lop := 0 to 7 do
      begin
        begin
          t_temp := pal_dx_b[attr and 7][pix[w3][lop][tile_fx]];
          case dx_bits of
            2: asm
                       MOV   EAX,t_temp
                       MOV   EDI,buff_addr
                       MOV   word ptr window_b[EDI],AX
                       LEA   EDI,[EDI+2]
                       MOV   buff_addr,EDI
              end;
            3: asm
                       MOV   EAX,t_temp
                       MOV   EBX,EAX
                       SHR   EAX,16
                       MOV   EDI,buff_addr
                       MOV   byte(window_b[EDI]),BL
                       MOV   byte(window_b[EDI+1]),BH
                       MOV   byte(window_b[EDI+2]),AL
                       LEA   EDI,[EDI+3]
                       MOV   buff_addr,EDI
              end;
            4: asm
                       MOV   EAX,t_temp
                       MOV   EDI,buff_addr
                       MOV   dword ptr window_b[EDI],EAX
                       LEA   EDI,[EDI+4]
                       MOV   buff_addr,EDI
              end;
          end;
        end;
      end;
      Inc(window_start);
      window_start := window_start and 31;
    end;

    strt := m_iram[$ff4b];
    Dec(strt, 7);

    if strt < 160 then
      for yu := 0 to ((160 - strt) * dx_bits) - 1 do
      begin
        line_b[(strt + (nr_x and 7)) * dx_bits + yu + 100] := window_b[yu];
        line_b_mask[(strt + (nr_x and 7)) * dx_bits + yu + 100] := window_b_mask[yu];
      end;
  end;

  if m_iram[$ff40] and 2 > 0 then
  begin
    s16 := 0;
    if (m_iram[$ff40] and 4) > 0 then
      s16 := 1;

    for ioi := 39 downto 0 do
    begin
      address := ioi * 4 + $fe00;
      wx := m_iram[address + 1];
      wy := m_iram[address];
      coordx_s := wx - 8;
      coordy_s := wy - 16;
      attr := m_iram[address + 3];
      if (gb_mode <> CGB) then
      begin
        attr := attr and $f0;
        if attr and $10 > 0 then
          attr := attr or 1;
      end;

      if (wx > 0) and (wy > 0) and (wx < 168) and (coordy_s <= nr) and
        (((s16 = 0) and (coordy_s >= nr - 7)) or ((s16 > 0) and
        (coordy_s >= nr - 15))) then
      begin
        l_numb := nr - coordy_s;
        if (m_iram[address + 3] and 64) > 0 then
          if s16 > 0 then
            l_numb := 15 - l_numb
          else
            l_numb := 7 - l_numb;
        if s16 = 0 then
          sprite_la := $8000 + m_iram[address + 2] * 16 + 2 * l_numb
        else
          sprite_la := $8000 + (m_iram[address + 2] and 254) * 16 + 2 * l_numb;
        if attr and 32 > 0 then
          flipx := 1
        else
          flipx := 0;
        if attr and 8 > 0 then
        begin
          w1 := m_iram2[sprite_la];
          Inc(sprite_la);
          w2 := m_iram2[sprite_la];
        end
        else
        begin
          w1 := m_iram[sprite_la];
          Inc(sprite_la);
          w2 := m_iram[sprite_la];
        end;
        w3 := (w2 shl 8) or w1;
        address_w := (coordx_s + (nr_x and 7)) * dx_bits + 100;
        for zmp := 0 to 7 do
        begin
          pix_temp := pix[w3][zmp][flipx];
          if ((((attr and 128 = 0)) or ((line_b_mask[address_w] = 0))) and (pix_temp > 0)) then
          begin
            t_temp := pal_dx_o[attr and 7][pix_temp];
            case dx_bits of
              2: asm
                         MOV   EAX,t_temp
                         MOV   EDI,address_w
                         MOV   word ptr line_b[EDI],AX
                         LEA   EDI,[EDI+2]
                         MOV   address_w,EDI
                end;
              3: asm
                         MOV   EAX,t_temp
                         MOV   EBX,EAX
                         SHR   EAX,16
                         MOV   EDI,address_w
                         MOV   byte(line_b[EDI]),BL
                         MOV   byte(line_b[EDI+1]),BH
                         MOV   byte(line_b[EDI+2]),AL
                         LEA   EDI,[EDI+3]
                         MOV   address_w,EDI
                end;
              4: asm
                         MOV   EAX,t_temp
                         MOV   EDI,address_w
                         MOV   dword ptr line_b[EDI],EAX
                         LEA   EDI,[EDI+4]
                         MOV   address_w,EDI
                end;
            end;
          end
          else
            Inc(address_w, dx_bits);
        end;
      end;
    end;
  end;
  aal := nr * dx_pitch;
  buffer := addr(line_b[100 + (nr_x and 7) * dx_bits]);
  ad2 := addr(dx_buffer[0]);
  asm
           MOV     ESI,dword ptr [buffer]
           MOV     EDI,dword ptr [ad2]
           ADD     EDI,aal
           MOV     ECX,cmov
           CLD
           REP     movsd
  end;
end;

end.
