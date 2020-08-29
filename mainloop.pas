{+-----------------------------------------------------------------------------+
 | Author:      Christian Hackbart
 | Description: Mainloop
 | Copyright (c) 2000 Christian Hackbart
 | Stand: 31.10.2000
 |
 | http://www.tu-ilmenau.de/~hackbart
 +----------------------------------------------------------------------------+}

unit mainloop;

{$MODE Delphi}

interface

uses ExtCtrls;

function main_loop(a: DWord): DWORD;

function z80_decode: byte; pascal;
procedure z80_reset;

implementation

uses vars, machine, z80cpu, sound;

procedure TimerControl;
(*

FF05 -- TIMECNT [RW] Timer Counter
        This register contains constantly increasing number. The timer
        interrupt occurs when this register overflows.

        ------------------------------------+---------------+---------------
FF07 -- TIMCONT [RW] Timer Control  | when set to 1 | when set to 0
Bit2    Start/Stop timer            | COUNTING      | STOPPED
Bit1-0  Timer clock select:
  00 - 4096Hz    01 - 262144Hz    10 - 65536Hz    11 - 16384Hz
------------------------------------+---------------+---------------

BIT:       7  6  5  4  3  2  1  0
value:   128 64 32 16 08 04 02 01

*)
begin
  case m_iram[$ff07] and 3 of // Timer clock select
    0:
    begin
      if (c_count >= 1024) then
      begin
        c_count := 0;
        Inc(t_count);
      end;
    end;
    1:
    begin
      if (c_count >= 16) then
      begin
        c_count := 0;
        Inc(t_count);
      end;
    end;
    2:
    begin
      if (c_count >= 64) then
      begin
        c_count := 0;
        Inc(t_count);
      end;
    end;
    3:
    begin
      if (c_count >= 256) then
      begin
        c_count := 0;
        Inc(t_count);
      end;
    end;
  end;

  m_iram[$ff05] := t_count;
  if (t_count > 255) then
  begin
    m_iram[$ff05] := m_iram[$ff06]; // TIMEMOD [RW] Timer Modulo
    t_count := m_iram[$ff06];
    set_ff0f(4);
  end;
end;

function main_loop(a: DWord): DWORD;
begin
  repeat
    z80_decode;
  until f_stopped;
  Result := 0;
end;

procedure LCDControl;
(*
------------------------------------------+---------------+---------------
FF40 -- LCDCONT [RW] LCD Control          | when set to 1 | when set to 0
Bit7  LCD operation                       | ON            | OFF
Bit6  Window Tile Table address           | 9C00-9FFF     | 9800-9BFF
Bit5  Window display                      | ON            | OFF
Bit4  Tile Pattern Table address          | 8000-8FFF     | 8800-97FF
Bit3  Background Tile Table address       | 9C00-9FFF     | 9800-9BFF
Bit2  Sprite size                         | 8x16          | 8x8
Bit1  Color #0 transparency in the window | SOLID         | TRANSPARENT
Bit0  Background display                  | ON            | OFF
------------------------------------------+---------------+---------------
*)
var
  JI: integer;
begin
  {if byte(pchar(cart)[$44]) and $80>0 then exit;}
  if (m_iram[$ff40] and 128) = 0 then
  begin // LCDCONT=0
    make_line_count := -1;
    make_line_ccount := 0;
    m_iram[$ff41] := m_iram[$ff41] and 252;
    nr_t := 0;
    cur_c := 3;
    cycles := 0;
    m_iram[$ff44] := 0;
    mode := 5;
    cycles := 0;
    Inc(vbioff_count);
    vbioff_count := vbioff_count mod full_cycles;
    if (vbioff_count = 0) then
      set_ff0f(1);
  end
  else // LCDCONT=1
  if (cur_c >= cycles) then
  begin
    cur_c := (cur_c - cycles) mod 4;
    old_mode := mode;
    Inc(nr_t);
    mode := c_tab[nr_t][0];
    if (mode = 9) then
    begin
      mode := 2;
      nr_t := 0;
    end;
    case mode of
      2:
      begin
        make_line_count := -1;
        cycles := 82;
        m_iram[$ff41] := (m_iram[$ff41] and 252) or 2;
        m_iram[$ff44] := c_tab[nr_t][1];
        if (m_iram[$ff44] = m_iram[$ff45]) then
        begin
          m_iram[$ff41] := m_iram[$ff41] or 4;
          if (m_iram[$ff41] and 64) > 0 then
            set_ff0f(2);
        end
        else
          m_iram[$ff41] := m_iram[$ff41] and 251;
        if (m_iram[$ff41] and 32) > 0 then
          set_ff0f(2);
      end;
      3:
      begin
        make_line_count := 0;
        make_line_ccount := cnumber;
        if (spriten > 10) then
          spriten := 10;
        cycles := (175 + 12 * spriten);
        m_iram[$ff41] := (m_iram[$ff41] and 252) or 3;
      end;
      0:
      begin
        //make_line_finish(m_iram[$ff44]);
        make_line_count := -1;
        if hdma > 0 then
        begin
          for ji := 0 to 15 do
          begin
            spokeb(adr_p, speekb(adr_s));
            Inc(adr_p);
            Inc(adr_s);
          end;
          Dec(hdma_count);
          m_iram[$ff55] := hdma_count - 1;
          if hdma_count = 0 then
          begin
            hdma := 0;
            m_iram[$ff55] := $ff;
          end;
        end;
        cycles := 377 - cycles;
        m_iram[$ff41] := m_iram[$ff41] and 252;
        if (m_iram[$ff41] and 8) > 0 then
          set_ff0f(2);
      end;
      1:
      begin
        cycles := 152 * 3;
        m_iram[$ff44] := c_tab[nr_t][1];
        if (m_iram[$ff44] = m_iram[$ff45]) then
        begin
          m_iram[$ff41] := m_iram[$ff41] or 4;
          if (m_iram[$ff41] and 64) > 0 then
            set_ff0f(2);
        end
        else
          m_iram[$ff41] := m_iram[$ff41] and 251;
        if ((m_iram[$ff41] and 32 > 0) and (m_iram[$ff40] and 128 > 0)) then
          set_ff0f(2);
        if (old_mode = 0) then
        begin
          //make_line_finish(143);
          //if (m_iram[$ff40] and 128) > 0 then
             //PaintBox.Repaint;
          vbi_count := vbi_latency;
        end;
        make_line_count := -1;
        m_iram[$ff41] := (m_iram[$ff41] and 252) or 1;
        if (m_iram[$ff41] and 16) > 0 then
          set_ff0f(2);
      end;
    end;
  end;
end;

procedure IFlags;
(*
 ----------------------------------------------+---+---
 FF0F -- IFLAGS [RW] Interrupt Flags           | 1 | 0
 Bit4  Transition High->Low on pins P10-P13    | Y | N
 Bit3  End of serial I/O transfer              | Y | N
 Bit2  Timer overflow                          | Y | N
 Bit1  LCD controller interrupt [see LCDSTAT]  | Y | N
 Bit0  LCD vertical blanking impulse           | Y | N
 ----------------------------------------------+---+---
*)
begin
  gbr_ime := False;
  if halt_mode = 1 then
  begin
    Inc(pc.w);
    halt_mode := 0;
  end;
  push_pc;

  if (m_iram[$ff0f] and 16) > 0 then
  begin
    pc.w := $60;
    m_iram[$ff0f] := m_iram[$ff0f] and 239;
  end
  else
  if (m_iram[$ff0f] and 8) > 0 then
  begin
    pc.w := $58;
    m_iram[$ff0f] := m_iram[$ff0f] and 247;
  end
  else
  if (m_iram[$ff0f] and 4) > 0 then
  begin
    pc.w := $50;
    m_iram[$ff0f] := m_iram[$ff0f] and 251;
  end
  else
  if (m_iram[$ff0f] and 2) > 0 then
  begin
    pc.w := $48;
    m_iram[$ff0f] := m_iram[$ff0f] and 253;
  end
  else
  if (m_iram[$ff0f] and 1) > 0 then
  begin
    pc.w := $40;
    m_iram[$ff0f] := m_iram[$ff0f] and 254;
  end;
end;

const
  cycles_per_hblank = 451;

function z80_decode: byte;
var
  Count: byte;
begin
  if (gbr_ime) and (m_iram[$ff0f] and 31 > 0) then
    IFlags; // Start Interupt

  pc_old := pc;
  code := speekb(pc.w);
  Inc(pc.w);

  if halt_mode = 2 then
  begin
    halt_mode := 0;
    Dec(pc.w);
  end;

  if (code = $cb) then
  begin
    code := speekb(pc.w) or 256;
    Inc(pc.w);
  end;

  asm
     CLC
  end; // Set the Carry-Flag to zero
  if (RomName[2] <> 'A') and (code <> 118) then Writeln('OP ', code);
  Count := z80[code];
  if (RomName[2] <> 'A') and (code <> 118) then Writeln('REGS pc=', pc.w, ' sp=', sp_.w, ' af=', af.w, ' hl=', hl.w, ' bc=', bc.w, ' de=', de.w);
  SoundUpdate(Count * (3 - gb_speed));
  cnumber := Count div gb_speed;

  if (stop_mode = 1) then
  begin
    stop_mode := 0;
    if ((m_iram[$ff4d]) and 1) > 0 then
    begin
      if (gb_speed = 1) then
      begin
        gb_speed := 2;
        m_iram[$ff4d] := m_iram[$ff4d] or 128;
      end
      else
      begin
        gb_speed := 1;
        m_iram[$ff4d] := m_iram[$ff4d] and 127;
      end;
    end;
  end;

  if make_line_count >= 0 then
  begin
    Inc(make_line_ccount, cnumber);
    if make_line_ccount >= 8 then
    begin
      //make_line_tile(m_iram[$ff44], make_line_count);
      Inc(make_line_count);
      Dec(make_line_ccount, 8);
    end;
  end;

  Inc(cur_c, cnumber);
  Inc(n_ff04, cnumber);

  if (n_ff04 > 255) then
  begin
    {$PUSH}{$R-}{$Q-}
    Dec(m_iram[$ff04]); // DIVIDER [RW] Divider
    {$POP}
    // This register is incremented 16384 times
    // a second. Writing any value sets it to $00.

    n_ff04 := 0;
  end;

  if (vbi_count <> 9879) then
  begin
    Dec(vbi_count, cnumber);
    if (vbi_count <= 0) then
    begin
      vbi_count := 9879;
      set_ff0f(1);
    end;
  end;

  if (m_iram[$ff07] and 4) > 0 then
    Inc(c_count, cnumber); // Start timer

  if (halt_mode = 1) then
    Dec(pc.w);

  case ei_fix of
    1:
    begin
      ei_fix := 0;
      gbr_ime := True;
    end;
    2: Dec(ei_fix);
  end;

  case di_fix of
    1:
    begin
      di_fix := 0;
      gbr_ime := False;
    end;
    2: Dec(di_fix);
  end;

  LCDControl;

  if ((m_iram[$ff07]) and 4) > 0 then
    TimerControl;

  if (sio_count > 0) then
  begin
    m_iram[$ff01] := (m_iram[$ff01] shl 1) or 1; // SIODATA Serial I/O Data
    Dec(sio_count);
    if sio_count = 0 then
    begin
      m_iram[$ff02] := m_iram[$ff02] and 127;
      set_ff0f(8);
    end;
  end;

  Result := Count;
end;

procedure z80_reset;
var
  yrr: byte;
begin
  stop_mode := 0;
  gb_speed := 1;
  vbi_count := 9879;
  hdma_count := 0;
  hdma := 0;
  bc.w := 0;
  de.w := $ff56;
  hl.w := $d;
  pc.w := $100;
  sp_.w := $fffe;
  af.l := 128;
  sio_count := 0;
  vbioff_count := 0;
  fillchar(m_iram, sizeof(m_iram), 0);
  fillchar(m_iram2, sizeof(m_iram2), 0);
  fillchar(m_ram, sizeof(m_ram), 0);
  fillchar(m_cgbram, sizeof(m_cgbram), 0);
  spokeb($ff44, $94);
  spokeb($ff01, 0);
  spokeb($ff02, $7f);
  spokeb($ff04, $af);
  spokeb($ff05, 0);
  spokeb($ff06, 0);
  spokeb($ff07, 0);
  spokeb($ff0f, $e1);
  {spokeb($ff10, $80);
  spokeb($ff11, $bf);
  spokeb($ff12, $f3);
  spokeb($ff14, $bf);}
  spokeb($ff16, $3f);
  spokeb($ff17, 0);
  spokeb($ff19, $bf);
  spokeb($ff1a, $7f);
  spokeb($ff1b, $ff);
  spokeb($ff1c, $9f);
  spokeb($ff1e, $bf);
  spokeb($ff20, $ff);
  spokeb($ff21, 0);
  spokeb($ff22, 0);
  spokeb($ff23, $bf);
  spokeb($ff24, $77);
  spokeb($ff25, $f3);
  spokeb($ff26, $f1);
  spokeb($ff40, $91);
  spokeb($ff41, $81);
  spokeb($ff42, 0);
  spokeb($ff43, 0);
  m_iram[$ff44] := 0;
  spokeb($ff45, 0);
  spokeb($ff4a, 0);
  spokeb($ff4b, 0);
  m_iram[$ff4c] := $fe;
  m_iram[$ff4d] := $7e;
  m_iram[$ff4e] := $ff;
  m_iram[$ff4f] := $fe;
  m_iram[$ff50] := $fe;
  m_iram[$ff51] := 0;
  m_iram[$ff52] := 0;
  m_iram[$ff53] := 0;
  m_iram[$ff54] := 0;
  m_iram[$ff55] := $ff;
  m_iram[$ff56] := 0;
  m_iram[$ff70] := 0;
  make_line_count := -1;
  make_line_ccount := 0;
  addr_bank := 0;
  rom_priv := 0;
  spokeb($ffff, 0);
  di_fix := 0;
  ei_fix := 0;
  old_mode := 1;
  cycles := 1;
  machine_cycles := 0; // important for hblank
  cur_c := 3;
  nr_t := 0;
  n_ff04 := 0;
  mode := 2;
  halt_mode := 0;
  gbr_ime := False;
  n_ram := 0;
  mbc1_type := 0;
  c_count := 0;
  t_count := 0;
  gb_mode := CGB;
  spokeb($ff68, 128);
  for yrr := 0 to 31 do
  begin
    spokeb($ff69, $ff);
    spokeb($ff69, $7f);
  end;
  spokeb($ff6a, 128);
  spokeb($ff6b, $00);
  spokeb($ff6b, $00);
  spokeb($ff6b, $27);
  spokeb($ff6b, $AF);
  spokeb($ff6b, $81);
  spokeb($ff6b, $49);
  spokeb($ff6b, $E9);
  spokeb($ff6b, $3C);
  spokeb($ff6b, $AC);
  spokeb($ff6b, $28);
  spokeb($ff6b, $53);
  spokeb($ff6b, $FE);
  spokeb($ff6b, $81);
  spokeb($ff6b, $51);
  spokeb($ff6b, $7D);
  spokeb($ff6b, $67);
  spokeb($ff6b, $04);
  spokeb($ff6b, $04);
  spokeb($ff6b, $DF);
  spokeb($ff6b, $BF);
  spokeb($ff6b, $09);
  spokeb($ff6b, $01);
  spokeb($ff6b, $EF);
  spokeb($ff6b, $FF);
  spokeb($ff6b, $0C);
  spokeb($ff6b, $03);
  spokeb($ff6b, $FC);
  spokeb($ff6b, $77);
  spokeb($ff6b, $08);
  spokeb($ff6b, $69);
  spokeb($ff6b, $FE);
  spokeb($ff6b, $DB);
  spokeb($ff6b, $0C);
  spokeb($ff6b, $84);
  spokeb($ff6b, $E9);
  spokeb($ff6b, $7F);
  spokeb($ff6b, $0C);
  spokeb($ff6b, $61);
  spokeb($ff6b, $1F);
  spokeb($ff6b, $DF);
  spokeb($ff6b, $00);
  spokeb($ff6b, $80);
  spokeb($ff6b, $7E);
  spokeb($ff6b, $7E);
  spokeb($ff6b, $04);
  spokeb($ff6b, $80);
  spokeb($ff6b, $58);
  spokeb($ff6b, $76);
  spokeb($ff6b, $9B);
  spokeb($ff6b, $9C);
  spokeb($ff6b, $C3);
  spokeb($ff6b, $D6);
  spokeb($ff6b, $12);
  spokeb($ff6b, $4C);
  spokeb($ff6b, $CD);
  spokeb($ff6b, $EC);
  spokeb($ff6b, $3B);
  spokeb($ff6b, $26);
  spokeb($ff6b, $7B);
  spokeb($ff6b, $FA);
  spokeb($ff6b, $88);
  spokeb($ff6b, $48);
  spokeb($ff6b, $FF);
  spokeb($ff6b, $F2);
  spokeb($ff47, $fc);
  spokeb($ff48, $ff);
  spokeb($ff49, $ff);
  if cart <> nil then
    if (PChar(cart)[$143] in [#$80, #$c0]) then
    begin
      af.h := $11;
      gb_mode := CGB;
    end
    else
    begin
      af.h := 1;
      gb_mode := DMG;
    end;
end;

end.
