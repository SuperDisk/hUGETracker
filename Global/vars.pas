{+ all global variables concerning CPU,Graphic and the other +
 + stuff are stored in this unit                             +}

unit vars;

{$MODE Delphi}

interface

type
  TFDCallback = procedure of object;

const
  thr_f: integer = 1;
  cart: pointer = nil; // Cartridge
  isddraw: boolean = True; // DirectDraw;

var
  FDCallback: TFDCallback;

  bdrop, dirname, dirload: string;
  romname, speicher: string[128];
  buffer: array[0..1023] of char;

  th: integer;
  dw, dh, tid: DWORD;

  // Graphics
  dx_buffer: array[0..(160 * 160 * 4 * 10) - 1] of byte;

const
  dx_bits: DWORD = 0;
  dx_r: DWORD = 0;
  dx_g: DWORD = 0;
  dx_b: DWORD = 0;
  dx_sr: DWORD = 0;

  dx_sg: DWORD = 0;
  dx_sb: DWORD = 0;
  dx_pitch: longint = 0;
  cmov: DWORD = 0;

  vbi_latency = 44;
  di_fix: integer = 0;
  ei_fix: integer = 0;
  n_ram: dword = 0;
  mbc1_type: dword = 0;
  gb_mode: byte = 0;
  stop_mode: byte = 0;
  halt_mode: byte = 0;

  // Keyboard :

  k_down: byte = 1;
  k_up: byte = 1;
  k_left: byte = 1;
  k_right: byte = 1;
  k_start: byte = 1;
  k_select: byte = 1;
  k_a: byte = 1;
  k_b: byte = 1;
  code: word = 0;

  cur_c: word = 0;
  mode: DWORD = 0;
  vbi_count: integer = 0;

  ROM = 1;
  MBC1 = 2;
  MBC2 = 3;
  MBC3 = 4;
  MBC5 = 5;
  full_cycles = 70224;
  CGB = 1;
  DMG = 0;
  f_stopped: boolean = False;
  colors: array[0..3] of word = ($7fff, $4210, $2108, 0); // GBColor

  // Speed functions, the timervar is increased each 60Hz
  lastTime: DWord = 0;
  perfFreq: DWord = 0;

type
  pair = record
    case boolean of
      False: (l, h: byte);
      True: (W: word);
  end;
  quadruple = record
    case byte of
      0: (b1, b2, b3, b4: byte);
      1: (w1, w2: word);
      2: (q: longint);
  end;

var
  gbr_ime: boolean;
  sp_, pc, pc_old: pair;
  af, bc, de, hl: pair;

  m_iram, m_iram2: array[0..65535] of byte;
  m_ram: array[0..40000 - 1] of byte;
  memc: integer;

  cycles: word;
  machine_cycles: longint;

  spriten, n_ff04: DWord;
  cnumber: byte;

  c_count, t_count: DWord;
  addr_bank, rom_priv: DWord;
  cart_type, srom, sram: word;
  old_mode, nr_t, vbioff_count: Dword;
  sio_count: byte;

  m_cgbram: array[0..32767] of byte;
  pal_b, pal_o: array[0..7, 0..3, 0..2] of byte;
  pal_dx_b, pal_dx_o: array[0..7, 0..3] of DWord;
  palramb, palramo: array[0..16383] of byte;
  hdma: DWord;
  gb_speed: byte;
  hdma_count, make_line_count, make_line_ccount: integer;
  adr_s, adr_p: Dword;

  line_b, line_b_mask, window_b, window_b_mask: array[0..15120 * 4 - 1] of byte;
  pix: array[0..65535, 0..7, 0..1] of byte;

  cartsize: Dword;

const
  c_tab: array[0..461, 0..1] of byte =
    (
    (2, 0), (3, 0), (0, 0),
    (2, 1), (3, 1), (0, 1),
    (2, 2), (3, 2), (0, 2),
    (2, 3), (3, 3), (0, 3),
    (2, 4), (3, 4), (0, 4),
    (2, 5), (3, 5), (0, 5),
    (2, 6), (3, 6), (0, 6),
    (2, 7), (3, 7), (0, 7),
    (2, 8), (3, 8), (0, 8),
    (2, 9), (3, 9), (0, 9),
    (2, 10), (3, 10), (0, 10),
    (2, 11), (3, 11), (0, 11),
    (2, 12), (3, 12), (0, 12),
    (2, 13), (3, 13), (0, 13),
    (2, 14), (3, 14), (0, 14),
    (2, 15), (3, 15), (0, 15),
    (2, 16), (3, 16), (0, 16),
    (2, 17), (3, 17), (0, 17),
    (2, 18), (3, 18), (0, 18),
    (2, 19), (3, 19), (0, 19),
    (2, 20), (3, 20), (0, 20),
    (2, 21), (3, 21), (0, 21),
    (2, 22), (3, 22), (0, 22),
    (2, 23), (3, 23), (0, 23),
    (2, 24), (3, 24), (0, 24),
    (2, 25), (3, 25), (0, 25),
    (2, 26), (3, 26), (0, 26),
    (2, 27), (3, 27), (0, 27),
    (2, 28), (3, 28), (0, 28),
    (2, 29), (3, 29), (0, 29),
    (2, 30), (3, 30), (0, 30),
    (2, 31), (3, 31), (0, 31),
    (2, 32), (3, 32), (0, 32),
    (2, 33), (3, 33), (0, 33),
    (2, 34), (3, 34), (0, 34),
    (2, 35), (3, 35), (0, 35),
    (2, 36), (3, 36), (0, 36),
    (2, 37), (3, 37), (0, 37),
    (2, 38), (3, 38), (0, 38),
    (2, 39), (3, 39), (0, 39),
    (2, 40), (3, 40), (0, 40),
    (2, 41), (3, 41), (0, 41),
    (2, 42), (3, 42), (0, 42),
    (2, 43), (3, 43), (0, 43),
    (2, 44), (3, 44), (0, 44),
    (2, 45), (3, 45), (0, 45),
    (2, 46), (3, 46), (0, 46),
    (2, 47), (3, 47), (0, 47),
    (2, 48), (3, 48), (0, 48),
    (2, 49), (3, 49), (0, 49),
    (2, 50), (3, 50), (0, 50),
    (2, 51), (3, 51), (0, 51),
    (2, 52), (3, 52), (0, 52),
    (2, 53), (3, 53), (0, 53),
    (2, 54), (3, 54), (0, 54),
    (2, 55), (3, 55), (0, 55),
    (2, 56), (3, 56), (0, 56),
    (2, 57), (3, 57), (0, 57),
    (2, 58), (3, 58), (0, 58),
    (2, 59), (3, 59), (0, 59),
    (2, 60), (3, 60), (0, 60),
    (2, 61), (3, 61), (0, 61),
    (2, 62), (3, 62), (0, 62),
    (2, 63), (3, 63), (0, 63),
    (2, 64), (3, 64), (0, 64),
    (2, 65), (3, 65), (0, 65),
    (2, 66), (3, 66), (0, 66),
    (2, 67), (3, 67), (0, 67),
    (2, 68), (3, 68), (0, 68),
    (2, 69), (3, 69), (0, 69),
    (2, 70), (3, 70), (0, 70),
    (2, 71), (3, 71), (0, 71),
    (2, 72), (3, 72), (0, 72),
    (2, 73), (3, 73), (0, 73),
    (2, 74), (3, 74), (0, 74),
    (2, 75), (3, 75), (0, 75),
    (2, 76), (3, 76), (0, 76),
    (2, 77), (3, 77), (0, 77),
    (2, 78), (3, 78), (0, 78),
    (2, 79), (3, 79), (0, 79),
    (2, 80), (3, 80), (0, 80),
    (2, 81), (3, 81), (0, 81),
    (2, 82), (3, 82), (0, 82),
    (2, 83), (3, 83), (0, 83),
    (2, 84), (3, 84), (0, 84),
    (2, 85), (3, 85), (0, 85),
    (2, 86), (3, 86), (0, 86),
    (2, 87), (3, 87), (0, 87),
    (2, 88), (3, 88), (0, 88),
    (2, 89), (3, 89), (0, 89),
    (2, 90), (3, 90), (0, 90),
    (2, 91), (3, 91), (0, 91),
    (2, 92), (3, 92), (0, 92),
    (2, 93), (3, 93), (0, 93),
    (2, 94), (3, 94), (0, 94),
    (2, 95), (3, 95), (0, 95),
    (2, 96), (3, 96), (0, 96),
    (2, 97), (3, 97), (0, 97),
    (2, 98), (3, 98), (0, 98),
    (2, 99), (3, 99), (0, 99),
    (2, 100), (3, 100), (0, 100),
    (2, 101), (3, 101), (0, 101),
    (2, 102), (3, 102), (0, 102),
    (2, 103), (3, 103), (0, 103),
    (2, 104), (3, 104), (0, 104),
    (2, 105), (3, 105), (0, 105),
    (2, 106), (3, 106), (0, 106),
    (2, 107), (3, 107), (0, 107),
    (2, 108), (3, 108), (0, 108),
    (2, 109), (3, 109), (0, 109),
    (2, 110), (3, 110), (0, 110),
    (2, 111), (3, 111), (0, 111),
    (2, 112), (3, 112), (0, 112),
    (2, 113), (3, 113), (0, 113),
    (2, 114), (3, 114), (0, 114),
    (2, 115), (3, 115), (0, 115),
    (2, 116), (3, 116), (0, 116),
    (2, 117), (3, 117), (0, 117),
    (2, 118), (3, 118), (0, 118),
    (2, 119), (3, 119), (0, 119),
    (2, 120), (3, 120), (0, 120),
    (2, 121), (3, 121), (0, 121),
    (2, 122), (3, 122), (0, 122),
    (2, 123), (3, 123), (0, 123),
    (2, 124), (3, 124), (0, 124),
    (2, 125), (3, 125), (0, 125),
    (2, 126), (3, 126), (0, 126),
    (2, 127), (3, 127), (0, 127),
    (2, 128), (3, 128), (0, 128),
    (2, 129), (3, 129), (0, 129),
    (2, 130), (3, 130), (0, 130),
    (2, 131), (3, 131), (0, 131),
    (2, 132), (3, 132), (0, 132),
    (2, 133), (3, 133), (0, 133),
    (2, 134), (3, 134), (0, 134),
    (2, 135), (3, 135), (0, 135),
    (2, 136), (3, 136), (0, 136),
    (2, 137), (3, 137), (0, 137),
    (2, 138), (3, 138), (0, 138),
    (2, 139), (3, 139), (0, 139),
    (2, 140), (3, 140), (0, 140),
    (2, 141), (3, 141), (0, 141),
    (2, 142), (3, 142), (0, 142),
    (2, 143), (3, 143), (0, 143),
    (1, 144), (1, 145), (1, 146),
    (1, 147), (1, 148), (1, 149),
    (1, 150), (1, 151), (1, 152),
    (1, 153), (9, 9), (9, 9), (9, 9),
    (0, 0), (0, 0), (0, 0), (0, 0),
    (0, 0), (0, 0), (0, 0), (0, 0),
    (0, 0), (0, 0), (0, 0), (0, 0),
    (0, 0), (0, 0), (0, 0), (0, 0),
    (0, 0));

(*

Gameboy Memory map
--------------------------- FFFF  | 32kB ROMs are non-switchable and occupy
 I/O ports + internal RAM         | 0000-7FFF are. Bigger ROMs use one of two
--------------------------- FF00  | different bank switches. The type of a
 Internal RAM                     | bank switch can be determined from the
--------------------------- C000  | internal info area located at 0100-014F
 8kB switchable RAM bank          | in each cartridge.
--------------------------- A000  |
 16kB VRAM                        | MBC1 (Memory Bank Controller 1):
--------------------------- 8000  | Writing a value into 2000-3FFF area will
 16kB switchable ROM bank         | select an appropriate ROM bank at
--------------------------- 4000  | 4000-7FFF. Writing a value into 4000-5FFF
 16kB ROM bank #0                 | area will select an appropriate RAM bank
--------------------------- 0000  | at A000-C000.
                                  |
                                  | MBC2 (Memory Bank Controller 2):
                                  | Writing a value into 2100-21FF area will
                                  | select an appropriate ROM bank at
                                  | 4000-7FFF. RAM switching is not provided.
*)

procedure load(Name: string);
function ReadGameState(filename: string): byte;
function SaveGameState(filename: string): boolean;

procedure make_pix;
function finde(Data: DWord): DWord;
function zaehle(Data: DWord): Dword;

implementation

function finde(Data: DWord): DWord;
var
  res: DWord;
begin
  if Data = 0 then
    Result := 0
  else
  begin
    res := 0;
    while True do
    begin
      if (Data and 1) > 0 then
      begin
        Result := res;
        exit;
        exit;
      end;
      Data := Data shr 1;
      Inc(res);
    end;
  end;
end;

function zaehle(Data: DWord): Dword;
var
  res: DWord;
  i: integer;
begin
  res := 0;
  for i := 0 to 31 do
  begin
    if (Data and 1) > 0 then
      Inc(res);
    Data := Data shr 1;
  end;
  Result := res;
end;

procedure load(Name: string);
var
  cf: file;
  m_rom: array[0..$180 - 1] of byte;
  i, c1: byte;
  S: string;
begin
  assignfile(cf, Name);
  reset(cf, 1);
  blockread(cf, {%H-}m_rom, $180);
  closefile(cf);
  buffer := '                ';
  for i := 0 to 15 do
  begin
    c1 := m_rom[$134 + i];
    if (c1 = $80) or (c1 = $c0) then
      c1 := 0;
    buffer[i] := chr(c1);
    buffer[i + 1] := #0;
    if buffer[i] = #0 then
      break;
  end;
  romname := '';
  i := 0;
  repeat
    romname := romname + buffer[i];
    Inc(i);
  until buffer[i] = #0;
  case m_rom[$147] of
    0:
    begin
      s := ' ROM ONLY';
      memc := ROM;
    end;
    1:
    begin
      s := ' MBC1,';
      memc := MBC1;
    end;
    2:
    begin
      s := ' MBC1+RAM,';
      memc := MBC1;
    end;
    3:
    begin
      s := ' MBC1+RAM+BAT,';
      memc := MBC1;
    end;
    5:
    begin
      s := ' MBC2,';
      memc := MBC2;
    end;
    6:
    begin
      s := ' MBC2+BAT,';
      memc := MBC2;
    end;
    8:
    begin
      s := ' ROM+RAM,';
      memc := ROM;
    end;
    9:
    begin
      s := ' ROM+RAM+BAT,';
      memc := ROM;
    end;
    $f:
    begin
      s := ' MBC3+TMR+BAT,';
      memc := MBC3;
    end;
    $10:
    begin
      s := ' MBC3+RAM+TMR+BAT,';
      memc := MBC3;
    end;
    $11:
    begin
      s := ' MBC3,';
      memc := MBC3;
    end;
    $12:
    begin
      s := ' MBC3+RAM,';
      memc := MBC3;
    end;
    $13:
    begin
      s := ' MBC3+RAM+BAT,';
      memc := MBC3;
    end;
    $19:
    begin
      s := ' MBC5,';
      memc := MBC5;
    end;
    $1a:
    begin
      s := ' MBC5+RAM,';
      memc := MBC5;
    end;
    $1b:
    begin
      s := ' MBC5+RAM+BAT,';
      memc := MBC5;
    end;
    $1c:
    begin
      s := ' MBC5+RP,';
      memc := MBC5;
    end;
    $1d:
    begin
      s := ' MBC5+RP+RAM,';
      memc := MBC5;
    end;
    $1e:
    begin
      s := ' MBC5+RP+RAM+BAT,';
      memc := MBC5;
    end;
  end;
  case m_rom[$148] of
    0:
    begin
      s := s + ' 32 KB ROM,';
      srom := 2;
    end;
    1:
    begin
      s := s + ' 64 KB ROM,';
      srom := 4;
    end;
    2:
    begin
      s := s + ' 128 KB ROM,';
      srom := 8;
    end;
    3:
    begin
      s := s + ' 256 KB ROM,';
      srom := 16;
    end;
    4:
    begin
      s := s + ' 512 KB ROM,';
      srom := 32;
    end;
    5:
    begin
      s := s + ' 1 MB ROM,';
      srom := 64;
    end;
    6:
    begin
      s := s + ' 2 MB ROM,';
      srom := 128;
    end;
    7:
    begin
      s := s + ' 4 MB EOM,';
      srom := 256;
    end;
    $52:
    begin
      s := s + ' 1.1 MB ROM,';
      srom := 72;
    end;
    $53:
    begin
      s := s + ' 1.2 MB ROM,';
      srom := 80;
    end;
    $54:
    begin
      s := s + ' 1.5 MB ROM,';
      srom := 96;
    end;
  end;
  case m_rom[$149] of
    0:
    begin
      s := s + ' 0 KB RAM';
      sram := 0;
    end;
    1:
    begin
      s := s + ' 2 KB RAM';
      sram := 1;
    end;
    2:
    begin
      s := s + ' 8 KB RAM';
      sram := 1;
    end;
    3:
    begin
      s := s + ' 32 KB RAM';
      sram := 4;
    end;
    4:
    begin
      s := s + ' 128 KB RAM';
      sram := 16;
    end;
  end;
  if cart <> nil then
    freemem(cart, cartsize);
  cartsize := srom * 16384;
  assignfile(cf, Name);
  reset(cf, 1);
  getmem(cart, cartsize);
  blockread(cf, cart^, srom * 16384);
  closefile(cf);
  f_stopped := False;
  speicher := s;
  // GetCurrentDirectory(sizeof(dirload),dirload);
end;

function SaveGameState(filename: string): boolean;
var
  f: file;
  numwritten, fsize: DWord;
begin
  if gb_mode = cgb then
    fsize := 32 * 1024
  else
    fsize := 8 * 1024;
 { case byte(pchar(cart)[$149]) of
  0: fsize:=       0;
  1: fsize:=  2*1024;
  2: fsize:=  8*1024;
  3: fsize:= 32*1024;
  4: fsize:=128*1024;
 end;}
  if fsize = 0 then
  begin
    Result := True;
    exit;
  end; // NO RAM?
 {$i-}
  assignfile(f, filename);
  rewrite(f, 1);
{$i+}
  if ioresult <> 0 then
  begin
    Result := False;
    exit;
  end;
  blockwrite(f, m_ram, fsize, {%H-}numwritten);
  closefile(f);
  Result := fsize = numwritten;
end;

function ReadGameState(filename: string): byte;
var
  f: file;
  numread, fsize: DWord;
begin
  if gb_mode = cgb then
    fsize := 32 * 1024
  else
    fsize := 8 * 1024;
 {case byte(pchar(cart)[$149]) of
  0: fsize:=       0;
  1: fsize:=  2*1024;
  2: fsize:=  8*1024;
  3: fsize:= 32*1024;
  4: fsize:=128*1024;
 end;}
  if fsize = 0 then
  begin
    Result := 1;
    exit;
  end; // NO RAM?
 {$i-}
  assignfile(f, filename);
  reset(f, 1);
{$i+}
  if ioresult <> 0 then
  begin
    Result := 2;
    exit;
  end;// not found
  if fsize = filesize(f) then
  begin
    blockread(f, m_ram, fsize);
    Result := 0; // ok
  end
  else
    Result := 3; // no valid savegame
  closefile(f);
end;

procedure make_pix;
var
  o0, o1: word;
  zm: word;
begin
  for zm := 0 to 65535 do
  begin
    o0 := zm shr 8;
    o1 := zm and $ff;
    pix[zm][7][0] := ((o0 and 1) shl 1) or (o1 and 1);
    pix[zm][6][0] := ((o0 and 2)) or ((o1 and 2) shr 1);
    pix[zm][5][0] := ((o0 and 4) shr 1) or ((o1 and 4) shr 2);
    pix[zm][4][0] := ((o0 and 8) shr 2) or ((o1 and 8) shr 3);
    pix[zm][3][0] := ((o0 and 16) shr 3) or ((o1 and 16) shr 4);
    pix[zm][2][0] := ((o0 and 32) shr 4) or ((o1 and 32) shr 5);
    pix[zm][1][0] := ((o0 and 64) shr 5) or ((o1 and 64) shr 6);
    pix[zm][0][0] := ((o0 and 128) shr 6) or ((o1 and 128) shr 7);
    pix[zm][0][1] := ((o0 and 1) shl 1) or (o1 and 1);
    pix[zm][1][1] := ((o0 and 2)) or ((o1 and 2) shr 1);
    pix[zm][2][1] := ((o0 and 4) shr 1) or ((o1 and 4) shr 2);
    pix[zm][3][1] := ((o0 and 8) shr 2) or ((o1 and 8) shr 3);
    pix[zm][4][1] := ((o0 and 16) shr 3) or ((o1 and 16) shr 4);
    pix[zm][5][1] := ((o0 and 32) shr 4) or ((o1 and 32) shr 5);
    pix[zm][6][1] := ((o0 and 64) shr 5) or ((o1 and 64) shr 6);
    pix[zm][7][1] := ((o0 and 128) shr 6) or ((o1 and 128) shr 7);
  end;
end;




end.
