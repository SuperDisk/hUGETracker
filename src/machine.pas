{+-----------------------------------------------------------------------------+
 | Description: Memory functions
 | last changes: 15.12.2000
 |
 | http://www.tu-ilmenau.de/~hackbart
 +----------------------------------------------------------------------------+}

unit machine;

{$MODE Delphi}

interface

const
  MaxCompanies = 74;

  Companies: array[0..maxCompanies] of record
      Code: word;
      Name: string
    end
  =
    (
    (code: $3301; Name: 'Nintendo'), (code: $7901; Name: 'Accolade'), (code: $A400;
    Name: 'Konami'),
    (code: $6701; Name: 'Ocean'), (code: $5601; Name: 'LJN'), (code: $9900;
    Name: 'ARC?'),
    (code: $0101; Name: 'Nintendo'), (code: $0801; Name: 'Capcom'), (code: $0100;
    Name: 'Nintendo'),
    (code: $BB01; Name: 'SunSoft'), (code: $A401; Name: 'Konami'), (code: $AF01;
    Name: 'Namcot?'),
    (code: $4901; Name: 'Irem'), (code: $9C01; Name: 'Imagineer'), (code: $A600;
    Name: 'Kawada?'),
    (code: $B101; Name: 'Nexoft'), (code: $5101; Name: 'Acclaim'), (code: $6001;
    Name: 'Titus'),
    (code: $B601; Name: 'HAL'), (code: $3300; Name: 'Nintendo'), (code: $0B00;
    Name: 'Coconuts?'),
    (code: $5401; Name: 'Gametek'), (code: $7F01; Name: 'Kemco?'), (code: $C001;
    Name: 'Taito'),
    (code: $EB01; Name: 'Atlus'), (code: $E800; Name: 'Asmik?'), (code: $DA00;
    Name: 'Tomy?'),
    (code: $B100; Name: 'ASCII?'), (code: $EB00; Name: 'Atlus'), (code: $C000;
    Name: 'Taito'),
    (code: $9C00; Name: 'Imagineer'), (code: $C201; Name: 'Kemco?'), (code: $D101;
    Name: 'Sofel?'),
    (code: $6101; Name: 'Virgin'), (code: $BB00; Name: 'SunSoft'), (code: $CE01;
    Name: 'FCI?'),
    (code: $B400; Name: 'Enix?'), (code: $BD01; Name: 'Imagesoft'), (code: $0A01;
    Name: 'Jaleco?'),
    (code: $DF00; Name: 'Altron?'), (code: $A700; Name: 'Takara?'), (code: $EE00;
    Name: 'IGS?'),
    (code: $8300; Name: 'Lozc?'), (code: $5001; Name: 'Absolute?'), (code: $DD00;
    Name: 'NCS?'),
    (code: $E500; Name: 'Epoch?'), (code: $CB00; Name: 'VAP?'), (code: $8C00;
    Name: 'Vic Tokai'),
    (code: $C200; Name: 'Kemco?'), (code: $BF00; Name: 'Sammy?'),
    (code: $1800; Name: 'Hudson Soft'), (code: $CA01; Name: 'Palcom/Ultra'),
    (code: $CA00; Name: 'Palcom/Ultra'), (code: $C500; Name: 'Data East?'),
    (code: $A900; Name: 'Technos Japan?'), (code: $D900; Name: 'Banpresto?'),
    (code: $7201; Name: 'Broderbund?'), (code: $7A01; Name: 'Triffix Entertainment?'),
    (code: $E100; Name: 'Towachiki?'), (code: $9300; Name: 'Tsuburava?'),
    (code: $C600; Name: 'Tonkin House?'), (code: $CE00; Name: 'Pony Canyon'),
    (code: $7001; Name: 'Infogrames?'), (code: $8B01; Name: 'Bullet-Proof Software?'),
    (code: $5501; Name: 'Park Place?'), (code: $EA00; Name: 'King Records?'),
    (code: $5D01; Name: 'Tradewest?'), (code: $6F01; Name: 'ElectroBrain?'),
    (code: $AA01; Name: 'Broderbund?'), (code: $C301; Name: 'SquareSoft'),
    (code: $5201; Name: 'Activision?'), (code: $5A01; Name: 'Bitmap Brothers/Mindscape'),
    (code: $5301; Name: 'American Sammy'), (code: $4701; Name: 'Spectrum Holobyte'),
    (code: $1801; Name: 'Hudson Soft'));


function speekb(address: word): byte;
procedure spokeb(address: word; Data: byte);

function wordpeek(Addr: word): word;
procedure wordpoke(addr: word; val: word);
procedure push_pc;

procedure set_bank(number: DWORD);
procedure set_bank_mbc1_hi(Data: DWORD);
procedure set_bank_mbc1_lo(Data: DWORD);
procedure set_bank_mbc5_lo(Data: DWORD);
procedure set_bank_mbc5_hi(Data: DWORD);
procedure set_ff0f(number: byte);
function getcompany: string;

implementation

uses vars, sound, vgm;

function getCompany: string;
var
  j: word;
  i: integer;
begin
  if cart = nil then
    exit;
  J := byte(PChar(cart)[$014B]) shl 8 + byte(PChar(cart)[$014A]);
  for I := low(companies) to high(companies) do
    if Companies[I].Code = J then
    begin
      Result := Companies[I].Name;
      exit;
    end;
  Result := '';
end;

procedure set_bank(number: DWORD);
begin
  if number = 0 then
    number := 1;
  number := number mod srom;
  if number = 0 then
    number := 1;
  addr_bank := number * $4000 - $4000;
end;

procedure set_bank_mbc1_hi(Data: DWord);
begin
  Data := (Data and 3) shl 5;
  rom_priv := (rom_priv and 31) or Data;
  set_bank(rom_priv);
end;

procedure set_bank_mbc1_lo(Data: Dword);
begin
  rom_priv := (rom_priv and 96) or Data;
  set_bank(rom_priv);
end;

procedure set_bank_mbc5_lo(Data: DWORD);
begin
  rom_priv := (rom_priv and 256) or Data;
  set_bank(rom_priv);
end;

procedure set_bank_mbc5_hi(Data: Dword);
begin
  if (Data and 1 > 0) then
    rom_priv := rom_priv or 256
  else
    rom_priv := rom_priv and 255;
  set_bank(rom_priv);
end;

procedure set_ff0f(number: byte);
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
  if m_iram[$ffff] and number > 0 then
    m_iram[$ff0f] := m_iram[$ff0f] or number;
end;



function wordpeek(Addr: word): word;
begin
  Result := ((speekb((addr) + 1) shl 8) or speekb(addr));
end;

procedure wordpoke(addr: word; val: word);
begin
  spokeb(addr, LO(val));
  spokeb(addr + 1, HI(val));
end;

function speekb(address: word): byte;
var
  cr, res: byte;
begin
  if cart = nil then
  begin
    Result := 0;
    exit;
  end;
  if address < $8000 then
  begin
    if (memc = ROM) or (address < $4000) then
      res := byte(PChar(cart)[address])
    else
      res := byte(PChar(cart)[address + addr_bank]);

    Result := res;
    exit;
    // Problem mit cdecl, keine Registerübergabe !!
  end;

  if (address >= $e000) and (address < $fe00) then
    Dec(address, $2000);

  if gb_mode = CGB then
  begin
    if (m_iram[$ff4f] and 1 > 0) and (address >= $8000) and (address < $a000) then
    begin
      res := m_iram2[address];
      Result := res;
      exit;
    end;
    if (address >= $d000) and (address <= $dfff) then
    begin
      cr := (m_iram[$ff70]) and 7;
      if cr = 0 then
        cr := 1;
      res := m_cgbram[4096 * cr + address - $d000];
      Result := res;
      exit;
    end;

    case address of
      $ff26:
      begin // sound
        m_iram[$ff26] := (m_iram[$ff26] and $f0) or (byte(snd[4].Enable) shl 3) or
          (byte(snd[3].Enable) shl 2) or (byte(snd[2].Enable) shl 1) or
          byte(snd[1].Enable);
        res := m_iram[$ff26];
        Result := res;
      end;
      $ff69:
      begin
        res := palramb[(m_iram[$ff68]) and 63];
        Result := res;
        exit;
      end;
      $ff6b:
      begin
        res := palramo[(m_iram[$ff68]) and 63];
        Result := res;
        exit;
      end;
    end; // case
  end; // end CGB


  if (sram > 0) and ((address >= $a000) and (address < $c000)) then
  begin
    res := m_ram[address - $a000 + n_ram * $8912];
    Result := res;
    exit;
  end;

  res := m_iram[address];
  Result := res;
end;

procedure spokeb(address: word; Data: byte); // neu
var
  cr: byte;
  ji, pi: longint;
  sAddr, Offset: word;
begin
  if cart = nil then
    exit;
  if (address < $8000) then
  begin
    case memc of
      MBC3:
      begin
        if ((address >= $2000) and (address <= $3fff)) then
        begin
          if (Data = 0) then
            Data := 1;
          set_bank(Data);
          exit;
        end
        else
        if ((address >= $4000) and (address < $6000)) then
        begin
          n_ram := Data and 3;
          exit;
        end;
      end;
      MBC1:
      begin
        if ((address >= $6000) and (address < $8000)) then
        begin
          mbc1_type := Data and 1;
          exit;
        end
        else
        if ((address >= $2000) and (address < $4000)) then
        begin
          Data := Data and 31;
          set_bank_mbc1_lo(Data);
          exit;
        end
        else
        if ((address >= $4000) and (address < $6000)) then
        begin
          if mbc1_type > 0 then
          begin
            n_ram := Data and 3;
            exit;
          end;
        end
        else
        begin
          set_bank_mbc1_hi(Data);
          exit;
        end;
      end;
      MBC5:
      begin
        if ((address >= $2000) and (address <= $2fff)) then
        begin
          set_bank_mbc5_lo(Data);
          exit;
        end;
        if ((address >= $3000) and (address <= $3fff)) then
        begin
          set_bank_mbc5_hi(Data);
          exit;
        end;
        if ((address >= $4000) and (address <= $5fff)) then
        begin
          n_ram := Data and 15;
          exit;
        end;
      end;
      MBC2:
      begin
        if ((address >= $2000) and (address < $4000) and (address and 256 > 0)) then
        begin
          Data := Data and 15;
          set_bank(Data);
          exit;
        end;
      end;
    end;
  end;


  if gb_mode = CGB then
  begin
    if ((address >= $d000) and (address <= $dfff)) then
    begin
      cr := (m_iram[$ff70]) and 7;
      if (cr = 0) then
        cr := 1;
      m_cgbram[4096 * cr + address - $d000] := Data;
      exit;
    end;

    if (address = $ff55) then
    begin
      if (Data and 128 = 0) then
      begin
        if hdma > 0 then
          hdma := 0
        else
        begin
          adr_s := ((m_iram[$ff51]) shl 8) or (m_iram[$ff52] and 240);
          adr_p := ((((m_iram[$ff53]) and 31) shl 8) or (m_iram[$ff54] and 240)) or $8000;
          for ji := 0 to ((Data and 127) * 16 + 16) - 1 do
          begin
            spokeb(adr_p, speekb(adr_s));
            Inc(adr_p);
            Inc(adr_s);
          end;
          Inc(cnumber, Data * 32 + 928 div gb_speed);
          Data := $ff;
        end;
      end
      else
      begin
        Data := Data and 127;
        if (m_iram[$ff40] and 128 > 0) then
        begin
          hdma := 1;
          hdma_count := Data + 1;
          adr_s := ((m_iram[$ff51]) shl 8) or (m_iram[$ff52] and 240);
          adr_p := ((((m_iram[$ff53]) and 31) shl 8) or (m_iram[$ff54] and 240)) or $8000;
        end;
      end;
    end;


    if ((address >= $8000) and (address < $a000) and ((m_iram[$ff4f] and 1 > 0))) then
    begin
      m_iram2[address] := Data;
      exit;
    end;
  end;

  if ((address >= $e000) and (address < $fe00)) then
    Dec(address, $2000);

  if (address >= $ff00) then
  begin
    case address of
      $ff00:
      begin
        if (Data = 3) then
          Data := $ff
        else
        if ((Data and $30) = $30) then
          Data := $ff
        else
        begin
          if ((Data and $30) = $20) then
            Data := $20 or (k_down shl 3) or (k_up shl 2) or (k_left shl 1) or k_right
          else
          if ((Data and $30) = $10) then
            Data := $10 or (k_start shl 3) or (k_select shl 2) or (k_b shl 1) or k_a;
        end;
        Data := Data or $c0;
      end;
      $ff02:
      begin
        if ((Data and 128 > 0) and (Data and 1 > 0)) then
          sio_count := 8;
        Data := Data or 126;
      end;
      $ff04:
      begin
        Data := 0;
        n_ff04 := 0;
      end;
      $ff07:
      begin
        if (Data and 4) > 0 then
        begin
          c_count := 0;
          t_count := m_iram[$ff06];
        end;
      end;
      $ff40:
      begin
        if (Data and 128 = 0) then
          vbioff_count := 1
        else
        if (m_iram[$ff40] and 128) = 0 then
        begin
          old_mode := 5;
          cycles := 1;
          cur_c := 2;
          nr_t := 0;
        end;
      end;
      $ff0f:
      begin
        if (Data and 16) > 0 then
          set_ff0f(16);
        if (Data and 8) > 0 then
          set_ff0f(8);
        if (Data and 4) > 0 then
          set_ff0f(4);
        if (Data and 2) > 0 then
          set_ff0f(2);
        if (Data and 1) > 0 then
          set_ff0f(1);
      end;
      $ff10..$ff3F:
      begin
        if WritingVGM then
          VGMWriteReg(Address, Data);

        sndRegChange := True;
        if address = $ff26 then
        begin
          m_iram[$ff26] := Data;
          if Data and $80 = 0 then
          begin
            snd[1].Enable := False;
            snd[2].Enable := False;
            snd[3].Enable := False;
            snd[4].Enable := False;
          end;
          exit;
        end;
        m_iram[address] := Data;
        exit;
      end;

      $ff47:
      begin
        if (gb_mode <> CGB) then
        begin
          gb_mode := CGB;
          spokeb($ff68, 128);
          spokeb($ff69, (colors[Data and 3]) and $ff);
          spokeb($ff69, (colors[Data and 3]) shr 8);
          spokeb($ff69, (colors[(Data and $c) shr 2]) and $ff);
          spokeb($ff69, (colors[(Data and $c) shr 2]) shr 8);
          spokeb($ff69, (colors[(Data and $30) shr 4]) and $ff);
          spokeb($ff69, (colors[(Data and $30) shr 4]) shr 8);
          spokeb($ff69, (colors[(Data and $c0) shr 6]) and $ff);
          spokeb($ff69, (colors[(Data and $c0) shr 6]) shr 8);
          gb_mode := DMG;
        end;
      end;
      $ff48:
      begin
        if (gb_mode <> CGB) then
        begin
          gb_mode := CGB;
          spokeb($ff6a, 128);
          spokeb($ff6b, (colors[Data and 3]) and $ff);
          spokeb($ff6b, (colors[Data and 3]) shr 8);
          spokeb($ff6b, (colors[(Data and $c) shr 2]) and $ff);
          spokeb($ff6b, (colors[(Data and $c) shr 2]) shr 8);
          spokeb($ff6b, (colors[(Data and $30) shr 4]) and $ff);
          spokeb($ff6b, (colors[(Data and $30) shr 4]) shr 8);
          spokeb($ff6b, (colors[(Data and $c0) shr 6]) and $ff);
          spokeb($ff6b, (colors[(Data and $c0) shr 6]) shr 8);
          gb_mode := DMG;
        end;
      end;
      $ff49:
      begin
        if gb_mode <> CGB then
        begin
          gb_mode := CGB;
          spokeb($ff6a, 128 + 8);
          spokeb($ff6b, (colors[Data and 3]) and $ff);
          spokeb($ff6b, (colors[Data and 3]) shr 8);
          spokeb($ff6b, (colors[(Data and $c) shr 2]) and $ff);
          spokeb($ff6b, (colors[(Data and $c) shr 2]) shr 8);
          spokeb($ff6b, (colors[(Data and $30) shr 4]) and $ff);
          spokeb($ff6b, (colors[(Data and $30) shr 4]) shr 8);
          spokeb($ff6b, (colors[(Data and $c0) shr 6]) and $ff);
          spokeb($ff6b, (colors[(Data and $c0) shr 6]) shr 8);
          gb_mode := DMG;
        end;
      end;
      $ff41:
      begin
        if ((mode = 0) or (mode = 1)) then
        begin
          m_iram[$ff0f] := m_iram[$ff0f] or 2;
          set_ff0f(2);
        end;
      end;
      $ff46:
      begin
        sAddr := Data shl 8;
        Offset := $fe00;
        for pi := 0 to $9f do
        begin
          m_iram[Offset] := speekb(sAddr);
          Inc(Offset);
          Inc(sAddr);
        end;
      end;

    end;
  end;

  if (sram > 0) and (address >= $a000) and (address < $c000) then
    m_ram[address - $a000 + n_ram * $8912] := Data
  else
    m_iram[address] := Data;
end;


procedure push_pc;
begin
  dec(sp_.W,2);
  wordpoke(sp_.W,pc.w);
end;

end.
