{+-----------------------------------------------------------------------------+
 | Author:      Christian Hackbart
 | Description: Sound
 | Copyright (c) 2000 Christian Hackbart
 | Stand: 15.12.2000
 |
 | http://www.tu-ilmenau.de/~hackbart
 +----------------------------------------------------------------------------+}

unit sound;

{$mode objfpc} //For binary literals

interface

(*

FF10 -- SNDREG10 [RW] Sweep [Sound Mode #1]
Bit6-4  Sweep time:
  000: SWEEP OFF    010: 15.6ms    100: 31.3ms    110: 46.9ms
  001: 7.8ms        011: 23.4ms    101: 39.1ms    111: 54.7ms
Bit3    Frequency increase[0]/decrease[1]
Bit2-0  Number of shifts
------------------------------------------------------------------------------
FF11 -- SNDREG11 [RW] Sound Length/Pattern Duty [Sound Mode #1]
Bit7-6  Wave Pattern Duty [only these bits can be read]:
  00: 12.5%    01: 25%    10: 50%    11: 75%
Bit5-0  Length of sound data
------------------------------------------------------------------------------
FF12 -- SNDREG12 [RW] Control [Sound Mode #1]
Bit7-4  Initial value of envelope
Bit3    Envelope up[1]/down[0]
Bit2-0  Number of envelope sweep
------------------------------------------------------------------------------
FF13 -- SNDREG13 [W] Frequency Low [Sound Mode #1]
        Lower 8 bits of the 11bit frequency. Higher 3 bits are in SNDREG14.
------------------------------------------------------------------------------
FF14 -- SNDREG14 [RW] Frequency High [Sound Mode #1]
Bit7    When 1 is written into this bit, sound restarts
Bit6    Counter/Consecutive selection [only this bit can be read]
Bit2-0  Higher 3 bits of the 11bit frequency
------------------------------------------------------------------------------
FF16 -- SNDREG21 [RW] Sound Length/Pattern Duty [Sound Mode #2]
Bit7-6  Wave Pattern Duty [only these bits can be read]:
  00: 12.5%    01: 25%    10: 50%    11: 75%
Bit5-0  Length of sound data
------------------------------------------------------------------------------
FF17 -- SNDREG22 [RW] Control [Sound Mode #2]
Bit7-4  Initial value of envelope
Bit3    Envelope up[1]/down[0]
Bit2-0  Number of envelope step
------------------------------------------------------------------------------
FF18 -- SNDREG23 [W] Frequency Low [Sound Mode #2]
        Lower 8 bits of the 11bit frequency. Higher 3 bits are in SNDREG24.
------------------------------------------------------------------------------
FF19 -- SNDREG24 [RW] Frequency High [Sound Mode #2]
Bit7    When 1 is written into this bit, sound restarts
Bit6    Counter/Consecutive selection [only this bit can be read]
Bit2-0  Higher 3 bits of the 11bit frequency
------------------------------------------------------------------------------
FF1A -- SNDREG30 [RW] Control [Sound Mode #3]
Bit7    Sound on[1]/off[0]
------------------------------------------------------------------------------
FF1B -- SNDREG31 [RW] Sound Length [Sound Mode #3]
------------------------------------------------------------------------------
FF1C -- SNDREG32 [RW] Output Level [Sound Mode #3]
Bit6-5  Output Level:
  00: MUTE    01: 100%    10: 50%    11: 25%
------------------------------------------------------------------------------
FF1D -- SNDREG33 [W] Frequency Low [Sound Mode #3]
        Lower 8 bits of the 11bit frequency. Higher 3 bits are in SNDREG34.
------------------------------------------------------------------------------
FF1E -- SNDREG34 [RW] Frequency High [Sound Mode #3]
Bit7    When 1 is written into this bit, sound restarts
Bit6    Counter/Consecutive selection [only this bit can be read]
Bit2-0  Higher 3 bits of the 11bit frequency
------------------------------------------------------------------------------
FF20 -- SNDREG41 [RW] Sound Length/Pattern Duty [Sound Mode #4]
Bit5-0  Length of sound data
------------------------------------------------------------------------------
FF21 -- SNDREG42 [RW] Control [Sound Mode #4]
Bit7-4  Initial value of envelope
Bit3    Envelope up[1]/down[0]
Bit2-0  Number of envelope step
------------------------------------------------------------------------------
FF22 -- SNDREG43 [RW] Polynomial Counter [Sound Mode #4]
Bit7-4  Shift clock frequency for the counter
  0000: Dividing ratio of frequencies / 2
  0001: Dividing ratio of frequencies / 2^2
  0010: Dividing ratio of frequencies / 2^3
  ....  ....
  1101: Dividing ratio of frequencies / 2^14
  1100: Prohibited
  1111: Prohibited
Bit3    Number of steps: 7 [1]/15 [0]
Bit2-0  Dividing ratio of frequences
  000: f*2    010: f/2    100: f/4    110: f/6    where f = 4.194304Mhz/8
  001: f*1    011: f/3    101: f/5    111: f/7
------------------------------------------------------------------------------
FF23 -- SNDREG44 [RW] Frequency High [Sound Mode #4]
Bit7    When 1 is written into this bit, sound restarts
Bit6    Counter/Consecutive selection [only this bit can be read]
------------------------------------------------------------------------------
FF24 -- SNDREG50 [RW] Channel and Volume Control
Bit7    Vin -> SO2 on[1]/off[0]
Bit6-4  Volume on SO2
Bit3    Vin -> SO1 on[1]/off[0]
Bit2-0  Volume on SO1
------------------------------------------------------------------------------
FF25 -- SNDREG51 [RW] Sound Output Terminal Selector
Bit7  Sound 4 -> SO2  |
Bit6  Sound 3 -> SO2  |
Bit5  Sound 2 -> SO2  | SO1 and SO2 are two sound outputs connected to the
Bit4  Sound 1 -> SO2  | headphones. Vin is an input terminal in the cartridge
Bit3  Sound 4 -> SO1  | slot.
Bit2  Sound 3 -> SO1  |
Bit1  Sound 2 -> SO1  |
Bit0  Sound 1 -> SO1  |
----------------------+-------------------------------------------------------
FF26 -- SNDREG52 [RW] Sound ON/OFF
Bit7  All sound on[1]/off[0]
Bit3  Sound 4 on[1]/off[0]
Bit2  Sound 3 on[1]/off[0]
Bit1  Sound 2 on[1]/off[0]
Bit0  Sound 1 on[1]/off[0]

*)

procedure EnableSound;
procedure DisableSound;
procedure SoundUpdate(cycles: integer);
procedure SoundSetCycles(n: integer);

var
  soundEnable: boolean;
  sndRegChange: boolean;
  snd: array[1..4] of record
    // public:
    ChannelOFF: boolean; // (un)mute Channel
    // private:
    enable: boolean;
    Freq: integer;
    Vol: shortint;
    Len: integer;
    swpCnt: byte;
    EnvCnt: byte;
    bit: byte;
    cnt: integer;
  end;

implementation

uses vars, bass;

const
  sampleCycles: longint = (8192 * 1024) div 22050;

var
  playStream: HStream;
  ready: integer;
  bufPos, bufCycles, bufLVal, bufRVal: integer;
  buf: array[0..2047] of byte;

  myFile: file of byte;

procedure EnableSound;
begin
  if soundEnable then
    exit;

  BASS_Init(-1, 22050, 0, 0, nil);
  PlayStream := BASS_StreamCreate(
    22050,
    2,
    BASS_SAMPLE_8BITS,
    StreamProc(STREAMPROC_PUSH),
    nil);
  BASS_ChannelPlay(PlayStream, True);

  soundEnable := True;
  ready := 3;
  bufPos := 0;
  bufCycles := 0;
  bufLVal := 0;
  bufRVal := 0;
end;

procedure DisableSound;
begin
  if not soundEnable then
    exit;

  BASS_Free;

  soundEnable := False;
end;

procedure SoundDoOut(l, r: byte; cycles: integer);
begin
  Inc(bufLVal, l * cycles);
  Inc(bufRVal, r * cycles);
  Inc(bufCycles, cycles);
  if bufCycles >= sampleCycles then
  begin
    buf[bufPos] := bufRVal div sampleCycles;
    buf[bufPos + 1] := bufLVal div sampleCycles;
    bufCycles := 0;
    Inc(bufPos, 2);
    bufLVal := 0;
    bufRVal := 0;
    if bufPos >= 2048 then
    begin
      BlockWrite(myFile, buf, 2048);
      BASS_StreamPutData(PlayStream, @buf, 2048);
      Dec(ready);
      bufPos := 0;
    end;
  end;
end;

procedure SoundOutBits(l, r: byte; cycles: integer);
var
  left: integer;
begin
  if not soundEnable then
    exit;
  while bufCycles + cycles > sampleCycles do
  begin
    left := sampleCycles - bufCycles;
    SoundDoOut(l, r, left);
    Dec(cycles, left);
  end;
  SoundDoOut(l, r, cycles);
end;

const
  bit: array[0..3, 0..7] of integer =
    ((1, 0, 0, 0, 0, 0, 0, 0),
    (1, 1, 0, 0, 0, 0, 0, 0),
    (1, 1, 1, 1, 0, 0, 0, 0),
    (1, 1, 1, 1, 1, 1, 0, 0));
  vol: array[0..15] of integer = (0, 8, 17, 25, 34, 42, 51, 59, 68, 76, 85, 93, 102, 110, 119, 127);

var
  swpClk, envClk, lenClk, freqClk, freq4Clk: longint;
  l, r: integer;

procedure SoundUpdate(cycles: integer);
var
  n, stage: integer;
begin
  if (not soundEnable) then
    exit;
  if sndRegChange then
  begin
    snd[1].Freq := m_iram[$FF13] or ((m_iram[$FF14] and %111) shl 8);
    snd[2].Freq := m_iram[$FF18] or ((m_iram[$FF19] and %111) shl 8);
    snd[3].Freq := m_iram[$FF1d] or ((m_iram[$FF1e] and %111) shl 8);
    case m_iram[$FF22] and %111 of
      0: snd[4].Freq := (512 * 1024 * 2) shr ((m_iram[$FF22] shr 4) + 1);
      1: snd[4].Freq := (512 * 1024) shr ((m_iram[$FF22] shr 4) + 1);
      2: snd[4].Freq := ((512 * 1024) div 2) shr ((m_iram[$FF22] shr 4) + 1);
      3: snd[4].Freq := ((512 * 1024) div 3) shr ((m_iram[$FF22] shr 4) + 1);
      4: snd[4].Freq := ((512 * 1024) div 4) shr ((m_iram[$FF22] shr 4) + 1);
      5: snd[4].Freq := ((512 * 1024) div 5) shr ((m_iram[$FF22] shr 4) + 1);
      6: snd[4].Freq := ((512 * 1024) div 6) shr ((m_iram[$FF22] shr 4) + 1);
      7: snd[4].Freq := ((512 * 1024) div 7) shr ((m_iram[$FF22] shr 4) + 1);
    end;
    snd[4].Freq := (8192 * 1024) div snd[4].Freq;
    snd[3].Enable := m_iram[$FF1a] and %10000000 > 0;
    snd[1].Vol := m_iram[$FF12] shr 4;
    if m_iram[$FF14] and %10000000 > 0 then
    begin
      snd[1].Len := 64 - (m_iram[$FF11] and %111111);
      snd[1].Cnt := 0;
      m_iram[$FF14] := m_iram[$FF14] and %1111111;
      snd[1].Enable := True;
    end;
    snd[2].Vol := m_iram[$FF17] shr 4;
    if m_iram[$FF19] and %10000000 > 0 then
    begin
      snd[2].Len := 64 - (m_iram[$FF16] and %111111);
      snd[2].Cnt := 0;
      m_iram[$FF19] := m_iram[$FF19] and %1111111;
      snd[2].Enable := True;
    end;
    if m_iram[$FF1e] and %10000000 > 0 then
    begin
      snd[3].Len := (256 - byte(m_iram[$FF1b])) shl 7;
      snd[3].Cnt := 0;
      m_iram[$FF1e] := m_iram[$FF1e] and %1111111;
    end;
    snd[4].Vol := m_iram[$FF21] shr 4;
    if m_iram[$FF23] and %10000000 > 0 then
    begin
      snd[4].Len := 64 - (m_iram[$FF20] and %111111);
      m_iram[$FF23] := m_iram[$FF23] and %1111111;
      snd[4].Enable := True;
    end;
    sndRegChange := False;
  end;
  if (snd[1].Enable) and (m_iram[$FF10] and %1110000 > 0) then
  begin
    Inc(swpClk, cycles);
    if swpClk >= (8192 * 1024 div 128) then
    begin
      Dec(swpClk, 8192 * 1024 div 128);
      Inc(snd[1].SwpCnt);
      if snd[1].SwpCnt >= ((m_iram[$FF10] shr 4) and %111) then
      begin
        snd[1].SwpCnt := 0;
        if m_iram[$FF10] and %1000 > 0 then
        begin
          Dec(snd[1].Freq, snd[1].Freq shr (m_iram[$FF10] and %111));
          if snd[1].Freq < 0 then
            snd[1].Freq := 0;
        end
        else
        begin
          Inc(snd[1].Freq, snd[1].Freq shr (m_iram[$FF10] and %111));
          if snd[1].Freq > 2047 then
          begin
            snd[1].Freq := 2047;
            snd[1].Enable := False;
          end;
        end;
      end;
    end;
  end;
  Inc(envClk, cycles);
  if envClk >= 8192 * 1024 div 64 then
  begin
    Dec(envClk, 8192 * 1024 div 64);
    if (snd[1].Enable) and (m_iram[$FF12] and %111 > 0) then
    begin
      Inc(snd[1].EnvCnt);
      if snd[1].EnvCnt >= (m_iram[$FF12] and %111) then
      begin
        snd[1].EnvCnt := 0;
        if m_iram[$FF12] and %1000 > 0 then
        begin
          Inc(snd[1].Vol);
          if (snd[1].Vol > $f) then
            snd[1].Vol := $f;
          m_iram[$FF12] := (m_iram[$FF12] and %1111) or (snd[1].Vol shl 4);
        end
        else
        begin
          Dec(snd[1].Vol);
          if (snd[1].Vol < 0) then
            snd[1].Vol := 0;
          m_iram[$FF12] := (m_iram[$FF12] and %1111) or (snd[1].Vol shl 4);
        end;
      end;
    end;
    if (snd[2].Enable) and (m_iram[$FF17] and %111 > 0) then
    begin
      Inc(snd[2].EnvCnt);
      if snd[2].EnvCnt >= (m_iram[$FF17] and %111) then
      begin
        snd[2].EnvCnt := 0;
        if m_iram[$FF17] and %1000 > 0 then
        begin
          Inc(snd[2].Vol);
          if (snd[2].Vol > $f) then
            snd[2].Vol := $f;
          m_iram[$FF17] := (m_iram[$FF17] and %1111) or (snd[2].Vol shl 4);
        end
        else
        begin
          Dec(snd[2].Vol);
          if (snd[2].Vol < 0) then
            snd[2].Vol := 0;
          m_iram[$FF17] := (m_iram[$FF17] and %1111) or (snd[2].Vol shl 4);
        end;
      end;
    end;
    if (snd[4].Enable) and (m_iram[$FF21] and %111 > 0) then
    begin
      Inc(snd[4].EnvCnt);
      if snd[4].EnvCnt >= m_iram[$FF21] and %111 then
      begin
        snd[4].EnvCnt := 0;
        if m_iram[$FF21] and %1000 > 0 then
        begin
          Inc(snd[4].Vol);
          if snd[4].Vol > $f then
            snd[4].Vol := $f;
          m_iram[$FF21] := (m_iram[$FF21] and %1111) or (snd[4].Vol shl 4);
        end
        else
        begin
          Dec(snd[4].Vol);
          if (snd[4].Vol < 0) then
            snd[4].Vol := 0;
          m_iram[$FF21] := (m_iram[$FF21] and %1111) or (snd[4].Vol shl 4);
        end;
      end;
    end;
  end;
  Inc(lenClk, cycles);
  if lenClk >= 8192 * 1024 div 256 then
  begin
    Dec(lenClk, 8192 * 1024 div 256);
    if snd[1].Enable then
    begin
      Dec(snd[1].Len);
      if (snd[1].Len <= 0) and (m_iram[$FF14] and %1000000 > 0) then
        snd[1].Enable := False;
    end;
    if snd[2].Enable then
    begin
      Dec(snd[2].Len);
      if (snd[2].Len <= 0) and (m_iram[$FF19] and %1000000 > 0) then
        snd[2].Enable := False;
    end;
    if snd[3].Enable then
    begin
      Dec(snd[3].Len);
      if (snd[3].Len <= 0) and (m_iram[$FF1e] and %1000000 > 0) then
      begin
        snd[3].Enable := False;
        m_iram[$ff1a] := m_iram[$ff1a] and $7f;
      end;
    end;
    if snd[4].Enable then
    begin
      Dec(snd[4].Len);
      if (snd[4].Len <= 0) and (m_iram[$FF23] and %1000000 > 0) then
        snd[4].Enable := False;
    end;
  end;
  m_iram[$FF13] := snd[1].Freq and $ff;
  m_iram[$FF14] := (m_iram[$FF14] and $f8) or ((snd[1].Freq shr 8) and %111);
  Inc(freqClk, cycles);
  if freqClk >= 4 then
  begin
    n := freqClk shr 2;
    Dec(freqClk, n shl 2);
    if snd[1].Enable then
    begin
      Inc(snd[1].Cnt, n);
      while snd[1].Cnt >= ((2048 - snd[1].Freq) shl 4) do
        Dec(snd[1].Cnt, ((2048 - snd[1].Freq) shl 4));
    end;
    if snd[2].Enable then
    begin
      Inc(snd[2].Cnt, n);
      while snd[2].Cnt >= ((2048 - snd[2].Freq) shl 4) do
        Dec(snd[2].Cnt, ((2048 - snd[2].Freq) shl 4));
    end;
    if snd[3].Enable then
    begin
      Inc(snd[3].Cnt, n);
      while snd[3].Cnt >= ((2048 - snd[3].Freq) shl 5) do
        Dec(snd[3].Cnt, ((2048 - snd[3].Freq) shl 5));
    end;
  end;
  if not snd[1].channelOFF then
  begin
    if snd[1].Enable then
    begin
      stage := (snd[1].Cnt div (2048 - snd[1].Freq)) shr 1;
      if stage > 7 then
        stage := 7;
      snd[1].Bit := bit[m_iram[$FF11] shr 6][stage];
    end;
    if m_iram[$FF25] and 1 > 0 then
      if snd[1].Bit > 0 then
        Inc(l, vol[snd[1].Vol])
      else
        Dec(l, vol[snd[1].Vol]);

    if m_iram[$FF25] and $10 > 0 then
      if snd[1].bit > 0 then
        Inc(r, vol[snd[1].Vol])
      else
        Dec(r, vol[snd[1].Vol]);
  end;

  if not snd[2].channelOFF then
  begin

    if snd[2].Enable then
    begin
      stage := (snd[2].Cnt div (2048 - snd[2].Freq)) shr 1;
      if stage > 7 then
        stage := 7;
      snd[2].Bit := bit[m_iram[$FF16] shr 6][stage];
    end;
    if m_iram[$FF25] and 2 > 0 then
      if snd[2].bit > 0 then
        Inc(l, vol[snd[2].Vol])
      else
        Dec(l, vol[snd[2].Vol]);

    if m_iram[$FF25] and $20 > 0 then
      if snd[2].Bit > 0 then
        Inc(r, vol[snd[2].Vol])
      else
        Dec(r, vol[snd[2].Vol]);
  end;

  if not snd[3].channelOFF then
  begin

    if snd[3].Enable then
    begin
      stage := snd[3].Cnt div (2048 - snd[3].Freq);
      if stage > 31 then
        stage := 31;
      snd[3].Bit := m_iram[$FF30 + (stage shr 1)];
      if stage and 1 > 0 then
        snd[3].Bit := snd[3].Bit and %1111
      else
        snd[3].Bit := snd[2].Bit shr 4;

      case (m_iram[$FF1c] shr 5) and 3 of
        0: snd[3].Bit := 8;
        1: ;
        2: snd[3].Bit := 8 or (snd[3].Bit shr 1);
        3: snd[3].Bit := $c or (snd[3].Bit shr 2);
      end;
    end;
    if m_iram[$FF25] and 4 > 0 then
      Inc(l, (snd[3].Bit shl 4) - $80);
    if m_iram[$FF25] and %1000000 > 0 then
      Inc(r, (snd[3].Bit shl 4) - $80);
  end;
  if not snd[4].channelOFF then
  begin

    if snd[4].Enable then
    begin

      Inc(freq4Clk, cycles);
      if (freq4Clk >= snd[4].Freq) then
      begin
        freq4Clk := freq4Clk mod snd[4].Freq;
        snd[4].Bit := random(255) and 1; // white noise
      end;
    end;
    if (m_iram[$FF25] and 8) > 0 then
      if snd[4].Bit > 0 then
        Inc(l, vol[snd[4].Vol])
      else
        Dec(l, vol[snd[4].Vol]);
    if (m_iram[$FF25] and $80) > 0 then
      if snd[4].Bit > 0 then
        Inc(r, vol[snd[4].Vol])
      else
        Dec(r, vol[snd[4].Vol]);
  end;

    {l:=l shr 2;
    if (shortint(l)<-128) then l:=-128;
    if (shortint(l)>127) then l:=127;
    inc(l,$80);

    r:=r shr 2;
    if (shortint(r)<-128) then r:=-128;
    if (shortint(r)>127) then r:=127;
    inc(r,$80);}

  l := shortint(l shr 2) + 128;
  r := shortint(r shr 2) + 128;
  if (l <> 170) and (r <> 170) then
    SoundOutBits(l, r, cycles);
end;

procedure SoundSetCycles(n: integer);
begin
  sampleCycles := n;
end;

begin
  assign(myFile, 'snd_debug.pcm');
  rewrite(myFile);
end.
