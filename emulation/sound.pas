{+-----------------------------------------------------------------------------+
 | Author:      Christian Hackbart
 | Description: Sound
 | Copyright (c) 2000 Christian Hackbart
 | Stand: 15.12.2000
 |
 | http://www.tu-ilmenau.de/~hackbart
 |
 | Translated from VGBC's Sound.cpp, written by Rusty Wagner
 +----------------------------------------------------------------------------+}

unit sound;

{$MODE objfpc}{$H+}

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

uses Classes, sysutils, sdl2;

const
  SAMPLE_BUFFER_SIZE = 1024;

type
  PSingle = ^Single;
  TSampleBuffer = record
    BufferL, BufferR: array[0..SAMPLE_BUFFER_SIZE] of Integer;
    Cursor: Integer;
  end;

procedure StartPlayback;
procedure StopPlayback;
procedure LockPlayback;
procedure UnlockPlayback;

procedure AudioCallback(userdata: Pointer; stream: PUInt8; len: Integer) cdecl;

procedure EnableSound;
procedure DisableSound;
procedure ResetSound;
procedure SoundUpdate(cycles: integer);

function SoundBufferTooFull: Boolean;
function SoundBufferSize: Integer;

procedure BeginWritingSoundToStream(Stream: TStream);
procedure EndWritingSoundToStream;

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
  SampleBuffers: array[0..4] of TSampleBuffer;

implementation

uses mainloop, vars, fpWavWriter;

const
  SampleSize = SizeOf(Single)*2;
  playbackFrequency = 44100;
  sampleCycles: longint = (8192 * 1024) div playbackFrequency;
  TooFullThreshold: Integer = (playbackFrequency div 10)*sizeof(single); //0.1s

var
  PlayStream: TSDL_AudioDeviceID;
  bufCycles, bufLVal, bufRVal: integer;

  sndBuffer: ^Single;
  sndBytesWritten: Integer;

  lfsr: Cardinal = 0;

  WritingSoundToStream: Boolean;
  WaveWriter: TWavWriter;

procedure ResetSound;
var
  i: Integer;
begin
  for i := Low(Snd) to High(snd) do begin
    with snd[i] do begin
      ChannelOFF := False;
      enable := False;
      Freq := 0;
      Vol := 0;
      Len := 0;
      swpCnt := 0;
      EnvCnt := 0;
      bit := 0;
      cnt := 0;
    end;
  end;
end;

function SoundBufferTooFull: Boolean;
begin
  Result := SoundBufferSize > TooFullThreshold
end;

function SoundBufferSize: Integer;
begin
  Result := SDL_GetQueuedAudioSize(PlayStream)
end;

procedure BeginWritingSoundToStream(Stream: TStream);
begin
  WritingSoundToStream := True;
  WaveWriter := TWavWriter.Create;
  {$push}{$R-}WaveWriter.StoreToStream(Stream);{$pop}
  with WaveWriter.fmt do begin
    SampleRate := playbackFrequency;
    BitsPerSample := 16;
    Channels := 2;
    ByteRate := SampleRate * Channels * (BitsPerSample div 8);
  end;
end;

procedure EndWritingSoundToStream;
begin
  WritingSoundToStream := False;
  WaveWriter.Free;
end;

procedure StartPlayback;
begin
  SDL_PauseAudioDevice(PlayStream, 0);
end;

procedure StopPlayback;
begin
  SDL_PauseAudioDevice(PlayStream, 1);
end;

procedure LockPlayback;
begin
  SDL_LockAudioDevice(PlayStream);
end;

procedure UnlockPlayback;
begin
  SDL_UnlockAudioDevice(PlayStream);
end;

procedure AudioCallback(userdata: Pointer; stream: PUInt8; len: Integer) cdecl;
begin
  sndBuffer := PSingle(Stream);
  sndBytesWritten := 0;
  while sndBytesWritten < len do
    z80_decode;

  if sndBytesWritten > len then
    Writeln(StdErr,
      Format('[WARNING] Audio callback wrote into uninitialized ram! (%d written, %d requested)', [sndBytesWritten, len]));
end;

procedure EnableSound;
var
  Want, Have: TSDL_AudioSpec;
begin
  if soundEnable then
    exit;

  Want := Default(TSDL_AudioSpec);
  Have := Default(TSDL_AudioSpec);

  SDL_Init(SDL_INIT_AUDIO);

  Want.freq := playbackFrequency;
  Want.format := AUDIO_F32;
  Want.channels := 2;
  Want.samples := 512;
  Want.callback := @AudioCallback;

  PlayStream := SDL_OpenAudioDevice(nil, 0, @Want, @Have, 0);

  soundEnable := True;
  bufCycles := 0;
  bufLVal := 0;
  bufRVal := 0;
end;

procedure DisableSound;
begin
  if not soundEnable then
    exit;

  SDL_CloseAudioDevice(PlayStream);
  SDL_Quit;

  soundEnable := False;
end;

procedure SoundDoOut(l, r: Integer; cycles: integer);
var
  buf: array[0..1] of Single;
  buf2: array[0..1] of Smallint;
begin
  Inc(bufLVal, l * cycles);
  Inc(bufRVal, r * cycles);
  Inc(bufCycles, cycles);
  if bufCycles >= sampleCycles then
  begin
    buf[0] := ((bufRVal div sampleCycles) / 512.0);
    buf[1] := ((bufLVal div sampleCycles) / 512.0);
    bufCycles := 0;
    bufLVal := 0;
    bufRVal := 0;

    if WritingSoundToStream then begin
      buf2[0] := Trunc(buf[0]*High(Smallint));
      buf2[1] := Trunc(buf[1]*High(Smallint));
      WaveWriter.WriteBuf(buf2, SizeOf(Smallint)*2);
    end
    else begin
      sndBuffer^ := buf[0];
      Inc(sndBuffer);
      sndBuffer^ := buf[1];
      Inc(sndBuffer);
      Inc(sndBytesWritten, SampleSize);
    end;
  end;
end;

procedure SoundOutBits(l, r: Integer; cycles: integer);
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


// Yanked from SameBoy: https://github.com/LIJI32/SameBoy/blob/master/Core/apu.c#L489
function NextLFSRBit(Narrow: Boolean): Byte;
var
  HighBitMask: Cardinal;
  NewHighBit: Boolean;
begin
  if Narrow then
    HighBitMask := $4040
  else
    HighBitMask := $4000;

  NewHighBit := (((lfsr xor (lfsr shr 1)) xor 1) and 1) <> 0;
  lfsr := lfsr shr 1;

  if NewHighBit then
    lfsr := lfsr or HighBitMask
  else
    lfsr := lfsr and not HighBitMask;

  Result := (lfsr and 1);
end;

procedure SoundUpdate(cycles: integer);
var
  n, stage: integer;
  ls: array[1..4] of Integer = (0, 0, 0, 0);
  rs: array[1..4] of Integer = (0, 0, 0, 0);
  l, r: Integer;
  I: Integer;
begin
  if (not soundEnable) then
    exit;

  l := 0;
  r := 0;
  if sndRegChange then
  begin
    snd[1].Freq := m_iram[$FF13] or ((m_iram[$FF14] and 7) shl 8);
    snd[2].Freq := m_iram[$FF18] or ((m_iram[$FF19] and 7) shl 8);
    snd[3].Freq := m_iram[$FF1d] or ((m_iram[$FF1e] and 7) shl 8);
    case m_iram[$FF22] and 7 of
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
    snd[3].Enable := m_iram[$FF1a] and $80 > 0;

    if m_iram[$FF14] and $80 > 0 then
    begin
      snd[1].Vol := m_iram[$FF12] shr 4;
      snd[1].Len := 64 - (m_iram[$FF11] and 63);
      snd[1].Cnt := 0;
      m_iram[$FF14] := m_iram[$FF14] and $7f;
      snd[1].Enable := True;
    end;

    if m_iram[$FF19] and $80 > 0 then
    begin
      snd[2].Vol := m_iram[$FF17] shr 4;
      snd[2].Len := 64 - (m_iram[$FF16] and 63);
      snd[2].Cnt := 0;
      m_iram[$FF19] := m_iram[$FF19] and $7f;
      snd[2].Enable := True;
    end;

    if m_iram[$FF1e] and $80 > 0 then
    begin
      snd[3].Len := (256 - byte(m_iram[$FF1b]));// shl 7;
      snd[3].Cnt := 0;
      m_iram[$FF1e] := m_iram[$FF1e] and $7f;
      snd[3].Enable := True;
      m_iram[$ff1a] := m_iram[$ff1a] or %10000000;
    end;

    if m_iram[$FF23] and $80 > 0 then
    begin
      snd[4].Vol := m_iram[$FF21] shr 4;
      snd[4].Len := 64 - (m_iram[$FF20] and 63);
      m_iram[$FF23] := m_iram[$FF23] and $7f;
      snd[4].Enable := True;
      lfsr := 0;
    end;

    sndRegChange := False;
  end;
  if (snd[1].Enable) and (m_iram[$FF10] and $70 > 0) then
  begin
    Inc(swpClk, cycles);
    if swpClk >= (8192 * 1024 div 128) then
    begin
      Dec(swpClk, 8192 * 1024 div 128);
      Inc(snd[1].SwpCnt);
      if snd[1].SwpCnt >= ((m_iram[$FF10] shr 4) and 7) then
      begin
        snd[1].SwpCnt := 0;
        if m_iram[$FF10] and 8 > 0 then
        begin
          Dec(snd[1].Freq, snd[1].Freq shr (m_iram[$FF10] and 7));
          if snd[1].Freq < 0 then
            snd[1].Freq := 0;
        end
        else
        begin
          Inc(snd[1].Freq, snd[1].Freq shr (m_iram[$FF10] and 7));
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
    if (snd[1].Enable) and (m_iram[$FF12] and 7 > 0) then
    begin
      Inc(snd[1].EnvCnt);
      if snd[1].EnvCnt >= (m_iram[$FF12] and 7) then
      begin
        snd[1].EnvCnt := 0;
        if m_iram[$FF12] and 8 > 0 then
        begin
          Inc(snd[1].Vol);
          if (snd[1].Vol > $f) then
            snd[1].Vol := $f;
          m_iram[$FF12] := (m_iram[$FF12] and $f) or (snd[1].Vol shl 4);
        end
        else
        begin
          Dec(snd[1].Vol);
          if (snd[1].Vol < 0) then
            snd[1].Vol := 0;
          m_iram[$FF12] := (m_iram[$FF12] and $f) or (snd[1].Vol shl 4);
        end;
      end;
    end;
    if (snd[2].Enable) and (m_iram[$FF17] and 7 > 0) then
    begin
      Inc(snd[2].EnvCnt);
      if snd[2].EnvCnt >= (m_iram[$FF17] and 7) then
      begin
        snd[2].EnvCnt := 0;
        if m_iram[$FF17] and 8 > 0 then
        begin
          Inc(snd[2].Vol);
          if (snd[2].Vol > $f) then
            snd[2].Vol := $f;
          m_iram[$FF17] := (m_iram[$FF17] and $f) or (snd[2].Vol shl 4);
        end
        else
        begin
          Dec(snd[2].Vol);
          if (snd[2].Vol < 0) then
            snd[2].Vol := 0;
          m_iram[$FF17] := (m_iram[$FF17] and $f) or (snd[2].Vol shl 4);
        end;
      end;
    end;
    if (snd[4].Enable) and (m_iram[$FF21] and 7 > 0) then
    begin
      Inc(snd[4].EnvCnt);
      if snd[4].EnvCnt >= m_iram[$FF21] and 7 then
      begin
        snd[4].EnvCnt := 0;
        if m_iram[$FF21] and 8 > 0 then
        begin
          Inc(snd[4].Vol);
          if snd[4].Vol > $f then
            snd[4].Vol := $f;
          m_iram[$FF21] := (m_iram[$FF21] and $f) or (snd[4].Vol shl 4);
        end
        else
        begin
          Dec(snd[4].Vol);
          if (snd[4].Vol < 0) then
            snd[4].Vol := 0;
          m_iram[$FF21] := (m_iram[$FF21] and $f) or (snd[4].Vol shl 4);
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
      if (snd[1].Len <= 0) and (m_iram[$FF14] and $40 > 0) then
        snd[1].Enable := False;
    end;
    if snd[2].Enable then
    begin
      Dec(snd[2].Len);
      if (snd[2].Len <= 0) and (m_iram[$FF19] and $40 > 0) then
        snd[2].Enable := False;
    end;
    if snd[3].Enable then
    begin
      Dec(snd[3].Len);
      if (snd[3].Len <= 0) and (m_iram[$FF1e] and $40 > 0) then
      begin
        snd[3].Enable := False;
        m_iram[$ff1a] := m_iram[$ff1a] and $7f;
      end;
    end;
    if snd[4].Enable then
    begin
      Dec(snd[4].Len);
      if (snd[4].Len <= 0) and (m_iram[$FF23] and $40 > 0) then
        snd[4].Enable := False;
    end;
  end;
  m_iram[$FF13] := snd[1].Freq and $ff;
  m_iram[$FF14] := (m_iram[$FF14] and $f8) or ((snd[1].Freq shr 8) and 7);
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
        Inc(ls[1], vol[snd[1].Vol])
      else
        Dec(ls[1], vol[snd[1].Vol]);

    if m_iram[$FF25] and $10 > 0 then
      if snd[1].bit > 0 then
        Inc(rs[1], vol[snd[1].Vol])
      else
        Dec(rs[1], vol[snd[1].Vol]);
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
        Inc(ls[2], vol[snd[2].Vol])
      else
        Dec(ls[2], vol[snd[2].Vol]);

    if m_iram[$FF25] and $20 > 0 then
      if snd[2].Bit > 0 then
        Inc(rs[2], vol[snd[2].Vol])
      else
        Dec(rs[2], vol[snd[2].Vol]);
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
        snd[3].Bit := snd[3].Bit and $f
      else
        snd[3].Bit := snd[3].Bit shr 4;

      case (m_iram[$FF1c] shr 5) and 3 of
        0: snd[3].Bit := 8;
        1: ;
        2: snd[3].Bit := 8 or (snd[3].Bit shr 1);
        3: snd[3].Bit := $c or (snd[3].Bit shr 2);
      end;

      if m_iram[$FF25] and 4 > 0 then
        Inc(ls[3], (snd[3].Bit shl 4) - $80);
      if m_iram[$FF25] and $40 > 0 then
        Inc(rs[3], (snd[3].Bit shl 4) - $80);
    end;
  end;
  if not snd[4].channelOFF then
  begin

    if snd[4].Enable then
    begin
      Inc(freq4Clk, cycles);
      if (freq4Clk >= snd[4].Freq) then
      begin
        freq4Clk := freq4Clk mod snd[4].Freq;
        snd[4].Bit := NextLFSRBit((m_iram[$FF22] and %1000) <> 0);
      end;

      if (m_iram[$FF25] and 8) > 0 then
        if snd[4].Bit > 0 then
          Inc(ls[4], vol[snd[4].Vol])
        else
          Dec(ls[4], vol[snd[4].Vol]);
      if (m_iram[$FF25] and $80) > 0 then
        if snd[4].Bit > 0 then
          Inc(rs[4], vol[snd[4].Vol])
        else
          Dec(rs[4], vol[snd[4].Vol]);
    end;
  end;

  for I := 1 to 4 do begin
    SampleBuffers[I].BufferL[SampleBuffers[I].Cursor] := ls[I];
    SampleBuffers[I].BufferR[SampleBuffers[I].Cursor] := rs[I];
    SampleBuffers[I].Cursor := (SampleBuffers[I].Cursor + 1) mod SAMPLE_BUFFER_SIZE;
  end;

  l := Trunc((ls[1] + ls[2] + ls[3] + ls[4]) * (((m_iram[$FF24] and 7)+1) / 8));
  r := Trunc((rs[1] + rs[2] + rs[3] + rs[4]) * ((((m_iram[$FF24] shr 4) and 7)+1) / 8));

  SampleBuffers[0].BufferL[SampleBuffers[0].Cursor] := l;
  SampleBuffers[0].BufferR[SampleBuffers[0].Cursor] := r;
  SampleBuffers[0].Cursor := (SampleBuffers[0].Cursor + 1) mod SAMPLE_BUFFER_SIZE;

  SoundOutBits(l, r, cycles);
end;

begin
  ResetSound
end.
