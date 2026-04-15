unit FurImport;

{$mode objfpc}{$H+}
{$scopedenums on}

// Ported from fur2uge (C#, MIT): https://github.com/potatoTeto/fur2uge
// Loads a Furnace Tracker .fur file directly into a hUGETracker TSong.
// Preserves fur2uge's known limitations: GB module only, 64-row patterns,
// 15 instruments/channel type, hardware envelopes only, one effect column.

interface

uses
  Classes, SysUtils, Song, HugeDatatypes, Constants, Utils, fgl;

type
  EFurException = class(Exception);

function LoadSongFromFurStream(Stream: TStream): TSong;

implementation

uses
  zstream, Math;

const
  FUR_MAGIC = '-Furnace module-';
  CHIP_GAME_BOY = $04;

  // fur effect opcodes
  FX_ARPEGGIO       = $00;
  FX_PORTA_UP       = $01;
  FX_PORTA_DOWN     = $02;
  FX_TONE_PORTA     = $03;
  FX_VIBRATO        = $04;
  FX_HW_PAN         = $08;
  FX_VOLUME_SLIDE   = $0A;
  FX_POSITION_JUMP  = $0B;
  FX_PATTERN_BREAK  = $0D;
  FX_SET_SPEED      = $0F;
  FX_SET_DUTY       = $12;
  FX_SOFTWARE_PAN   = $80;
  FX_NOTE_CUT       = $EC;
  FX_NOTE_DELAY     = $ED;
  FX_EMPTY          = $F1;

  // hUGE effect codes (as they appear in TCell.EffectCode)
  HFX_ARPEGGIO       = $0;
  HFX_PORTA_UP       = $1;
  HFX_PORTA_DOWN     = $2;
  HFX_TONE_PORTA     = $3;
  HFX_VIBRATO        = $4;
  HFX_CALL_ROUTINE   = $6;
  HFX_NOTE_DELAY     = $7;
  HFX_SET_PANNING    = $8;
  HFX_SET_DUTY       = $9;
  HFX_VOLUME_SLIDE   = $A;
  HFX_POSITION_JUMP  = $B;
  HFX_SET_VOL        = $C;
  HFX_PATTERN_BREAK  = $D;
  HFX_NOTE_CUT       = $E;
  HFX_SET_SPEED      = $F;

  // fur instrument feature codes
  FEAT_NAME   = 1;
  FEAT_MACRO  = 2;
  FEAT_GB     = 3;
  FEAT_WS     = 4;
  FEAT_END    = 99;
  FEAT_IGNORE = 0;

  // fur macro codes
  MC_VOL     = 0;
  MC_ARP     = 1;
  MC_DUTY    = 2;
  MC_WAVE    = 3;
  MC_PITCH   = 4;
  MC_PAN_L   = 12;

type
  TFurMacro = record
    Code: Integer;
    LoopPoint: Byte;
    Data: array of Integer;
  end;

  TFurGBHWSeq = record
    CmdType: Byte;
    // For SET_SWEEP:
    SweepSpeed: Byte;
    SweepDir: Integer;
    ShiftVal: Byte;
  end;

  TFurGBInstr = record
    Present: Boolean;
    EnvLen: Byte;
    EnvDir: Integer;    // 0 = decrease, 1 = increase
    EnvVol: Byte;
    SndLen: Byte;
    Flags: Byte;
    HWSeq: array of TFurGBHWSeq;
  end;

  TFurWSInstr = record
    Present: Boolean;
    FirstWave: Integer;
    Enabled: Boolean;
  end;

  TFurInstrument = class
    Name: String;
    InstFormatVersion: Integer;
    ID: Integer;
    GB: TFurGBInstr;
    WS: TFurWSInstr;
    Macros: array of TFurMacro;
    procedure AddMacro(const M: TFurMacro);
  end;

  TFurInstrumentList = specialize TFPGObjectList<TFurInstrument>;

  TFurCell = record
    Row: Integer;
    Note: Integer;           // -9999 if absent
    Instrument: Integer;     // -9999 if absent
    Volume: Integer;         // -9999 if absent
    // Only effect column 0 matters for our purposes; keep up to 8 anyway
    FxPresent:    array[0..7] of Boolean;
    FxValPresent: array[0..7] of Boolean;
    Fx:           array[0..7] of Byte;
    FxVal:        array[0..7] of Byte;
  end;

  TFurPattern = class
    Length: Integer;
    Rows: array of TFurCell;
    procedure Add(const C: TFurCell);
  end;

  TFurPatternMap = specialize TFPGMapObject<Integer, TFurPattern>;

  { TFurReader — parses fur binary and exposes everything ParseSong needs }

  TFurReader = class
  private
    FData: TBytes;
    FPos: Int64;
    FVersion: Integer;

    // Module info
    FSongName: String;
    FAuthor: String;
    FSongComment: String;

    // Song 0 (we only care about song 0; fur subsongs are ignored)
    FTimeBase, FSpeed1, FHighlightA: Byte;
    FTicksPerSecond: Single;
    FPatternLen: Integer;
    FOrdersLen: Integer;
    FVirtualTempoNum, FVirtualTempoDen: Word;
    FSpeedPatternLen: Integer;
    FSpeedPattern: array[0..15] of Byte;

    FInstrumentCount, FWavetableCount, FSampleCount: Integer;
    FPatternCountGlobal: Integer;
    FTotalChanCount: Integer;

    FInstrumentPointers, FWavetablePointers, FSamplePointers, FPatternPointers:
      array of Integer;

    // orders[chan, row]
    FOrderTable: array of array of Integer;

    FInstruments: TFurInstrumentList;
    // Waveforms as raw 32-byte arrays of 4-bit values
    FWaveforms: array of TBytes;

    // Patterns per channel, keyed by pattern index
    FPatterns: array[0..3] of TFurPatternMap;

    function ReadU8: Byte;
    function ReadU16: Word;
    function ReadU32: Cardinal;
    function ReadI32: Integer;
    function ReadF32: Single;
    function ReadBytes(Count: Integer): TBytes;
    function ReadCString: String;
    procedure Seek(Offset: Int64);
    procedure Skip(Count: Integer);

    procedure ParseHeader;
    procedure ParseSongInfo;     // Old INFO-block layout (<v200)
    procedure ParseInfo2;        // New INF2-block layout (v200+)
    procedure ParseSng2(Offset: Integer);
    procedure ParseInstruments;
    procedure ParseWavetables;
    procedure ParsePatterns;
  public
    constructor Create(RawBytes: TBytes);
    destructor Destroy; override;

    procedure Parse;

    property Version: Integer read FVersion;
    property SongName: String read FSongName;
    property Author: String read FAuthor;
    property SongComment: String read FSongComment;
    property PatternLen: Integer read FPatternLen;
    property OrdersLen: Integer read FOrdersLen;
    property TimeBase: Byte read FTimeBase;
    property Speed1: Byte read FSpeed1;
    property HighlightA: Byte read FHighlightA;
    property TicksPerSecond: Single read FTicksPerSecond;
    property VirtualTempoNum: Word read FVirtualTempoNum;
    property VirtualTempoDen: Word read FVirtualTempoDen;
    property SpeedPatternLen: Integer read FSpeedPatternLen;
    property InstrumentCount: Integer read FInstrumentCount;
    property WavetableCount: Integer read FWavetableCount;
    property TotalChanCount: Integer read FTotalChanCount;
    property Instruments: TFurInstrumentList read FInstruments;
    function OrderAt(Chan, Row: Integer): Integer;

    function SpeedPatternValue(I: Integer): Byte;
    function Waveform(I: Integer): TBytes;
    function GetPattern(Chan, Index: Integer): TFurPattern;
  end;

{ TFurInstrument }

procedure TFurInstrument.AddMacro(const M: TFurMacro);
begin
  SetLength(Macros, System.Length(Macros) + 1);
  Macros[System.Length(Macros) - 1] := M;
end;

{ TFurPattern }

procedure TFurPattern.Add(const C: TFurCell);
begin
  SetLength(Rows, System.Length(Rows) + 1);
  Rows[System.Length(Rows) - 1] := C;
end;

{ TFurReader }

constructor TFurReader.Create(RawBytes: TBytes);
var
  I: Integer;
begin
  inherited Create;
  FData := RawBytes;
  FPos := 0;
  FInstruments := TFurInstrumentList.Create(True);
  for I := 0 to 3 do
    FPatterns[I] := TFurPatternMap.Create(True);
end;

destructor TFurReader.Destroy;
var
  I: Integer;
begin
  FInstruments.Free;
  for I := 0 to 3 do
    FPatterns[I].Free;
  inherited;
end;

function TFurReader.ReadU8: Byte;
begin
  if FPos >= Length(FData) then
    raise EFurException.Create('Unexpected EOF while parsing .fur');
  Result := FData[FPos];
  Inc(FPos);
end;

function TFurReader.ReadU16: Word;
begin
  Result := ReadU8 or (ReadU8 shl 8);
end;

function TFurReader.ReadU32: Cardinal;
begin
  Result := Cardinal(ReadU8)
         or (Cardinal(ReadU8) shl 8)
         or (Cardinal(ReadU8) shl 16)
         or (Cardinal(ReadU8) shl 24);
end;

function TFurReader.ReadI32: Integer;
begin
  Result := Integer(ReadU32);
end;

function TFurReader.ReadF32: Single;
var
  V: Cardinal;
begin
  V := ReadU32;
  Move(V, Result, 4);
end;

function TFurReader.ReadBytes(Count: Integer): TBytes;
begin
  if FPos + Count > Length(FData) then
    raise EFurException.Create('Unexpected EOF while reading block');
  SetLength(Result, Count);
  if Count > 0 then
    Move(FData[FPos], Result[0], Count);
  Inc(FPos, Count);
end;

function TFurReader.ReadCString: String;
var
  Start: Int64;
  Len, Safety: Integer;
begin
  Start := FPos;
  Safety := 5000;
  while (FPos < Length(FData)) and (FData[FPos] <> 0) do begin
    Inc(FPos);
    Dec(Safety);
    if Safety <= 0 then
      raise EFurException.Create('String too long or corrupted');
  end;
  Len := FPos - Start;
  SetLength(Result, Len);
  if Len > 0 then
    Move(FData[Start], Result[1], Len);
  if FPos < Length(FData) then
    Inc(FPos); // skip null
end;

procedure TFurReader.Seek(Offset: Int64);
begin
  if (Offset < 0) or (Offset > Length(FData)) then
    raise EFurException.CreateFmt('Bad fur offset: %d', [Offset]);
  FPos := Offset;
end;

procedure TFurReader.Skip(Count: Integer);
begin
  Seek(FPos + Count);
end;

procedure TFurReader.ParseHeader;
var
  MagicBytes: TBytes;
  Magic: String;
begin
  MagicBytes := ReadBytes(16);
  SetLength(Magic, 16);
  Move(MagicBytes[0], Magic[1], 16);
  if Magic <> FUR_MAGIC then
    raise EFurException.Create('Not a Furnace module (bad magic).');

  FVersion := ReadU16;
  ReadU16;      // reserved
  ReadI32;      // song info pointer (we're already positioned after header)
  Skip(8);      // reserved2

  if FVersion < 157 then
    raise EFurException.CreateFmt(
      'Furnace file uses a legacy format (version %d). Open in a recent ' +
      'Furnace (0.6+) and re-save — import requires format version 157 or later.',
      [FVersion]);
end;

procedure TFurReader.ParseInfo2;
var
  InfStart, PayloadEnd: Int64;
  I, Conn: Integer;
  ElemType: Byte;
  ElemCount: Integer;
  Ptr, K: Integer;
  ChipID, ChipChanCount: Integer;
  ChipCount: Integer;
  Sng2Pointer: Integer;
begin
  // At entry we've already consumed the "INF2" block id. Payload begins
  // with block size (u32).
  PayloadEnd := FPos - 4; // will be updated once we read size
  ReadU32; // block size (we'll walk linearly via element pointers)
  InfStart := FPos;

  // 8 cstrings: song name, author, system name, album, + 4 JP variants
  FSongName    := ReadCString;
  FAuthor      := ReadCString;
  ReadCString; // system name
  ReadCString; // album/category/game
  ReadCString; // song name JP
  ReadCString; // author JP
  ReadCString; // system JP
  ReadCString; // album JP

  ReadF32;     // A-4 tuning
  ReadU8;      // auto-sys-name flag
  ReadF32;     // master volume
  FTotalChanCount := ReadU16;
  ChipCount := ReadU16;
  if ChipCount < 1 then
    raise EFurException.Create('Furnace module has no sound chips.');

  for I := 0 to ChipCount - 1 do begin
    ChipID := ReadU16;
    ChipChanCount := ReadU16;
    ReadF32; // vol
    ReadF32; // pan
    ReadF32; // fr bal
    if (I = 0) and (ChipID <> CHIP_GAME_BOY) then
      raise EFurException.CreateFmt(
        'This .fur uses chip type $%.4x. Only pure Game Boy modules are supported.',
        [ChipID]);
  end;

  // Patchbay: u32 count, count × u32 connections, u8 auto flag
  Conn := ReadI32;
  Skip(Conn * 4);
  ReadU8; // auto patchbay flag

  // Song Elements loop until type = 0
  Sng2Pointer := -1;
  SetLength(FInstrumentPointers, 0);
  SetLength(FWavetablePointers, 0);
  SetLength(FSamplePointers, 0);
  SetLength(FPatternPointers, 0);
  FInstrumentCount := 0;
  FWavetableCount := 0;
  FSampleCount := 0;
  FPatternCountGlobal := 0;

  repeat
    ElemType := ReadU8;
    if ElemType = 0 then Break;
    ElemCount := ReadI32;

    case ElemType of
      $01: begin // SNG2 (songs)
        for K := 0 to ElemCount - 1 do begin
          Ptr := ReadI32;
          if (Sng2Pointer = -1) and (Ptr <> 0) then
            Sng2Pointer := Ptr;
        end;
      end;
      $04: begin // INS2
        for K := 0 to ElemCount - 1 do begin
          Ptr := ReadI32;
          if Ptr <> 0 then begin
            SetLength(FInstrumentPointers, FInstrumentCount + 1);
            FInstrumentPointers[FInstrumentCount] := Ptr;
            Inc(FInstrumentCount);
          end;
        end;
      end;
      $05: begin // WAVE
        for K := 0 to ElemCount - 1 do begin
          Ptr := ReadI32;
          if Ptr <> 0 then begin
            SetLength(FWavetablePointers, FWavetableCount + 1);
            FWavetablePointers[FWavetableCount] := Ptr;
            Inc(FWavetableCount);
          end;
        end;
      end;
      $06: begin // SMP2
        for K := 0 to ElemCount - 1 do begin
          Ptr := ReadI32;
          if Ptr <> 0 then begin
            SetLength(FSamplePointers, FSampleCount + 1);
            FSamplePointers[FSampleCount] := Ptr;
            Inc(FSampleCount);
          end;
        end;
      end;
      $07: begin // PATN
        for K := 0 to ElemCount - 1 do begin
          Ptr := ReadI32;
          if Ptr <> 0 then begin
            SetLength(FPatternPointers, FPatternCountGlobal + 1);
            FPatternPointers[FPatternCountGlobal] := Ptr;
            Inc(FPatternCountGlobal);
          end;
        end;
      end;
    else
      // Skip unknown element's pointer list
      Skip(ElemCount * 4);
    end;
  until False;

  if Sng2Pointer < 0 then
    raise EFurException.Create('INF2 block did not list any subsong (SNG2).');

  ParseSng2(Sng2Pointer);
end;

procedure TFurReader.ParseSng2(Offset: Integer);
var
  Magic: TBytes;
  I, X, Y: Integer;
  OrderVal: Byte;
  First16Speed: Word;
begin
  Seek(Offset);
  Magic := ReadBytes(4); // "SNG2"
  if (System.Length(Magic) < 4) or (Magic[0] <> Ord('S')) or (Magic[1] <> Ord('N'))
     or (Magic[2] <> Ord('G')) or (Magic[3] <> Ord('2')) then
    raise EFurException.Create('Expected SNG2 block.');
  ReadI32; // size

  FTicksPerSecond := ReadF32;
  ReadU8; // initial arp time (ignored)
  FTimeBase := ReadU8; // effect speed divider (a.k.a. time base)
  FPatternLen := ReadU16;
  FOrdersLen := ReadU16;
  FHighlightA := ReadU8;
  ReadU8; // highlight B
  FVirtualTempoNum := ReadU16;
  FVirtualTempoDen := ReadU16;
  FSpeedPatternLen := ReadU8;
  for I := 0 to 15 do begin
    First16Speed := ReadU16;
    if I = 0 then FSpeed1 := Byte(First16Speed);
    if I < 16 then FSpeedPattern[I] := Byte(First16Speed);
  end;

  ReadCString; // subsong name
  ReadCString; // subsong comment

  // Order table — (channels × orders) bytes, chan-major
  SetLength(FOrderTable, FTotalChanCount);
  for X := 0 to FTotalChanCount - 1 do begin
    SetLength(FOrderTable[X], FOrdersLen);
    for Y := 0 to FOrdersLen - 1 do begin
      OrderVal := ReadU8;
      FOrderTable[X, Y] := OrderVal;
    end;
  end;

  Skip(FTotalChanCount);  // effect columns
  Skip(FTotalChanCount);  // hide status
  Skip(FTotalChanCount);  // collapse status
  for X := 0 to FTotalChanCount - 1 do ReadCString;  // chan name
  for X := 0 to FTotalChanCount - 1 do ReadCString;  // chan short name
  Skip(FTotalChanCount * 4);  // chan colors (ABGR)
end;

procedure TFurReader.ParseSongInfo;
var
  InfoMagic: TBytes;
  Chip: Byte;
  X, Y: Integer;
  OrderVal: Byte;
  ChipCount: Integer;
  AdditionalSubsongs: Integer;
  SubsongPtrs: Integer;
begin
  InfoMagic := ReadBytes(4); // "INFO"
  ReadI32;                   // block size

  FTimeBase := ReadU8;
  FSpeed1 := ReadU8;
  ReadU8; // speed2
  ReadU8; // initial arp time
  FTicksPerSecond := ReadF32;
  FPatternLen := ReadU16;
  FOrdersLen := ReadU16;
  FHighlightA := ReadU8;
  ReadU8; // highlightB

  FInstrumentCount := ReadU16;
  FWavetableCount := ReadU16;
  FSampleCount := ReadU16;
  FPatternCountGlobal := ReadI32;

  // Chip types list (32 bytes)
  ChipCount := 0;
  FTotalChanCount := 0;
  for X := 0 to 31 do begin
    Chip := ReadU8;
    if Chip = 0 then Continue;
    if Chip <> CHIP_GAME_BOY then
      raise EFurException.CreateFmt(
        'This .fur uses chip $%.2x. Only pure Game Boy modules are supported.',
        [Chip]);
    Inc(ChipCount);
    Inc(FTotalChanCount, 4);
  end;
  if ChipCount = 0 then
    raise EFurException.Create('Module has no sound chips.');
  // Skip rest of the chip list (we already read all 32)

  Skip(32); // chip vol list
  Skip(32); // chip pan list
  if FVersion >= 118 then
    Skip(128) // chip flag pointers
  else
    Skip(128); // legacy flags (one byte each)

  FSongName := ReadCString;
  FAuthor := ReadCString;

  ReadF32; // A4 tuning
  Skip(20); // Limit..ResetNoteBaseOnArpEffectStop compat flags

  // Instrument/Wavetable/Sample/Pattern pointer tables
  SetLength(FInstrumentPointers, FInstrumentCount);
  for X := 0 to FInstrumentCount - 1 do
    FInstrumentPointers[X] := ReadI32;

  SetLength(FWavetablePointers, FWavetableCount);
  for X := 0 to FWavetableCount - 1 do
    FWavetablePointers[X] := ReadI32;

  SetLength(FSamplePointers, FSampleCount);
  for X := 0 to FSampleCount - 1 do
    FSamplePointers[X] := ReadI32;

  SetLength(FPatternPointers, FPatternCountGlobal);
  for X := 0 to FPatternCountGlobal - 1 do
    FPatternPointers[X] := ReadI32;

  // Order table: channels outer, rows inner (like fur stores it)
  SetLength(FOrderTable, FTotalChanCount);
  for X := 0 to FTotalChanCount - 1 do begin
    SetLength(FOrderTable[X], FOrdersLen);
    for Y := 0 to FOrdersLen - 1 do begin
      OrderVal := ReadU8;
      FOrderTable[X, Y] := OrderVal;
    end;
  end;

  // Per-channel metadata
  Skip(FTotalChanCount);          // effect column counts
  Skip(FTotalChanCount);          // channel hide status
  Skip(FTotalChanCount);          // channel collapse status
  for X := 0 to FTotalChanCount - 1 do ReadCString; // chan name
  for X := 0 to FTotalChanCount - 1 do ReadCString; // chan short name

  FSongComment := ReadCString;
  ReadF32; // master volume

  // Extended compatibility flags (>= v70): 28 bytes
  Skip(28);

  FVirtualTempoNum := ReadU16;
  FVirtualTempoDen := ReadU16;

  // Subsong name/comment (for song 0) — we discard
  ReadCString;
  ReadCString;
  AdditionalSubsongs := ReadU8;
  Skip(3); // reserved

  // Subsong pointers — ignored
  for X := 0 to AdditionalSubsongs - 1 do
    SubsongPtrs := ReadI32;

  // SysName + localization strings
  ReadCString; ReadCString; ReadCString; ReadCString; ReadCString; ReadCString;

  // Extra chip output settings (only 1 chip in our GB-only case): 12 bytes
  Skip(12);

  // Patchbay (>=135) — we've already version-gated the file to >=157
  Skip(ReadI32 * 4);
  ReadU8; // AutoPatchBay

  // More compat flags (>=138): 8 bytes
  Skip(8);

  // Speed pattern (>=139): 1 byte len + 16 bytes pattern
  FSpeedPatternLen := ReadU8;
  if (FSpeedPatternLen < 0) or (FSpeedPatternLen > 16) then
    raise EFurException.CreateFmt('Speed pattern length out of range: %d',
      [FSpeedPatternLen]);
  for X := 0 to 15 do
    FSpeedPattern[X] := ReadU8;

  // Groove list
  ChipCount := ReadU8; // groove count (reusing var)
  Skip(ChipCount * 17); // len byte + 16 bytes

  // Pointers to asset directories (>=156)
  Skip(12);
end;

function FeatureCode(const S: String): Integer;
begin
  if S = 'NA' then Result := FEAT_NAME
  else if S = 'MA' then Result := FEAT_MACRO
  else if S = 'GB' then Result := FEAT_GB
  else if S = 'WS' then Result := FEAT_WS
  else if S = 'EN' then Result := FEAT_END
  else Result := FEAT_IGNORE;
end;

procedure TFurReader.ParseInstruments;
var
  I, J, K: Integer;
  FormatMagic: TBytes;
  BlockSize, InstFormatVer: Integer;
  InstrType: Word;
  FCode: String;
  FeatTag: TBytes;
  BlockLen: Integer;
  Inst: TFurInstrument;
  StopLoop: Boolean;
  Name: String;
  NameLen: Integer;
  // GB
  EnvParams: Byte;
  // Macros
  MacroHeaderLen: Integer;
  MacroCode: Byte;
  MacroLen, MacroLoop, MacroRelease, MacroMode: Byte;
  MacroOTWSByte: Byte;
  WordSize: Integer;
  MacroDelay, MacroSpeed: Byte;
  WordBytes: Integer;
  TotalBytes: Integer;
  M: TFurMacro;
  // WS
  WSFirstWave: Integer;
begin
  for I := 0 to FInstrumentCount - 1 do begin
    if FVersion < 127 then
      Continue; // Legacy instrument format not supported

    Seek(FInstrumentPointers[I]);
    FormatMagic := ReadBytes(4);       // "INS2"
    BlockSize := ReadI32;
    InstFormatVer := ReadU16;
    InstrType := ReadU16;

    Inst := TFurInstrument.Create;
    Inst.InstFormatVersion := InstFormatVer;
    Inst.ID := FInstruments.Count;
    // Default GB envelope (matches fur2uge defaults so empty-GB instruments work)
    Inst.GB.Present := True;
    Inst.GB.EnvLen := 2;
    Inst.GB.EnvDir := 0;
    Inst.GB.EnvVol := 15;
    Inst.GB.SndLen := 2;
    FInstruments.Add(Inst);

    StopLoop := False;
    while not StopLoop do begin
      FeatTag := ReadBytes(2);
      SetLength(FCode, 2);
      Move(FeatTag[0], FCode[1], 2);
      BlockLen := ReadU16;

      case FeatureCode(FCode) of
        FEAT_END:
          StopLoop := True;

        FEAT_NAME: begin
          Name := ReadCString;
          Inst.Name := Name;
          NameLen := System.Length(Name) + 1;
          if BlockLen > NameLen then
            Skip(BlockLen - NameLen);
        end;

        FEAT_MACRO: begin
          MacroHeaderLen := ReadU16;
          Dec(BlockLen, 2);
          while BlockLen > 0 do begin
            if BlockLen = 1 then begin
              ReadU8;
              Dec(BlockLen);
              Break;
            end;
            MacroCode := ReadU8;
            MacroLen := ReadU8;
            MacroLoop := ReadU8;
            MacroRelease := ReadU8;
            MacroMode := ReadU8;
            MacroOTWSByte := ReadU8;
            WordSize := ((MacroOTWSByte shr 6) and $03); // bits 6,7
            MacroDelay := ReadU8;
            MacroSpeed := ReadU8;
            Dec(BlockLen, 8);

            case WordSize of
              0: WordBytes := 1; // U8
              1: WordBytes := 1; // S8
              2: WordBytes := 2; // S16
              3: WordBytes := 4; // S32
            end;
            TotalBytes := MacroLen * WordBytes;

            M.Code := MacroCode;
            M.LoopPoint := MacroLoop;
            SetLength(M.Data, MacroLen);
            for J := 0 to MacroLen - 1 do begin
              case WordSize of
                0: M.Data[J] := ReadU8;
                1: M.Data[J] := ShortInt(ReadU8);
                2: M.Data[J] := SmallInt(ReadU16);
                3: M.Data[J] := ReadI32;
              end;
            end;
            Dec(BlockLen, TotalBytes);

            if MacroCode <> $FF then
              Inst.AddMacro(M);
            SetLength(M.Data, 0);
          end;
        end;

        FEAT_GB: begin
          EnvParams := ReadU8;
          Inst.GB.EnvLen := (EnvParams shr 5) and $07; // bits 5,6,7
          if (EnvParams and $10) <> 0 then
            Inst.GB.EnvDir := 1
          else
            Inst.GB.EnvDir := 0;
          Inst.GB.EnvVol := EnvParams and $0F;
          Inst.GB.SndLen := ReadU8;
          Inst.GB.Flags := ReadU8;
          K := ReadU8; // hw seq len
          Dec(BlockLen, 4);

          SetLength(Inst.GB.HWSeq, K);
          for J := 0 to K - 1 do begin
            Inst.GB.HWSeq[J].CmdType := ReadU8;
            MacroCode := ReadU8; // reuse var as data1
            MacroLen := ReadU8;  // data2

            if Inst.GB.HWSeq[J].CmdType = 1 then begin
              // SET_SWEEP
              Inst.GB.HWSeq[J].SweepSpeed := (MacroCode shr 4) and $07;
              if (MacroCode and $08) <> 0 then
                Inst.GB.HWSeq[J].SweepDir := 1
              else
                Inst.GB.HWSeq[J].SweepDir := 0;
              Inst.GB.HWSeq[J].ShiftVal := MacroCode and $07;
            end;
            Dec(BlockLen, 3);
          end;
          if BlockLen > 0 then Skip(BlockLen);
        end;

        FEAT_WS: begin
          Inst.WS.Present := True;
          WSFirstWave := ReadI32;
          ReadI32;         // second wave
          ReadU8;          // rate divider
          ReadU8;          // effect
          Inst.WS.Enabled := ReadU8 > 0;
          ReadU8;          // global
          ReadU8;          // speed
          Skip(4);         // param1..4
          Inst.WS.FirstWave := WSFirstWave;
          Dec(BlockLen, 17);
          if BlockLen > 0 then Skip(BlockLen);
        end;

      else
        // unhandled feature — skip its payload
        Skip(BlockLen);
      end;
    end;
  end;
end;

procedure TFurReader.ParseWavetables;
var
  I, J: Integer;
  Width: Integer;
  Block: TBytes;
begin
  SetLength(FWaveforms, FWavetableCount);
  for I := 0 to FWavetableCount - 1 do begin
    Seek(FWavetablePointers[I]);
    ReadBytes(4);      // "WAVE"
    ReadI32;           // block size
    ReadCString;       // name
    Width := ReadI32;
    ReadI32;           // reserved
    ReadI32;           // height

    SetLength(Block, Width);
    for J := 0 to Width - 1 do
      Block[J] := Byte(ReadI32 and $FF);
    FWaveforms[I] := Block;
  end;
end;

procedure TFurReader.ParsePatterns;
var
  I, J: Integer;
  SubSong, ChannelID, Ver: Integer;
  PatIndex: Word;
  Row: Integer;
  Pat: TFurPattern;
  Cell: TFurCell;
  FirstByte, SecondByte, ThirdByte: Byte;
  SkipCounter: Integer;
  DataWritten: Boolean;
  NotePresent, InstPresent, VolPresent: Boolean;
  FxPresent, FxValPresent: array[0..7] of Boolean;
  Bit5Set, Bit6Set: Boolean;
  PatternsParsed: Integer;

  procedure ClearCell(out C: TFurCell);
  var K: Integer;
  begin
    C.Row := 0;
    C.Note := -9999;
    C.Instrument := -9999;
    C.Volume := -9999;
    for K := 0 to 7 do begin
      C.FxPresent[K] := False;
      C.FxValPresent[K] := False;
      C.Fx[K] := 0;
      C.FxVal[K] := 0;
    end;
  end;

begin
  PatternsParsed := 0;
  for I := 0 to FPatternCountGlobal - 1 do begin
    if FVersion < 157 then Continue;

    Seek(FPatternPointers[I]);
    ReadBytes(4);                 // "PATN" block id
    ReadI32;                      // block size
    SubSong := ReadU8;
    ChannelID := ReadU8;
    PatIndex := ReadU16;
    ReadCString;                  // pattern name

    if SubSong <> 0 then Continue; // we only handle song 0
    if ChannelID > 3 then Continue;

    Pat := TFurPattern.Create;
    Pat.Length := FPatternLen;
    FPatterns[ChannelID].Add(PatIndex, Pat);
    Inc(PatternsParsed);

    Row := 0;
    while Row < FPatternLen do begin
      FirstByte := ReadU8;
      if FirstByte = $FF then Break;

      if (FirstByte and $80) <> 0 then begin
        SkipCounter := FirstByte and $3F;
        Inc(Row, SkipCounter + 2);
        Continue;
      end;

      ClearCell(Cell);
      Cell.Row := Row;
      DataWritten := False;

      NotePresent   := (FirstByte and $01) <> 0;
      InstPresent   := (FirstByte and $02) <> 0;
      VolPresent    := (FirstByte and $04) <> 0;
      FxPresent[0]    := (FirstByte and $08) <> 0;
      FxValPresent[0] := (FirstByte and $10) <> 0;
      Bit5Set       := (FirstByte and $20) <> 0;
      Bit6Set       := (FirstByte and $40) <> 0;
      for J := 1 to 7 do begin
        FxPresent[J] := False;
        FxValPresent[J] := False;
      end;

      if Bit5Set then begin
        SecondByte := ReadU8;
        FxPresent[0]    := (SecondByte and $01) <> 0;
        FxValPresent[0] := (SecondByte and $02) <> 0;
        FxPresent[1]    := (SecondByte and $04) <> 0;
        FxValPresent[1] := (SecondByte and $08) <> 0;
        FxPresent[2]    := (SecondByte and $10) <> 0;
        FxValPresent[2] := (SecondByte and $20) <> 0;
        FxPresent[3]    := (SecondByte and $40) <> 0;
        FxValPresent[3] := (SecondByte and $80) <> 0;
        if (SecondByte and $40) <> 0 then begin
          ThirdByte := ReadU8;
          FxPresent[4]    := (ThirdByte and $01) <> 0;
          FxValPresent[4] := (ThirdByte and $02) <> 0;
          FxPresent[5]    := (ThirdByte and $04) <> 0;
          FxValPresent[5] := (ThirdByte and $08) <> 0;
          FxPresent[6]    := (ThirdByte and $10) <> 0;
          FxValPresent[6] := (ThirdByte and $20) <> 0;
          FxPresent[7]    := (ThirdByte and $40) <> 0;
          FxValPresent[7] := (ThirdByte and $80) <> 0;
        end;
      end;

      if NotePresent  then begin Cell.Note := ReadU8; DataWritten := True; end;
      if InstPresent  then begin Cell.Instrument := ReadU8; DataWritten := True; end;
      if VolPresent   then begin Cell.Volume := ReadU8; DataWritten := True; end;

      for J := 0 to 7 do begin
        Cell.FxPresent[J] := FxPresent[J];
        Cell.FxValPresent[J] := FxValPresent[J];
        if FxPresent[J]    then begin Cell.Fx[J] := ReadU8; DataWritten := True; end;
        if FxValPresent[J] then begin Cell.FxVal[J] := ReadU8; DataWritten := True; end;
      end;

      if DataWritten then
        Pat.Add(Cell);

      Inc(Row);
    end;
  end;
end;

procedure TFurReader.Parse;
var
  BlockId: TBytes;
  Tag: String;
begin
  ParseHeader;

  // Peek at the first block ID to decide between INFO (old) and INF2 (new).
  BlockId := ReadBytes(4);
  SetLength(Tag, 4);
  Move(BlockId[0], Tag[1], 4);
  if Tag = 'INF2' then
    ParseInfo2
  else if Tag = 'INFO' then begin
    // Rewind so ParseSongInfo can read the block ID again.
    Seek(FPos - 4);
    ParseSongInfo;
  end else
    raise EFurException.CreateFmt(
      'Unexpected block after header: %s (expected INFO or INF2).', [Tag]);

  ParseInstruments;
  ParseWavetables;
  ParsePatterns;
end;

function TFurReader.SpeedPatternValue(I: Integer): Byte;
begin
  if (I < 0) or (I > 15) then
    Result := 0
  else
    Result := FSpeedPattern[I];
end;

function TFurReader.Waveform(I: Integer): TBytes;
begin
  if (I < 0) or (I >= System.Length(FWaveforms)) then
    Result := nil
  else
    Result := FWaveforms[I];
end;

function TFurReader.OrderAt(Chan, Row: Integer): Integer;
begin
  Result := FOrderTable[Chan, Row];
end;

function TFurReader.GetPattern(Chan, Index: Integer): TFurPattern;
var
  Idx: Integer;
begin
  Result := nil;
  Idx := FPatterns[Chan].IndexOf(Index);
  if Idx >= 0 then
    Result := FPatterns[Chan].Data[Idx];
end;

{ ==== Conversion helpers ==== }

function FurNoteToUgeNote(FurNote: Integer): Integer;
begin
  if FurNote = 180 then
    Result := 180  // sentinel meaning "note cut"
  else
    Result := FurNote - 84;
end;

// Compute Furnace→hUGE (TimerDivider, TicksPerRow) for a non-GBStudio target
procedure ConvertFurTempo(TimeBase: Integer; AvgSpeed: Double;
  VTNum, VTDen: Integer; TickRateHz: Double; HighlightA: Integer;
  out TicksPerRow, TimerDivider: Integer);
var
  HL, TB, VN, VD, SourceBpm, Bpm, BestErr, Err: Double;
  Div_: Integer;
  BestDiv: Integer;
begin
  if HighlightA > 0 then HL := HighlightA else HL := 4.0;
  if (TimeBase + 1) > 1 then TB := TimeBase + 1 else TB := 1.0;
  VN := Max(VTNum, 1);
  VD := Max(VTDen, 1);

  SourceBpm := (60.0 * TickRateHz) / (AvgSpeed * HL * TB) * (VN / VD);

  TicksPerRow := Round(AvgSpeed);
  if TicksPerRow < 1 then TicksPerRow := 1;

  BestErr := 1e30;
  BestDiv := 0;
  for Div_ := 1 to 254 do begin
    Bpm := (4096.0 * 15.0) / ((256 - Div_) * TicksPerRow);
    Err := Abs(Bpm - SourceBpm);
    if Err < BestErr then begin
      BestErr := Err;
      BestDiv := Div_;
    end;
  end;
  TimerDivider := BestDiv;
end;

function ZLibDecompress(Stream: TStream): TBytes;
var
  Ds: TDecompressionStream;
  Buffer: TBytes;
  Read, Total, Cap: Integer;
begin
  Stream.Position := 0;
  Ds := TDecompressionStream.Create(Stream);
  try
    Total := 0;
    Cap := 65536;
    SetLength(Buffer, Cap);
    repeat
      if Total + 65536 > Cap then begin
        Cap := Cap * 2;
        SetLength(Buffer, Cap);
      end;
      Read := Ds.Read(Buffer[Total], Cap - Total);
      Inc(Total, Read);
    until Read = 0;
    SetLength(Buffer, Total);
    Result := Buffer;
  finally
    Ds.Free;
  end;
end;

function LoadAllBytes(Stream: TStream): TBytes;
var
  N: Int64;
begin
  Stream.Position := 0;
  N := Stream.Size;
  SetLength(Result, N);
  if N > 0 then
    Stream.ReadBuffer(Result[0], N);
end;

{ ==== Subpattern population from Furnace macros ==== }

procedure ApplyMacrosToSubpattern(var Sub: TPattern; const Inst: TFurInstrument;
  PanMacroOnChannel: Integer);
var
  I, J, BitA, BitB, LoopOffset: Integer;
  HighestRow, HighestLoop: Integer;
  HadJumpable: Boolean;
  Data: array of Integer;
  LoopPt: Integer;
  PanVal, PanFinal: Integer;
  RightOn, LeftOn: Boolean;
  ArpVal, PitchVal: ShortInt;
  HasSubpatternData: Boolean;
begin
  BlankPattern(@Sub);
  HadJumpable := False;
  HighestRow := 0;
  HighestLoop := 0;
  HasSubpatternData := False;

  for I := System.Length(Inst.Macros) - 1 downto 0 do begin
    Data := Inst.Macros[I].Data;
    LoopPt := Inst.Macros[I].LoopPoint;
    if System.Length(Data) > 1 then HasSubpatternData := True;

    case Inst.Macros[I].Code of
      MC_PAN_L: begin
        if System.Length(Data) > 1 then begin
          BitA := PanMacroOnChannel - 1;
          BitB := PanMacroOnChannel + 3;
          for J := 0 to System.Length(Data) - 1 do begin
            if J > High(Sub) then Break;
            PanVal := Data[J];
            RightOn := (PanVal and 1) <> 0;
            LeftOn := (PanVal and 2) <> 0;
            PanFinal := $FF;
            if RightOn then PanFinal := PanFinal or (1 shl BitA)
                      else PanFinal := PanFinal and (not (1 shl BitA));
            if LeftOn  then PanFinal := PanFinal or (1 shl BitB)
                      else PanFinal := PanFinal and (not (1 shl BitB));
            Sub[J].EffectCode := HFX_SET_PANNING;
            Sub[J].EffectParams.Value := Byte(PanFinal);
          end;
        end;
      end;
      MC_ARP: begin
        if System.Length(Data) > 1 then begin
          for J := 0 to System.Length(Data) - 1 do begin
            if J > High(Sub) then Break;
            ArpVal := ShortInt(Data[J] and $FF);
            Sub[J].Note := $24 + ArpVal;
          end;
        end;
      end;
      MC_PITCH: begin
        if System.Length(Data) > 1 then begin
          for J := 0 to System.Length(Data) - 1 do begin
            if J > High(Sub) then Break;
            PitchVal := ShortInt(Data[J] and $FF);
            if PitchVal >= 0 then begin
              Sub[J].EffectCode := HFX_PORTA_UP;
              Sub[J].EffectParams.Value := Byte(PitchVal div 8);
            end else begin
              Sub[J].EffectCode := HFX_PORTA_DOWN;
              Sub[J].EffectParams.Value := Byte((-PitchVal) div 8);
            end;
          end;
        end;
      end;
      MC_DUTY: begin
        if System.Length(Data) > 1 then begin
          for J := 0 to System.Length(Data) - 1 do begin
            if J > High(Sub) then Break;
            Sub[J].EffectCode := HFX_SET_DUTY;
            Sub[J].EffectParams.Value := Byte(Data[J] * $40);
          end;
        end;
      end;
    end;

    if (System.Length(Data) > 1) and (LoopPt < $FF) then begin
      if HighestLoop <= LoopPt then begin
        HighestRow := System.Length(Data) - 1;
        HighestLoop := LoopPt;
        HadJumpable := True;
      end;
    end;
  end;

  if HadJumpable then begin
    LoopOffset := 0;
    if HighestLoop = HighestRow then LoopOffset := 1;
    if (HighestRow >= Low(Sub)) and (HighestRow <= High(Sub)) then
      Sub[HighestRow].Volume := HighestLoop + LoopOffset;
  end else if (System.Length(Inst.Macros) > 0) then begin
    if System.Length(Inst.Macros[0].Data) > 1 then begin
      J := System.Length(Inst.Macros[0].Data);
      if (J >= Low(Sub)) and (J <= High(Sub)) then
        Sub[J].Volume := J;
    end;
  end;
end;

{ ==== Main import ==== }

function LoadSongFromFurStream(Stream: TStream): TSong;
var
  Raw, Decompressed: TBytes;
  Reader: TFurReader;
  I, J, K, ChanID, OrderRow, OrdersLen, FurPatId, UgePatId: Integer;
  NextUgeId: Integer;
  Key: Int64;
  FurPointerMap: specialize TFPGMap<Int64, Integer>;
  Pat: PPattern;
  FurPat: TFurPattern;
  SubCell: TFurCell;
  PrevRow, RowsSincePrev, RowsToEnd: Integer;
  PatLen: Integer;
  FurFxCmd, FurFxVal, UgeFxVal: Byte;
  FurFxCmdPresent, FurFxValPresent: Boolean;
  UgeFxCmd: Integer;
  PulseInsts, WaveInsts, NoiseInsts: array of Integer; // fur IDs in order of first use
  TargetInst: Integer;
  DuplicateFound: Boolean;
  InstVal: Integer;
  NoteCode: Integer;
  OutVol: Byte;
  GbVol, GbLen, GbDir: Integer;
  TicksPerRow, TimerDivider: Integer;
  SpeedAvg: Double;
  RowIdx: Integer;
  WavData: TBytes;
  RemapInst: Integer;
  FurInst: TFurInstrument;
  HInst: ^TInstrument;
  HInstBank: TInstrumentType;
  PanLeft, PanRight: array[0..3] of Boolean;
  FinalPan: Integer;
  UseEffect: Boolean;

  function CopyContinueEffect(Cmd: Integer): Boolean;
  begin
    Result := (Cmd <> HFX_NOTE_DELAY) and (Cmd <> HFX_NOTE_CUT)
      and (Cmd <> HFX_SET_SPEED) and (Cmd <> HFX_SET_PANNING)
      and (Cmd >= 0);
  end;

  procedure SetEffectOnPat(APat: PPattern; ARow: Integer;
    Cmd: Integer; Param: Byte);
  begin
    if (ARow < Low(TPattern)) or (ARow > High(TPattern)) then Exit;
    APat^[ARow].EffectCode := Cmd;
    APat^[ARow].EffectParams.Value := Param;
  end;

begin
  InitializeSong(Result);

  Raw := LoadAllBytes(Stream);
  if System.Length(Raw) < 2 then
    raise EFurException.Create('File too small to be a .fur module.');

  if (Raw[0] = $78) and (Raw[1] = $9C) then begin
    Stream.Position := 0;
    Decompressed := ZLibDecompress(Stream);
  end else
    Decompressed := Raw;

  Reader := TFurReader.Create(Decompressed);
  FurPointerMap := specialize TFPGMap<Int64, Integer>.Create;
  try
    Reader.Parse;

    if Reader.PatternLen <> 64 then
      raise EFurException.CreateFmt(
        'Pattern length must be 64 (got %d). Adjust in Furnace and re-export.',
        [Reader.PatternLen]);
    if Reader.TotalChanCount <> 4 then
      raise EFurException.CreateFmt(
        'Expected 4 Game Boy channels, got %d.', [Reader.TotalChanCount]);

    Result.Name := Reader.SongName;
    Result.Artist := Reader.Author;
    Result.Comment := Reader.SongComment;

    // Waveforms
    for I := 0 to Min(Reader.WavetableCount, System.Length(Result.Waves)) - 1 do begin
      WavData := Reader.Waveform(I);
      if WavData = nil then Continue;
      for J := 0 to Min(System.Length(WavData), System.Length(Result.Waves[I])) - 1 do
        Result.Waves[I][J] := WavData[J] and $0F;
    end;

    // Tempo
    SpeedAvg := Reader.Speed1;
    if (Reader.SpeedPatternLen > 1) then begin
      K := 0;
      SpeedAvg := 0;
      for I := 0 to Reader.SpeedPatternLen - 1 do begin
        SpeedAvg := SpeedAvg + Reader.SpeedPatternValue(I);
        Inc(K);
      end;
      if K > 0 then SpeedAvg := SpeedAvg / K;
    end;
    ConvertFurTempo(Reader.TimeBase, SpeedAvg,
      Reader.VirtualTempoNum, Reader.VirtualTempoDen,
      Reader.TicksPerSecond, Reader.HighlightA,
      TicksPerRow, TimerDivider);
    Result.TicksPerRow := TicksPerRow;
    Result.TimerDivider := TimerDivider;
    Result.TimerEnabled := True;

    // Build unique (channel, fur-pattern) → uge-pattern-id mapping
    OrdersLen := Reader.OrdersLen;
    NextUgeId := 0;
    for I := 0 to 3 do begin
      SetLength(Result.OrderMatrix[I], OrdersLen + 1);
    end;
    for ChanID := 0 to 3 do begin
      for OrderRow := 0 to OrdersLen - 1 do begin
        FurPatId := Reader.OrderAt(ChanID, OrderRow);
        Key := (Int64(ChanID) shl 32) or FurPatId;
        if not FurPointerMap.TryGetData(Key, UgePatId) then begin
          UgePatId := NextUgeId;
          FurPointerMap.Add(Key, UgePatId);
          Inc(NextUgeId);
          // Seed a blank pattern in the map so channels w/ no data still exist
          Result.Patterns.GetOrCreateNew(UgePatId);
        end;
        Result.OrderMatrix[ChanID, OrderRow] := UgePatId;
      end;
      // Off-by-one slot at the end: point at the last valid pattern
      if OrdersLen > 0 then
        Result.OrderMatrix[ChanID, OrdersLen] := Result.OrderMatrix[ChanID, OrdersLen - 1];
    end;

    // Initial pan state — every channel unpanned (both sides on)
    for I := 0 to 3 do begin
      PanLeft[I] := True;
      PanRight[I] := True;
    end;

    // Fill pattern rows per channel / order, tracking continuing effects
    SetLength(PulseInsts, 0);
    SetLength(WaveInsts, 0);
    SetLength(NoiseInsts, 0);

    for ChanID := 0 to 3 do begin
      for OrderRow := 0 to OrdersLen - 1 do begin
        UgePatId := Result.OrderMatrix[ChanID, OrderRow];
        Pat := Result.Patterns.GetOrCreateNew(UgePatId);

        FurPatId := Reader.OrderAt(ChanID, OrderRow);
        FurPat := Reader.GetPattern(ChanID, FurPatId);
        if FurPat = nil then Continue;

        PatLen := FurPat.Length;
        PrevRow := 0;
        UgeFxCmd := -1;
        UgeFxVal := 0;

        for I := 0 to System.Length(FurPat.Rows) - 1 do begin
          SubCell := FurPat.Rows[I];
          RowIdx := SubCell.Row;

          // Fill "iterative" continuing effects on any skipped rows since prev
          if PrevRow > 0 then begin
            RowsSincePrev := RowIdx - PrevRow;
            for K := RowsSincePrev - 1 downto 1 do begin
              if (UgeFxVal = 0) or (UgeFxCmd < 0) then Break;
              if not CopyContinueEffect(UgeFxCmd) then Break;
              SetEffectOnPat(Pat, RowIdx - K, UgeFxCmd, UgeFxVal);
            end;
          end;
          PrevRow := RowIdx;

          // If this is the last-data row, extrapolate effects to end of pattern
          if I = System.Length(FurPat.Rows) - 1 then begin
            RowsToEnd := PatLen - RowIdx;
            for K := 1 to RowsToEnd - 1 do begin
              if (UgeFxVal = 0) or (UgeFxCmd < 0) then Break;
              if not CopyContinueEffect(UgeFxCmd) then Break;
              SetEffectOnPat(Pat, RowIdx + K, UgeFxCmd, UgeFxVal);
            end;
          end;

          // Note
          if SubCell.Note >= 0 then begin
            NoteCode := FurNoteToUgeNote(SubCell.Note);
            if NoteCode = 180 then begin
              // Note cut effect
              Pat^[RowIdx].EffectCode := HFX_NOTE_CUT;
              Pat^[RowIdx].EffectParams.Value := 0;
            end else begin
              if ChanID = 3 then NoteCode := NoteCode + 19;
              if (NoteCode >= 0) and (NoteCode < NO_NOTE) then
                Pat^[RowIdx].Note := NoteCode;
            end;
          end;

          // Instrument
          InstVal := SubCell.Instrument;
          TargetInst := -1;
          if (InstVal >= 0) and (InstVal < Reader.Instruments.Count) then begin
            FurInst := Reader.Instruments[InstVal];
            case ChanID of
              0, 1: begin
                DuplicateFound := False;
                for J := 0 to System.Length(PulseInsts) - 1 do
                  if PulseInsts[J] = FurInst.ID then begin
                    DuplicateFound := True;
                    RemapInst := J;
                    Break;
                  end;
                if not DuplicateFound then begin
                  RemapInst := System.Length(PulseInsts);
                  SetLength(PulseInsts, RemapInst + 1);
                  PulseInsts[RemapInst] := FurInst.ID;
                end;
                if RemapInst >= 15 then
                  raise EFurException.Create(
                    'Too many unique Pulse instruments (limit: 15).');
                Pat^[RowIdx].Instrument := RemapInst + 1;
                TargetInst := RemapInst;
              end;
              2: begin
                DuplicateFound := False;
                for J := 0 to System.Length(WaveInsts) - 1 do
                  if WaveInsts[J] = FurInst.ID then begin
                    DuplicateFound := True;
                    RemapInst := J;
                    Break;
                  end;
                if not DuplicateFound then begin
                  RemapInst := System.Length(WaveInsts);
                  SetLength(WaveInsts, RemapInst + 1);
                  WaveInsts[RemapInst] := FurInst.ID;
                end;
                if RemapInst >= 15 then
                  raise EFurException.Create(
                    'Too many unique Wave instruments (limit: 15).');
                Pat^[RowIdx].Instrument := RemapInst + 1;
                TargetInst := RemapInst;
              end;
              3: begin
                DuplicateFound := False;
                for J := 0 to System.Length(NoiseInsts) - 1 do
                  if NoiseInsts[J] = FurInst.ID then begin
                    DuplicateFound := True;
                    RemapInst := J;
                    Break;
                  end;
                if not DuplicateFound then begin
                  RemapInst := System.Length(NoiseInsts);
                  SetLength(NoiseInsts, RemapInst + 1);
                  NoiseInsts[RemapInst] := FurInst.ID;
                end;
                if RemapInst >= 15 then
                  raise EFurException.Create(
                    'Too many unique Noise instruments (limit: 15).');
                Pat^[RowIdx].Instrument := RemapInst + 1;
                TargetInst := RemapInst;
              end;
            end;
          end;

          // Volume column → SET_VOL effect (combined with envelope length nibble)
          if (SubCell.Volume >= 0) and (TargetInst >= 0) then begin
            FurInst := Reader.Instruments[InstVal];
            GbVol := FurInst.GB.EnvVol;
            GbLen := FurInst.GB.EnvLen;
            GbDir := FurInst.GB.EnvDir;
            if ChanID = 2 then GbLen := 0;
            OutVol := Byte(SubCell.Volume) + Byte($F0 and (GbLen shl 4));
            Pat^[RowIdx].EffectCode := HFX_SET_VOL;
            Pat^[RowIdx].EffectParams.Value := OutVol;
          end;

          // Effect column (only column 0 is mapped; others pass through untouched)
          for J := 0 to 7 do begin
            FurFxCmdPresent := SubCell.FxPresent[J];
            FurFxValPresent := SubCell.FxValPresent[J];
            if not (FurFxCmdPresent or FurFxValPresent) then Continue;

            FurFxCmd := SubCell.Fx[J];
            FurFxVal := SubCell.FxVal[J];

            UgeFxCmd := -1;
            UgeFxVal := FurFxVal;
            UseEffect := True;

            case FurFxCmd of
              FX_ARPEGGIO:      UgeFxCmd := HFX_ARPEGGIO;
              FX_PORTA_UP:      UgeFxCmd := HFX_PORTA_UP;
              FX_PORTA_DOWN:    UgeFxCmd := HFX_PORTA_DOWN;
              FX_TONE_PORTA:    UgeFxCmd := HFX_TONE_PORTA;
              FX_VIBRATO:       UgeFxCmd := HFX_VIBRATO;
              FX_VOLUME_SLIDE:  UgeFxCmd := HFX_VOLUME_SLIDE;
              FX_SET_DUTY:      UgeFxCmd := HFX_SET_DUTY;
              FX_POSITION_JUMP: begin UgeFxCmd := HFX_POSITION_JUMP; Inc(UgeFxVal); end;
              FX_PATTERN_BREAK: begin UgeFxCmd := HFX_PATTERN_BREAK; Inc(UgeFxVal); end;
              FX_SET_SPEED:     UgeFxCmd := HFX_SET_SPEED;
              FX_NOTE_CUT:      UgeFxCmd := HFX_NOTE_CUT;
              FX_NOTE_DELAY:    UgeFxCmd := HFX_NOTE_DELAY;
              FX_SOFTWARE_PAN: begin
                // 0x00 = left, 0x80 = both, 0xFF = right
                PanRight[ChanID] := (FurFxVal = $80) or (FurFxVal = $FF);
                PanLeft[ChanID]  := (FurFxVal = $80) or (FurFxVal = $00);
                UgeFxCmd := HFX_SET_PANNING;
                FinalPan := 0;
                for K := 0 to 3 do begin
                  if PanLeft[K]  then FinalPan := FinalPan or (1 shl K);
                  if PanRight[K] then FinalPan := FinalPan or (1 shl (K + 4));
                end;
                UgeFxVal := Byte(FinalPan);
              end;
              FX_HW_PAN:
                UseEffect := False; // Hardware pan conversion not supported
              $F1: begin
                UgeFxCmd := HFX_SET_SPEED; // noop marker from fur
                UseEffect := False;
              end;
            else
              UseEffect := False;
            end;

            if UseEffect and (UgeFxCmd >= 0) then begin
              Pat^[RowIdx].EffectCode := UgeFxCmd;
              Pat^[RowIdx].EffectParams.Value := UgeFxVal;
            end;

            Break; // fur2uge only emits the first present effect column
          end;
        end;
      end;
    end;

    // Instruments — create hUGETracker instruments from the deduped fur lists
    for I := 0 to System.Length(PulseInsts) - 1 do begin
      FurInst := Reader.Instruments[PulseInsts[I]];
      HInst := @Result.Instruments.Duty[I + 1];
      HInst^.Type_ := itSquare;
      HInst^.Name := FurInst.Name;
      HInst^.InitialVolume := FurInst.GB.EnvVol and $F;
      if FurInst.GB.EnvDir = 1 then
        HInst^.VolSweepDirection := stUp
      else
        HInst^.VolSweepDirection := stDown;
      HInst^.VolSweepAmount := FurInst.GB.EnvLen and $7;
      HInst^.Length := FurInst.GB.SndLen;
      HInst^.LengthEnabled := HInst^.Length < $3F;
      if not HInst^.LengthEnabled then HInst^.Length := $3F;
      if (System.Length(FurInst.GB.HWSeq) > 0)
        and (FurInst.GB.HWSeq[0].CmdType = 1) then begin
        HInst^.SweepTime := FurInst.GB.HWSeq[0].SweepSpeed;
        if FurInst.GB.HWSeq[0].SweepDir = 1 then
          HInst^.SweepIncDec := stUp
        else
          HInst^.SweepIncDec := stDown;
        HInst^.SweepShift := FurInst.GB.HWSeq[0].ShiftVal;
      end;
      // Duty cycle from first DUTY macro value, if any
      HInst^.Duty := 2;
      for J := 0 to System.Length(FurInst.Macros) - 1 do
        if FurInst.Macros[J].Code = MC_DUTY then begin
          if System.Length(FurInst.Macros[J].Data) > 0 then
            HInst^.Duty := FurInst.Macros[J].Data[0] and $3;
          Break;
        end;
      // Overwrite initial volume if a VOL macro is present
      for J := 0 to System.Length(FurInst.Macros) - 1 do
        if FurInst.Macros[J].Code = MC_VOL then begin
          if System.Length(FurInst.Macros[J].Data) > 0 then
            HInst^.InitialVolume := FurInst.Macros[J].Data[0] and $F;
          Break;
        end;
      ApplyMacrosToSubpattern(HInst^.Subpattern, FurInst, 1);
      HInst^.SubpatternEnabled := False;
      for J := 0 to System.Length(FurInst.Macros) - 1 do
        if System.Length(FurInst.Macros[J].Data) > 1 then begin
          HInst^.SubpatternEnabled := True;
          Break;
        end;
    end;

    for I := 0 to System.Length(WaveInsts) - 1 do begin
      FurInst := Reader.Instruments[WaveInsts[I]];
      HInst := @Result.Instruments.Wave[I + 1];
      HInst^.Type_ := itWave;
      HInst^.Name := FurInst.Name;
      HInst^.OutputLevel := 1;
      HInst^.Length := FurInst.GB.SndLen;
      HInst^.LengthEnabled := HInst^.Length < $3F;
      if not HInst^.LengthEnabled then HInst^.Length := $3F;
      // Wave index: WS's first wave, or first WAVE macro value
      HInst^.Waveform := 0;
      if FurInst.WS.Present and FurInst.WS.Enabled then
        HInst^.Waveform := FurInst.WS.FirstWave;
      for J := 0 to System.Length(FurInst.Macros) - 1 do
        if FurInst.Macros[J].Code = MC_WAVE then begin
          if System.Length(FurInst.Macros[J].Data) > 0 then
            HInst^.Waveform := FurInst.Macros[J].Data[0];
          Break;
        end;
      for J := 0 to System.Length(FurInst.Macros) - 1 do
        if FurInst.Macros[J].Code = MC_VOL then begin
          if System.Length(FurInst.Macros[J].Data) > 0 then
            HInst^.OutputLevel := FurInst.Macros[J].Data[0] and $3;
          Break;
        end;
      ApplyMacrosToSubpattern(HInst^.Subpattern, FurInst, 1);
      HInst^.SubpatternEnabled := False;
      for J := 0 to System.Length(FurInst.Macros) - 1 do
        if System.Length(FurInst.Macros[J].Data) > 1 then begin
          HInst^.SubpatternEnabled := True;
          Break;
        end;
    end;

    for I := 0 to System.Length(NoiseInsts) - 1 do begin
      FurInst := Reader.Instruments[NoiseInsts[I]];
      HInst := @Result.Instruments.Noise[I + 1];
      HInst^.Type_ := itNoise;
      HInst^.Name := FurInst.Name;
      HInst^.InitialVolume := FurInst.GB.EnvVol and $F;
      // Fur2uge quirk: noise always emits stDown
      HInst^.VolSweepDirection := stDown;
      HInst^.VolSweepAmount := FurInst.GB.EnvLen and $7;
      HInst^.Length := FurInst.GB.SndLen;
      HInst^.LengthEnabled := HInst^.Length < $3F;
      if not HInst^.LengthEnabled then HInst^.Length := $3F;
      HInst^.CounterStep := swFifteen;
      ApplyMacrosToSubpattern(HInst^.Subpattern, FurInst, 1);
      HInst^.SubpatternEnabled := False;
      for J := 0 to System.Length(FurInst.Macros) - 1 do
        if System.Length(FurInst.Macros[J].Data) > 1 then begin
          HInst^.SubpatternEnabled := True;
          Break;
        end;
    end;
  finally
    FurPointerMap.Free;
    Reader.Free;
  end;
end;

end.
