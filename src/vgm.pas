unit VGM;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, vars, mainloop, sound, constants;

type
  // Header for a v1.61 VGM file.
  // https://github.com/vgmrips/vgmplay/blob/4ac9870f13fd67155569c41da2269c70cd3fcb31/vgmspec161.txt
  TVGMHeader = packed record
    Vgmident: UInt32;
    EoFoffset: UInt32;
    Version: UInt32;
    SN76489clock: UInt32;
    YM2413clock: UInt32;
    GD3offset: UInt32;
    TotalNumsamples: UInt32;
    Loopoffset: UInt32;
    LoopNumsamples: UInt32;
    Rate: UInt32;
    SNFB: UInt16;
    SNW: UInt8;
    SF: UInt8;
    YM2612clock: UInt32;
    YM2151clock: UInt32;
    VGMdataoffset: UInt32;
    SegaPCMclock: UInt32;
    SPCMInterface: UInt32;
    RF5C68clock: UInt32;
    YM2203clock: UInt32;
    YM2608clock: UInt32;
    YM2610Bclock: UInt32;
    YM3812clock: UInt32;
    YM3526clock: UInt32;
    Y8950clock: UInt32;
    YMF262clock: UInt32;
    YMF278Bclock: UInt32;
    YMF271clock: UInt32;
    YMZ280Bclock: UInt32;
    RF5C164clock: UInt32;
    PWMclock: UInt32;
    AY8910clock: UInt32;
    AYT: UInt8;
    AYFlags: UInt8;
    AYFlags1: UInt8;
    AYFlags2: UInt8;
    VM: UInt8;
    Reserved1: UInt8;
    LB: UInt8;
    LM: UInt8;
    GBDMGclock: UInt32;
    NESAPUclock: UInt32;
    MultiPCMclock: UInt32;
    uPD7759clock: UInt32;
    OKIM6258clock: UInt32;
    _OF: UInt8;
    KF: UInt8;
    CF: UInt8;
    Reserved2: UInt8;
    OKIM6295clock: UInt32;
    K051649clock: UInt32;
    K054539clock: UInt32;
    HuC6280clock: UInt32;
    C140clock: UInt32;
    K053260clock: UInt32;
    Pokeyclock: UInt32;
    QSoundclock: UInt32;
    Reserved3: UInt64;
  end;

procedure ExportVGMFile(F: String; TrackName: String; ArtistName: String; Comments: String);

procedure VGMWriteReg(Reg: Integer; Value: Integer);
procedure VGMWait(Amount: Integer);

implementation

uses symparser;

type
  // Need to wrap the FC callback in a class because it's declared to be
  // "procedure of object" :(

  { TOrderChecker }

  TOrderChecker = class
    procedure OrderCheckCallback;
    procedure TickCallback;
  end;

var
  SeenOrders: array of record
    TimesSeen: Integer;
    SampsToReach: Integer;
    DataOffset: Integer;
  end;
  OrderChecker: TOrderChecker;
  StartOfSong: Integer;
  VGMFile: TFileStream;
  TotalWaitSamps: Integer;
  CyclesSinceLastTick: Integer;

procedure WriteUtf16String(S: String);
var
  Utf16Bytes: TBytes;
begin
  if Length(S) > 0 then begin
    Utf16Bytes := WideBytesOf(UTF8Decode(S));
    VGMFile.WriteBuffer(Utf16Bytes[0], Length(Utf16Bytes));
  end;
  VGMFile.WriteWord(0);
end;

procedure ExportVGMFile(F: String; TrackName: String; ArtistName: String; Comments: String);
var
  OldFD, OldFC, OldF4: TCPUCallback;
  Header: TVGMHeader;

  YY,MM,DD: Word;
  GD3Temp: Integer;
  StepCycles: Integer;
begin
  // TODO: Manage these callbacks in a better way, maybe some sort of stack.
  OldFD := FDCallback;
  OldFC := FCCallback;
  OldF4 := F4Callback;
  FDCallback := nil;
  F4Callback := @OrderChecker.TickCallback;
  FCCallback := @OrderChecker.OrderCheckCallback;

  z80_reset;
  ResetSound;
  enablesound;
  load(ConcatPaths([CacheDir, 'render/preview.gb']));

  SetLength(SeenOrders, PeekSymbol(SYM_ORDER_COUNT) div 2);
  StartOfSong := -1;

  WritingVGM := True;
  VGMFile := TFileStream.Create(F, fmCreate);

  Header := Default(TVGMHeader); // Zero the header
  VGMFile.WriteBuffer(Header, SizeOf(Header)); // Write dummy header (will overwrite later)

  CyclesSinceLastTick := 0;
  repeat
    StepCycles := z80_decode;
    Inc(CyclesSinceLastTick, StepCycles);
  until StartOfSong <> -1;

  VGMFile.WriteByte($66); // End of data

  WritingVGM := False;

  // Update the header to point to the GD3 tag, then write it
  Header.GD3offset := VGMFile.Position - $14;

  VGMFile.WriteDWord($20336447); // 'Gd3 '
  VGMFile.WriteDWord($00000100); // Version

  GD3Temp := VGMFile.Position;
  VGMFile.WriteDWord(0); // dummy value...

  DecodeDate(Date, YY, MM, DD);

  WriteUtf16String(TrackName); // English track name
  WriteUtf16String(''); // Japanese track name
  WriteUtf16String(''); // English game name
  WriteUtf16String(''); // Japanese game name
  WriteUtf16String('Nintendo Game Boy'); // English system name
  WriteUtf16String(''); // Japanese system name
  WriteUtf16String(ArtistName); // English artist name
  WriteUtf16String(''); // Japanese artist name
  WriteUtf16String(Format('%d/%.2d/%.2d', [YY, MM, DD])); // Creation date
  WriteUtf16String('hUGETracker'); // Program name
  WriteUtf16String(''); // Japanese program name
  WriteUtf16String(Comments); // Comments

  // Go back and overwrite that dummy (unnecessary) length value.
  // The VGM and GD3 formats sure are ugly. Not that UGE is any better.
  GD3Temp := VGMFile.Position - GD3Temp;
  VGMFile.Seek(-GD3Temp, soCurrent);
  VGMFile.WriteDWord(GD3Temp);

  // Rewrite the header now that we know the correct values
  Header.Vgmident := SwapEndian($56676d20); // "Vgm "
  Header.Version := $00000161; // 1.61
  Header.Rate := 60; // 60hz
  Header.GBDMGclock := 4194304; // 4194304 hz (from docs)
  Header.EoFoffset := VGMFile.Size - $4;
  Header.VGMdataoffset := $8C;
  Header.TotalNumsamples := TotalWaitSamps;

  Header.LoopNumsamples := TotalWaitSamps - SeenOrders[StartOfSong].SampsToReach;
  Header.Loopoffset := SeenOrders[StartOfSong].DataOffset - $1C;

  VGMFile.Seek(0, soBeginning);
  VGMFile.WriteBuffer(Header, SizeOf(Header));

  VGMFile.Free;

  SetLength(SeenOrders, 0);

  F4Callback := OldF4;
  FDCallback := OldFD;
  FCCallback := OldFC;
end;

procedure VGMWriteReg(Reg: Integer; Value: Integer);
begin
  VGMFile.WriteByte($B3); // Write DMG Reg
  VGMFile.WriteByte(Byte(Reg - $FF10));
  VGMFile.WriteByte(Byte(Value));
end;

procedure VGMWait(Amount: Integer);
begin
  VGMFile.WriteByte($61); // TODO: Use the smaller commands if it matches
  VGMFile.WriteWord(Word(Amount));
  Inc(TotalWaitSamps, Amount);
end;

{ TOrderChecker }

procedure TOrderChecker.OrderCheckCallback;
var
  Ord: Integer;
begin
  Ord := (PeekSymbol(SYM_CURRENT_ORDER) div 2);

  if (SeenOrders[Ord].TimesSeen = 0) then begin
    SeenOrders[Ord].SampsToReach := TotalWaitSamps;
    SeenOrders[Ord].DataOffset := VGMFile.Position;
  end;

  if (SeenOrders[Ord].TimesSeen <> 0) and (StartOfSong = -1) then
      StartOfSong := Ord;

  Inc(SeenOrders[Ord].TimesSeen);
end;

procedure TOrderChecker.TickCallback;
begin
  VGMWait(Trunc((CyclesSinceLastTick / (4194304))*44100.0));
  CyclesSinceLastTick := 0;
end;

begin
  OrderChecker := TOrderChecker.Create;
end.

