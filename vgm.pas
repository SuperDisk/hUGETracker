unit VGM;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, gvector, vars, mainloop, sound, constants;

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
    Reserved3: UInt64; // 8 bytes
  end;

var
  RecordingVGM: Boolean = False;

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
  SeenOrders: array of Integer;
  OrderChecker: TOrderChecker;
  StartOfSong, CurrentOrder: Integer;
  VGMFile: TFileStream;
  TotalWaitSamps: Integer;

procedure ExportVGMFile(F: String; TrackName: String; ArtistName: String; Comments: String);
var
  OldFD, OldFC, OldF4: TCPUCallback;
  Header: TVGMHeader;
  Utf16Bytes: TBytes;
  YY,MM,DD: Word;
  GD3Temp: Integer;
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
  load('render/preview.gb');

  SetLength(SeenOrders, PeekSymbol(SYM_ORDER_COUNT) div 2);
  StartOfSong := -1;
  CurrentOrder := -1;

  WritingVGM := True;
  VGMFile := TFileStream.Create(F, fmCreate);

  Header := Default(TVGMHeader); // Zero the header
  VGMFile.WriteBuffer(Header, SizeOf(Header)); // Write dummy header (will overwrite later)

  repeat
    z80_decode
  until StartOfSong <> -1;

  WritingVGM := False;

  // Update the header to point to the GD3 tag, then write it
  Header.GD3offset := VGMFile.Position - $14;

  VGMFile.WriteDWord($20336447);
  VGMFile.WriteDWord($00000100);

  GD3Temp := VGMFile.Position;
  VGMFile.WriteDWord(0); // dummy value...
  Utf16Bytes := WideBytesOf(UTF8Decode(TrackName));
  VGMFile.WriteBuffer(Utf16Bytes[0], Length(Utf16Bytes)); // English name
  VGMFile.WriteWord(0);

  VGMFile.WriteWord(0); // No japanese track name
  VGMFile.WriteWord(0); // No game name
  VGMFile.WriteWord(0); // No japanese game name
  Utf16Bytes := WideBytesOf('Nintendo Game Boy');
  VGMFile.WriteBuffer(Utf16Bytes[0], Length(Utf16Bytes));
  VGMFile.WriteWord(0);
  VGMFile.WriteWord(0); // No japanese system name
  Utf16Bytes := WideBytesOf(UTF8Decode(ArtistName));
  VGMFile.WriteBuffer(Utf16Bytes[0], Length(Utf16Bytes));
  VGMFile.WriteWord(0);
  VGMFile.WriteWord(0); // No japanese author name
  DecodeDate(Date, YY, MM, DD);
  Utf16Bytes := WideBytesOf(UTF8Decode(Format('%d/%.2d/%.2d', [YY, MM, DD])));
  VGMFile.WriteBuffer(Utf16Bytes[0], Length(Utf16Bytes));
  VGMFile.WriteWord(0);
  Utf16Bytes := WideBytesOf('hUGETracker');
  VGMFile.WriteBuffer(Utf16Bytes[0], Length(Utf16Bytes));
  VGMFile.WriteWord(0);
  Utf16Bytes := WideBytesOf(UTF8Decode(Comments));
  VGMFile.WriteBuffer(Utf16Bytes[0], Length(Utf16Bytes));
  VGMFile.WriteWord(0);

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

  VGMFile.Seek(0, soBeginning);
  VGMFile.WriteBuffer(Header, SizeOf(Header));

  VGMFile.Free;

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
  VGMFile.WriteByte($62); // TODO: Use the smaller commands if it matches
  //VGMFile.WriteWord(Word(Amount));
  Inc(TotalWaitSamps, 735);
end;

{ TOrderChecker }

procedure TOrderChecker.OrderCheckCallback;
var
  Ord: Integer;
begin
  Ord := (PeekSymbol(SYM_CURRENT_ORDER) div 2);

  if (SeenOrders[Ord] <> 0) and (StartOfSong = -1) then
      StartOfSong := Ord;

  Inc(SeenOrders[Ord]);
  CurrentOrder := Ord;
end;

procedure TOrderChecker.TickCallback;
begin
  VGMWait(0);
end;

begin
  OrderChecker := TOrderChecker.Create;
end.

