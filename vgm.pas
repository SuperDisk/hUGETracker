unit VGM;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, gvector;

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
  end;

  TVGMCommandType = (ctWait, ctRegWrite);

  TVGMCommand = record
    type_: TVGMCommandType;
    Reg: Integer;
    Param: Integer;
  end;

  TCommandVector = specialize TVector<TVGMCommand>;

var
  RecordingVGM: Boolean = False;

procedure BeginRecordingVGM(F: String);
procedure EndRecordingVGM;

procedure VGMWriteReg(Reg: Integer; Value: Integer);
procedure VGMWait(Amount: Integer);

implementation

var
  VGMFile: TFileStream;
  CommandBuffer: TCommandVector;

procedure BeginRecordingVGM(F: String);
begin
  // Due to the annoying format of VGM where the header needs values that can
  // only be computed after rendering the entire file, we store the data in
  // memory until recording is finished, and dump it all out then.
  // Thankfully VGM files are small enough to fit in RAM.

  VGMFile := TFileStream.Create(F, fmOpenWrite);
  CommandBuffer.Clear;
end;

procedure EndRecordingVGM;
var
  Header: TVGMHeader;
begin
  Header := Default(TVGMHeader); // Zero the header
  Header.Vgmident := $56676d20; // "Vgm "
  Header.Version := $00000161; // 1.61
  Header.Rate := 60; // 60hz
  Header.GBDMGclock := 4194304; // 4194304 hz (from docs)

  VGMFile.WriteBuffer(Header, SizeOf(Header));
end;

procedure VGMWriteReg(Reg: Integer; Value: Integer);
var
  Cmd: TVGMCommand;
begin
  Cmd.type_ := ctRegWrite;
  Cmd.Reg := Reg - $FF10;
  Cmd.Param := Value;

  CommandBuffer.PushBack(Cmd);
end;

procedure VGMWait(Amount: Integer);
var
  Cmd: TVGMCommand;
begin
  Cmd.type_ := ctWait;
  Cmd.Param := Amount;

  CommandBuffer.PushBack(Cmd);
end;

begin
  CommandBuffer := TCommandVector.Create;
end.

