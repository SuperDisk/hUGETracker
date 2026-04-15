unit portmidi;

{
  Minimal Pascal bindings for PortMidi (https://github.com/PortMidi/portmidi).

  Only the input-side of the API is translated - that's all hUGETracker needs.
  PortMidi is loaded dynamically via the dynlibs unit so the application still
  runs on machines where the shared library isn't installed; callers should
  check LoadPortMidi before touching any of the Pm_* routines.

  Calling convention is cdecl on every supported platform (PortMidi's public
  header uses PMEXPORT which is either empty or __declspec; there is no
  __stdcall). PortTime is not bound here; passing nil for the time_proc /
  time_info parameters of Pm_OpenInput makes PortMidi use its own internal
  millisecond clock, which is all we need for note input.
}

{$mode objfpc}{$H+}

interface

uses
  ctypes, dynlibs, SysUtils;

const
  {$if defined(MSWINDOWS)}
  PortMidiLibName = 'portmidi.dll';
  {$elseif defined(DARWIN)}
  PortMidiLibName = 'libportmidi.dylib';
  {$else}
  PortMidiLibName = 'libportmidi.so.2';
  {$endif}

  pmNoError               = 0;
  pmNoData                = 0;
  pmGotData               = 1;
  pmHostError             = -10000;
  pmInvalidDeviceId       = -9999;
  pmInsufficientMemory    = -9998;
  pmBufferTooSmall        = -9997;
  pmBufferOverflow        = -9996;
  pmBadPtr                = -9995;
  pmBadData               = -9994;
  pmInternalError         = -9993;
  pmBufferMaxSize         = -9992;
  pmNotImplemented        = -9991;
  pmInterfaceNotSupported = -9990;
  pmNameConflict          = -9989;
  pmDeviceRemoved         = -9988;

  pmNoDevice = -1;

type
  PmError = cint32;
  PmDeviceID = cint32;
  PmTimestamp = cint32;
  PmMessage = cuint32;
  PPortMidiStream = Pointer;
  PPPortMidiStream = ^PPortMidiStream;
  PmTimeProcPtr = function(time_info: Pointer): PmTimestamp; cdecl;

  TPmDeviceInfo = record
    structVersion: cint;
    interf: PAnsiChar;
    name: PAnsiChar;
    input: cint;
    output: cint;
    opened: cint;
    is_virtual: cint;
  end;
  PPmDeviceInfo = ^TPmDeviceInfo;

  TPmEvent = record
    message_: PmMessage;
    timestamp: PmTimestamp;
  end;
  PPmEvent = ^TPmEvent;

var
  Pm_Initialize: function: PmError; cdecl;
  Pm_Terminate: function: PmError; cdecl;
  Pm_HasHostError: function(stream: PPortMidiStream): cint; cdecl;
  Pm_GetErrorText: function(errnum: PmError): PAnsiChar; cdecl;
  Pm_GetHostErrorText: procedure(msg: PAnsiChar; len: cuint); cdecl;
  Pm_CountDevices: function: cint; cdecl;
  Pm_GetDefaultInputDeviceID: function: PmDeviceID; cdecl;
  Pm_GetDefaultOutputDeviceID: function: PmDeviceID; cdecl;
  Pm_GetDeviceInfo: function(id: PmDeviceID): PPmDeviceInfo; cdecl;
  Pm_OpenInput: function(out stream: PPortMidiStream;
                         inputDevice: PmDeviceID;
                         inputSysDepInfo: Pointer;
                         bufferSize: cint32;
                         time_proc: PmTimeProcPtr;
                         time_info: Pointer): PmError; cdecl;
  Pm_SetFilter: function(stream: PPortMidiStream; filters: cint32): PmError; cdecl;
  Pm_SetChannelMask: function(stream: PPortMidiStream; mask: cint): PmError; cdecl;
  Pm_Abort: function(stream: PPortMidiStream): PmError; cdecl;
  Pm_Close: function(stream: PPortMidiStream): PmError; cdecl;
  Pm_Read: function(stream: PPortMidiStream; buffer: PPmEvent; length: cint32): cint; cdecl;
  Pm_Poll: function(stream: PPortMidiStream): PmError; cdecl;

function LoadPortMidi: Boolean;
procedure UnloadPortMidi;
function IsPortMidiLoaded: Boolean;

function Pm_MessageStatus(msg: PmMessage): Byte; inline;
function Pm_MessageData1(msg: PmMessage): Byte; inline;
function Pm_MessageData2(msg: PmMessage): Byte; inline;

implementation

var
  LibHandle: TLibHandle = NilHandle;

function IsPortMidiLoaded: Boolean;
begin
  Result := LibHandle <> NilHandle;
end;

function Pm_MessageStatus(msg: PmMessage): Byte;
begin
  Result := Byte(msg and $FF);
end;

function Pm_MessageData1(msg: PmMessage): Byte;
begin
  Result := Byte((msg shr 8) and $FF);
end;

function Pm_MessageData2(msg: PmMessage): Byte;
begin
  Result := Byte((msg shr 16) and $FF);
end;

function ResolveSym(const Name: String): Pointer;
begin
  Result := GetProcedureAddress(LibHandle, Name);
  if Result = nil then begin
    UnloadLibrary(LibHandle);
    LibHandle := NilHandle;
  end;
end;

function TryLoad(const Name: String): TLibHandle;
begin
  Result := LoadLibrary(Name);
end;

function LoadPortMidi: Boolean;
var
  ExeDir: String;
begin
  if LibHandle <> NilHandle then
    Exit(True);

  LibHandle := TryLoad(PortMidiLibName);

  // On non-Windows, dlopen() by filename only does not search the
  // executable's directory, so explicitly look next to the running binary
  // before falling back to system paths.
  ExeDir := ExtractFilePath(ParamStr(0));
  if (LibHandle = NilHandle) and (ExeDir <> '') then
    LibHandle := TryLoad(ExeDir + PortMidiLibName);

  {$ifdef LINUX}
  if LibHandle = NilHandle then LibHandle := TryLoad('libportmidi.so');
  if LibHandle = NilHandle then LibHandle := TryLoad('libportmidi.so.0');
  if LibHandle = NilHandle then LibHandle := TryLoad('libportmidi.so.1');
  if (LibHandle = NilHandle) and (ExeDir <> '') then
    LibHandle := TryLoad(ExeDir + 'libportmidi.so');
  {$endif}
  {$ifdef DARWIN}
  if LibHandle = NilHandle then LibHandle := TryLoad('libportmidi.2.dylib');
  if (LibHandle = NilHandle) and (ExeDir <> '') then
    LibHandle := TryLoad(ExeDir + 'libportmidi.2.dylib');
  if LibHandle = NilHandle then LibHandle := TryLoad('/usr/local/lib/libportmidi.dylib');
  if LibHandle = NilHandle then LibHandle := TryLoad('/opt/homebrew/lib/libportmidi.dylib');
  {$endif}

  if LibHandle = NilHandle then Exit(False);

  Pointer(Pm_Initialize)               := ResolveSym('Pm_Initialize');
  Pointer(Pm_Terminate)                := ResolveSym('Pm_Terminate');
  Pointer(Pm_HasHostError)             := ResolveSym('Pm_HasHostError');
  Pointer(Pm_GetErrorText)             := ResolveSym('Pm_GetErrorText');
  Pointer(Pm_GetHostErrorText)         := ResolveSym('Pm_GetHostErrorText');
  Pointer(Pm_CountDevices)             := ResolveSym('Pm_CountDevices');
  Pointer(Pm_GetDefaultInputDeviceID)  := ResolveSym('Pm_GetDefaultInputDeviceID');
  Pointer(Pm_GetDefaultOutputDeviceID) := ResolveSym('Pm_GetDefaultOutputDeviceID');
  Pointer(Pm_GetDeviceInfo)            := ResolveSym('Pm_GetDeviceInfo');
  Pointer(Pm_OpenInput)                := ResolveSym('Pm_OpenInput');
  Pointer(Pm_SetFilter)                := ResolveSym('Pm_SetFilter');
  Pointer(Pm_SetChannelMask)           := ResolveSym('Pm_SetChannelMask');
  Pointer(Pm_Abort)                    := ResolveSym('Pm_Abort');
  Pointer(Pm_Close)                    := ResolveSym('Pm_Close');
  Pointer(Pm_Read)                     := ResolveSym('Pm_Read');
  Pointer(Pm_Poll)                     := ResolveSym('Pm_Poll');

  Result := LibHandle <> NilHandle;
end;

procedure UnloadPortMidi;
begin
  if LibHandle <> NilHandle then begin
    UnloadLibrary(LibHandle);
    LibHandle := NilHandle;
  end;
end;

finalization
  UnloadPortMidi;

end.
