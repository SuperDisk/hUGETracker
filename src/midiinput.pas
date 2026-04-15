unit MidiInput;

{
  Cross-platform MIDI input for hUGETracker using PortMidi.

  This unit owns a single global TMidiInput instance, hosts a TTimer that
  polls the currently open MIDI input stream on the main thread (so callers
  don't have to worry about synchronizing with the LCL), and raises
  OnNoteOn / OnNoteOff events with hUGETracker-scaled note numbers.

  Notes are translated from MIDI's 0..127 range to hUGETracker's 0..71
  (C_3..B_8). MIDI C3 (48) maps to hUGETracker C_3 (0); anything outside
  the tracker's range is silently dropped so we never feed an out-of-bounds
  value into the preview code.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, portmidi;

const
  HT_MIDI_NOTE_OFFSET = 48; // MIDI C3 -> hUGETracker C_3 (note index 0)

type
  TMidiNoteEvent = procedure(Sender: TObject; Note, Velocity: Integer) of object;

  TMidiInput = class
  private
    FStream: PPortMidiStream;
    FTimer: TTimer;
    FDeviceName: String;
    FOnNoteOn: TMidiNoteEvent;
    FOnNoteOff: TMidiNoteEvent;
    FInitialized: Boolean;
    FAvailable: Boolean;
    procedure HandleTimer(Sender: TObject);
    procedure DispatchMessage(msg: PmMessage);
  public
    constructor Create;
    destructor Destroy; override;

    function Available: Boolean;
    procedure EnumerateInputDevices(Names: TStrings);
    function FindInputDeviceID(const Name: String): PmDeviceID;
    function OpenDevice(const Name: String): Boolean;
    procedure CloseDevice;
    function IsOpen: Boolean;
    property DeviceName: String read FDeviceName;

    property OnNoteOn: TMidiNoteEvent read FOnNoteOn write FOnNoteOn;
    property OnNoteOff: TMidiNoteEvent read FOnNoteOff write FOnNoteOff;
  end;

function Midi: TMidiInput;

// Translate raw MIDI (0..127) to hUGETracker note index. Returns -1 if
// the note is outside the tracker's range.
function MidiNoteToTrackerNote(MidiNote: Integer): Integer;

implementation

var
  GMidi: TMidiInput = nil;

function Midi: TMidiInput;
begin
  if GMidi = nil then
    GMidi := TMidiInput.Create;
  Result := GMidi;
end;

function MidiNoteToTrackerNote(MidiNote: Integer): Integer;
begin
  Result := MidiNote - HT_MIDI_NOTE_OFFSET;
  if (Result < 0) or (Result > 71) then
    Result := -1;
end;

{ TMidiInput }

constructor TMidiInput.Create;
begin
  inherited Create;
  FStream := nil;
  FAvailable := LoadPortMidi;
  if FAvailable then begin
    if Pm_Initialize() = pmNoError then
      FInitialized := True
    else
      FAvailable := False;
  end;

  FTimer := TTimer.Create(nil);
  FTimer.Interval := 5;
  FTimer.Enabled := False;
  FTimer.OnTimer := @HandleTimer;
end;

destructor TMidiInput.Destroy;
begin
  CloseDevice;
  FTimer.Free;
  if FInitialized then
    Pm_Terminate();
  inherited Destroy;
end;

function TMidiInput.Available: Boolean;
begin
  Result := FAvailable;
end;

procedure TMidiInput.EnumerateInputDevices(Names: TStrings);
var
  I, N: Integer;
  Info: PPmDeviceInfo;
begin
  Names.Clear;
  if not FAvailable then Exit;

  N := Pm_CountDevices();
  for I := 0 to N - 1 do begin
    Info := Pm_GetDeviceInfo(I);
    if (Info <> nil) and (Info^.input <> 0) then
      Names.Add(String(Info^.name));
  end;
end;

function TMidiInput.FindInputDeviceID(const Name: String): PmDeviceID;
var
  I, N: Integer;
  Info: PPmDeviceInfo;
begin
  Result := pmNoDevice;
  if (not FAvailable) or (Name = '') then Exit;

  N := Pm_CountDevices();
  for I := 0 to N - 1 do begin
    Info := Pm_GetDeviceInfo(I);
    if (Info <> nil) and (Info^.input <> 0) and (String(Info^.name) = Name) then
      Exit(I);
  end;
end;

function TMidiInput.OpenDevice(const Name: String): Boolean;
var
  DevID: PmDeviceID;
begin
  Result := False;
  if not FAvailable then Exit;

  CloseDevice;

  DevID := FindInputDeviceID(Name);
  if DevID = pmNoDevice then Exit;

  if Pm_OpenInput(FStream, DevID, nil, 256, nil, nil) <> pmNoError then begin
    FStream := nil;
    Exit;
  end;

  FDeviceName := Name;
  FTimer.Enabled := True;
  Result := True;
end;

procedure TMidiInput.CloseDevice;
begin
  FTimer.Enabled := False;
  if FStream <> nil then begin
    Pm_Close(FStream);
    FStream := nil;
  end;
  FDeviceName := '';
end;

function TMidiInput.IsOpen: Boolean;
begin
  Result := FStream <> nil;
end;

procedure TMidiInput.DispatchMessage(msg: PmMessage);
var
  Status, Data1, Data2: Byte;
  Kind, TrackerNote: Integer;
begin
  Status := Pm_MessageStatus(msg);
  Data1 := Pm_MessageData1(msg);
  Data2 := Pm_MessageData2(msg);

  Kind := Status and $F0;
  TrackerNote := MidiNoteToTrackerNote(Data1);
  if TrackerNote < 0 then Exit;

  case Kind of
    $90: // Note On
      begin
        // MIDI convention: Note On with velocity 0 is a Note Off.
        if Data2 = 0 then begin
          if Assigned(FOnNoteOff) then FOnNoteOff(Self, TrackerNote, 0);
        end
        else begin
          if Assigned(FOnNoteOn) then FOnNoteOn(Self, TrackerNote, Data2);
        end;
      end;
    $80: // Note Off
      if Assigned(FOnNoteOff) then FOnNoteOff(Self, TrackerNote, Data2);
  end;
end;

procedure TMidiInput.HandleTimer(Sender: TObject);
const
  BUF_SIZE = 32;
var
  Buf: array[0..BUF_SIZE-1] of TPmEvent;
  Count, I: Integer;
  PollResult: PmError;
begin
  if FStream = nil then Exit;

  PollResult := Pm_Poll(FStream);
  if PollResult < 0 then begin
    // Device likely removed - close cleanly so we stop hammering it.
    CloseDevice;
    Exit;
  end;

  while Pm_Poll(FStream) = pmGotData do begin
    Count := Pm_Read(FStream, @Buf[0], BUF_SIZE);
    if Count <= 0 then Break;
    for I := 0 to Count - 1 do
      DispatchMessage(Buf[I].message_);
  end;
end;

finalization
  if GMidi <> nil then begin
    GMidi.Free;
    GMidi := nil;
  end;

end.
