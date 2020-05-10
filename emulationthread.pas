unit EmulationThread;

{$mode delphi}

interface

uses
  Classes, SysUtils, epiktimer;

type
  { TEmulationThread }

  TEmulationThread = class(TThread)
  protected
    procedure Execute; override;
    procedure OnFD;
  public
    Constructor Create(ROM: String);
  end;

implementation

uses sound, mainloop, vars, machine, LCLIntf, constants, Tracker;

var
  ET: TEpikTimer;

const
  CyclesPerFrame : Integer = 70224;
  TimePerFrame: Double = 1.0 / 60.0;

procedure TEmulationThread.Execute;
var
  cycles: Extended;
  frameStart, frameEnd: Extended;
  frameElapsedInSec: Extended;
begin
  LastTime := 0;

  Cycles := 0;

  FrameStart := ET.Elapsed;

  repeat
    while (Cycles < CyclesPerFrame) do
      Cycles += z80_decode;

    Cycles -= CyclesPerFrame;

    repeat
      FrameEnd := ET.Elapsed;
      FrameElapsedInSec := (FrameEnd - frameStart);

    until ((FrameElapsedInSec > TimePerFrame) and (not SoundBufferTooFull));

    FrameStart := FrameEnd;
  until Terminated;
end;

procedure TEmulationThread.OnFD;
begin
  PostMessage(frmTracker.Handle, LM_FD, 0, 0);
end;

constructor TEmulationThread.Create(ROM: String);
begin
  inherited Create(True);
  z80_reset;
  ResetSound;
  enablesound;

  FDCallback := OnFD;

  load(ROM);
end;

begin
  ET := TEpikTimer.Create(nil);
  ET.Start;
end.

