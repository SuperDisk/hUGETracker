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
  tickFreq, cycles: Extended;
  frameStart, frameEnd: Extended;
  frameElapsedInSec: Extended;
function GetCounter: Integer;
begin
  Result := Trunc(ET.Elapsed*1000000); // Convert to microseconds
end;
begin
  writeln('starting that thread up');

  lastTime := 0;

  cycles := 0;

  tickFreq := 1000000; // Convert to microseconds
  FrameStart := GetCounter;

  repeat
    writeln('thread loop begin');
    Flush(Output);

    while (cycles < CyclesPerFrame) do
      cycles += z80_decode;
    writeln('cycles part done');
    Flush(Output);

    cycles -= CyclesPerFrame;
    writeln('subbing cycles');
    Flush(Output);

    // TODO: Replace this with an actual timing mechanism. This devours CPU.
    {repeat
      FrameEnd := GetCounter;

      frameElapsedInSec := (frameEnd - frameStart) / tickFreq;
    until (frameElapsedInSec > TimePerFrame) and (not SoundBufferTooFull);  }

    //sleep(15);
    //writeln('sleep done');

    //frameStart := frameEnd;

    writeln('thread loop completed');
    Flush(Output);
    Flush(Stdout);
  until Terminated;

  writeln('thread dying');
  Flush(Output);
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

