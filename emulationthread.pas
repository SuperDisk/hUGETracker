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
  public
    Constructor Create(ROM: String);
  end;

implementation

uses sound, mainloop, vars, machine;

var
  ET: TEpikTimer;

const
  CyclesPerFrame : Integer = 70224;
  TimePerFrame: Double = 1.0 / 60.0;

procedure TEmulationThread.Execute;
var
  tickFreq, cycles: Extended;
  frameStart, frameEnd: Extended;
  frameElapsedInSec: Double;
function GetCounter: Integer;
begin
  Result := Trunc(ET.Elapsed*1000000); // Convert to microseconds
end;
begin
  lastTime := 0;

  cycles := 0;

  tickFreq := 1000000; // Convert to microseconds
  FrameStart := GetCounter;

  repeat
    while (cycles < CyclesPerFrame) do
      cycles += z80_decode;

    cycles -= CyclesPerFrame;

    repeat
      FrameEnd := GetCounter;

      frameElapsedInSec := (frameEnd - frameStart) / tickFreq;
    until (frameElapsedInSec > TimePerFrame) and (not SoundBufferTooFull);

    frameStart := frameEnd;
  until Terminated;
end;

constructor TEmulationThread.Create(ROM: String);
begin
  inherited Create(True);
  z80_reset;
  ResetSound;
  enablesound;

  load(ROM);
end;

begin
  ET := TEpikTimer.Create(nil);
  ET.Start;
end.

