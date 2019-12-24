unit EmulationThread;

{$mode delphi}

interface

uses
  {$ifdef unix}
  cthreads,
  cmem, // the c memory manager is on some systems much faster for multi-threading
  {$endif}
  Classes, SysUtils;

type
  TEmulationThread = class(TThread)
  private
    //stuff
  protected
    procedure Execute; override;
  public
    Constructor Create(ROM: String);
  end;

implementation

uses sound, mainloop, vars, machine,
  windows;

const
  CyclesPerFrame : Integer = 70224;
  TimePerFrame: Double = 1.0 / 60.0;

procedure TEmulationThread.Execute;
var
  li: Large_Integer;
  tickFreq, cycles: Integer;
  frameStart, frameEnd: Integer;
  frameElapsedInSec: Double;
function GetCounter: Integer;
begin
  QueryPerformanceCounter(@li);
  Result := li.QuadPart
end;
begin
  lastTime := 0;

  cycles := 0;

  QueryPerformanceFrequency(@li);
  tickFreq := li.QuadPart;
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

end.

