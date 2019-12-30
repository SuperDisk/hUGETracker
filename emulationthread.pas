unit EmulationThread;

{$mode delphi}

interface

uses
  {$ifdef unix}
  cthreads,
  cmem, // the c memory manager is on some systems much faster for multi-threading
  {$endif}
  Classes, SysUtils, epiktimer;

type
  { TEmulationThread }

  TEmulationThread = class(TThread)
  private
    ET: TEpikTimer;
  protected
    procedure Execute; override;
  public
    Constructor Create(ROM: String);
    destructor Destroy; override;
  end;

implementation

uses sound, mainloop, vars, machine;

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
  ET.Start;

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

  ET := TEpikTimer.Create(nil);

  load(ROM);
end;

destructor TEmulationThread.Destroy;
begin
  inherited Destroy;

  ET.Free;
end;

end.

