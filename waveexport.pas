unit WaveExport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure ExportWaveToFile(Filename: String);

implementation

uses sound, mainloop, vars, machine, constants;

procedure ExportWaveToFile(Filename: String);
var
  CompletedCycles: QWord = 0;
begin
  z80_reset;
  ResetSound;
  enablesound;

  BeginWritingSoundToFile(Filename);

  FDCallback := nil;

  load('hUGEDriver/preview.gb');

  while CompletedCycles < (70224*60)*10 do
    Inc(CompletedCycles, z80_decode);

  EndWritingSoundToFile;
end;

end.

