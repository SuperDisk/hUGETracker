program GBEmu;

{$MODE Delphi}

uses
  Forms,
  Interfaces,
  UnitMain in 'UnitMain.pas' {frmGameboy},
  Tracker in 'Tracker.pas',
  debugger in 'Debugger\debugger.pas',
  gfx in 'GPU\gfx.pas',
  machine in 'CPU\machine.pas',
  Z80CPU in 'CPU\z80cpu.pas',
  sound in 'Sound\sound.pas',
  vars in 'Global\vars.pas',
  mainloop in 'mainloop.pas',
  UnitDebug in 'Debugger\UnitDebug.pas' {frmDebug},
  UnitMapview in 'Debugger\UnitMapview.pas' {frmMapview};

{.$R *.RES}

{$R *.res}

begin
  Application.Initialize;
  {Application.CreateForm(TfrmGameboy, frmGameboy);
  Application.CreateForm(TfrmDebug, frmDebug);
  Application.CreateForm(TfrmMapview, frmMapview);}
  Application.CreateForm(TfrmTracker, frmTracker);
  Application.Run;
end.
