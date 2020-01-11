program GBEmu;

{$MODE Delphi}

uses
{$ifdef unix}
  cthreads,
  cmem, // the c memory manager is on some systems much faster for multi-threading
{$endif}
  Forms,
  Interfaces,
  Tracker in 'Tracker.pas',
  EffectEditor in 'effecteditor.pas',
  Z80CPU in 'CPU\z80cpu.pas',
  sound in 'Sound\sound.pas',
  vars in 'Global\vars.pas',
  mainloop in 'mainloop.pas';

{.$R *.RES}

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Title:='hUGETracker';
  Application.Initialize;
  Application.CreateForm(TfrmTracker, frmTracker);
  Application.Run;
end.
