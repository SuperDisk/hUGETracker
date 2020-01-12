program GBEmu;

{$MODE Delphi}

uses
{$ifdef MSWINDOWS}
  windows,
{$endif}

{$ifdef UNIX}
// font includes here
{$endif}

{$ifdef DARWIN}
// font includes here
{$endif}

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
  mainloop in 'mainloop.pas', options;

{.$R *.RES}

{$R *.res}

begin
  { Before the LCL starts, embed Pixelite so users dont have to install it.
    Unfortunately there isn't really a cross-platform way to do it, so here's an
    ifdef mess. }

  {$ifdef MSWINDOWS}
    if AddFontResource('PixeliteTTF.ttf') = 0 then
      Writeln(StdErr, 'Couldn''t load Pixelite!!!');
  {$endif}

  Application.Scaled:=True;
  Application.Title:='hUGETracker';
  Application.Initialize;
  Application.CreateForm(TfrmTracker, frmTracker);
  Application.CreateForm(TfrmOptions, frmOptions);
  Application.Run;
end.
