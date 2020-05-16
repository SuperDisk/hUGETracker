program GBEmu;

{$MODE Delphi}

uses
{$ifdef unix}
  cthreads,
  // cmem, // the c memory manager is on some systems much faster for multi-threading
{$endif}

{$ifdef MSWINDOWS}
  windows,
{$endif}

{$ifdef UNIX}
// font includes here
{$endif}

{$ifdef DARWIN}
// font includes here
{$endif}

{$ifdef DEVELOPMENT}
 SysUtils,
{$endif}

  Forms,
  Interfaces,
  Tracker in 'Tracker.pas',
  EffectEditor in 'effecteditor.pas',
  Z80CPU in 'CPU\z80cpu.pas',
  sound in 'Sound\sound.pas',
  vars in 'Global\vars.pas',
  options in 'options.pas',
  about_hugetracker in 'about_hugetracker.pas',
  rendertowave in 'rendertowave.pas';

{.$R *.RES}

{$R *.res}

{$ifdef MSWINDOWS}
procedure WindowsExitProc;
begin
  RemoveFontResource('PixeliteTTF.ttf');
end;
{$endif}

begin
  ReturnNilIfGrowHeapFails := False;

  {$if declared(useHeapTrace)}
    // Set up -gh output for the Leakview package:
    if FileExists('heap.trc') then
      DeleteFile('heap.trc');
    SetHeapTraceOutput('heap.trc');

    Writeln(StdErr, '[DEBUG] Using heaptrc...');
  {$endIf}

  { Before the LCL starts, embed Pixelite so users dont have to install it.
    Unfortunately there isn't really a cross-platform way to do it, so here's an
    ifdef mess. }

  {$ifdef MSWINDOWS}
    if AddFontResource('PixeliteTTF.ttf') = 0 then
      Writeln(StdErr, '[ERROR] Couldn''t load Pixelite!!!');

    ExitProc := @WindowsExitProc;
  {$endif}

  Application.Scaled:=True;
  Application.Title:='hUGETracker';
  Application.Initialize;
  Application.CreateForm(TfrmTracker, frmTracker);
  Application.CreateForm(TfrmOptions, frmOptions);
  Application.CreateForm(TfrmAboutHugeTracker, frmAboutHugetracker);
  Application.CreateForm(TfrmEffectEditor, frmEffectEditor);
  Application.CreateForm(TfrmRenderToWave, frmRenderToWave);
  Application.Run;
end.
