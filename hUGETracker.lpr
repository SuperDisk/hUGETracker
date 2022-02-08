program hUGETracker;

{$MODE Delphi}

{$ifdef DARWIN}
  {$linklib SDL2}
{$endif}

uses
{$ifdef UNIX}
  cthreads,
{$endif}
{$ifdef MSWINDOWS}
  Windows,
{$endif}
  FileUtil,
  SysUtils,
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

{$R *.res}

{$ifdef MSWINDOWS}
// https://forum.lazarus.freepascal.org/index.php?topic=39124.0
function AddFontResourceExA(Dir: PAnsiChar; Flag: DWORD): LongBool; StdCall; External GDI32;
{$endif}

{$if defined(LINUX) or defined(FREEBSD) or defined(OPENBSD)}
function FcConfigAppFontAddFile(Config: Pointer; _File: PChar): Integer; cdecl; External 'libfontconfig.so';
function PangoCairoFontMapGetDefault: Pointer; cdecl; External 'libpangocairo-1.0.so' Name 'pango_cairo_font_map_get_default';
procedure PangoFcFontMapConfigChanged(FcFontMap: Pointer); cdecl; External 'libpangoft2-1.0.so' Name 'pango_fc_font_map_config_changed';
{$endif}

begin
  ReturnNilIfGrowHeapFails := False;

  // Change working directory to the tracker directory
  ChDir(ExtractFileDir(ParamStr(0)));

  {$if declared(useHeapTrace)}
    // Set up -gh output for the Leakview package:
    if FileExists('heap.trc') then
      DeleteFile('heap.trc');
    SetHeapTraceOutput('heap.trc');

    Writeln(StdErr, '[DEBUG] Using heaptrc...');
  {$endIf}

  { Before the LCL starts, embed Pixelite so users dont have to install it.
    Unfortunately there isn't really a cross-platform way to do it. }

  {$ifdef MSWINDOWS}
    // $10 is FR_PRIVATE which uninstalls the font when the process ends
    if not AddFontResourceExA(PChar('PixeliteTTF.ttf'), $10) then
      Writeln(StdErr, '[ERROR] Couldn''t load Pixelite!!!');
  {$endif}

  {$if defined(LINUX) or defined(FREEBSD) or defined(OPENBSD)}
    // https://gitlab.gnome.org/GNOME/gtk/-/issues/3886
    if FcConfigAppFontAddFile(nil, PChar('PixeliteTTF.ttf')) = 0 then
      Writeln(StdErr, '[ERROR] Couldn''t load Pixelite!!!');

    PangoFcFontMapConfigChanged(PangoCairoFontMapGetDefault);
  {$endif}

  {$ifdef PRODUCTION}
  Output := StdErr;
  {$endif}

  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfrmTracker, frmTracker);
  Application.CreateForm(TfrmAboutHugeTracker, frmAboutHugetracker);
  Application.CreateForm(TfrmEffectEditor, frmEffectEditor);
  Application.CreateForm(TfrmRenderToWave, frmRenderToWave);
  Application.CreateForm(TfrmOptions, frmOptions);
  Application.Run;
end.
