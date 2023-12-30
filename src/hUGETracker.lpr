program hUGETracker;

{$MODE Delphi}

{$ifdef DARWIN}
  {$link /usr/local/lib/libSDL2.a}
  {$linkframework CoreAudio}
  {$linkframework AudioToolbox}
{$endif}

uses
{$ifdef UNIX}
  cthreads,
{$endif}
{$ifdef MSWINDOWS}
  Windows,
{$endif}
{$ifdef LCLQT5}
  qt5,
{$endif}
  FileUtil, Dialogs,
  SysUtils,
  Forms,
  Interfaces,
  LazLogger,
  hUGESettings,
  Constants,
  Tracker, EffectEditor, options, about_hugetracker, rendertowave, findreplace,
  utils;

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

var
  PixelitePath: WideString;
begin
  ReturnNilIfGrowHeapFails := False;
  ChDir(ExtractFileDir(ParamStr(0)));
  InitializeTrackerSettings;

  {$if declared(useHeapTrace)}
    // Set up -gh output for the Leakview package:
    if FileExists('heap.trc') then
      DeleteFile('heap.trc');
    SetHeapTraceOutput('heap.trc');

    DebugLn('[DEBUG] Using heaptrc...');
  {$endIf}

  { Before the LCL starts, embed Pixelite so users dont have to install it.
    Unfortunately there isn't really a cross-platform way to do it. }

  {$ifdef MSWINDOWS}
    // $10 is FR_PRIVATE which uninstalls the font when the process ends
    if not AddFontResourceExA(PChar(ConcatPaths([RuntimeDir, 'PixeliteTTF.ttf'])), $10) then
      DebugLn('[ERROR] Couldn''t load Pixelite!!!');
  {$endif}

  {$if defined(LINUX) or defined(FREEBSD) or defined(OPENBSD)}
    // https://gitlab.gnome.org/GNOME/gtk/-/issues/3886
    if FcConfigAppFontAddFile(nil, PChar(ConcatPaths([RuntimeDir, 'PixeliteTTF.ttf']))) = 0 then
      DebugLn('[ERROR] Couldn''t load Pixelite!!!');

    PangoFcFontMapConfigChanged(PangoCairoFontMapGetDefault);
  {$endif}

  {$ifdef DARWIN}
    if not FileExists(ConcatPaths([GetUserDir, 'Library', 'Fonts', 'Pixelite.ttf'])) then begin
      ForceDirectories(ConcatPaths([GetUserDir, 'Library', 'Fonts']));
      CopyFile(ConcatPaths([RuntimeDir, 'PixeliteTTF.ttf']), ConcatPaths([GetUserDir, 'Library', 'Fonts', 'Pixelite.ttf']));
      ShowMessage('Successfully loaded required fonts for the first time. Please restart hUGETracker!');
      Halt;
    end;
  {$endif}

  {$ifdef LCLQT5}
    PixelitePath := WideString(ConcatPaths([RuntimeDir, 'PixeliteTTF.ttf']));
    QFontDatabase_addApplicationFont(@PixelitePath);
  {$endif}

  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfrmTracker, frmTracker);
  Application.CreateForm(TfrmAboutHugeTracker, frmAboutHugetracker);
  Application.CreateForm(TfrmEffectEditor, frmEffectEditor);
  Application.CreateForm(TfrmRenderToWave, frmRenderToWave);
  Application.CreateForm(TfrmOptions, frmOptions);
  Application.CreateForm(TfrmFindReplace, frmFindReplace);
  Application.Run;
end.
