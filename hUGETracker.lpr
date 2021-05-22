program hUGETracker;

{$MODE Delphi}

{$ifdef DARWIN}
  {$linklib SDL2}
{$endif}

uses
{$ifdef unix}
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
Function AddFont    (Dir : PAnsiChar;
                      Flag: DWORD): LongBool; StdCall;
                      External GDI32
                      Name 'AddFontResourceExA';
{$endif}

var
  outputIndex : Integer;
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
    Unfortunately there isn't really a cross-platform way to do it. }

  {$ifdef MSWINDOWS}
    // $10 is FR_PRIVATE which uninstalls the font when the process ends
    if not AddFont(PChar('PixeliteTTF.ttf'), $10) then
      Writeln(StdErr, '[ERROR] Couldn''t load Pixelite!!!');
  {$endif}

  {$ifdef UNIX}
    // This is the correct way to load a font on Linux according to
    // #linux on freenode.
    if not DirectoryExists(GetUserDir+'.fonts') then
      CreateDir(GetUserDir+'.fonts');

    if not (CopyFile('./PixeliteTTF.ttf', GetUserDir+'.fonts/PixeliteTTF.ttf')
    and    (ExecuteProcess('/bin/bash', '-c fc-cache') = 0))
    then    Writeln(StdErr, '[ERROR] Couldn''t copy Pixelite to ~/.fonts !!!');
  {$endif}

  {$ifdef PRODUCTION}
    // Add a number to the log file name if the file is in use.
    outputIndex := 0;
    while True do
    begin
      Assign(Output, 'output' + IntToStr(outputIndex) + '.log');
      try
        Rewrite(Output);
        // if Rewrite is successful, exit loop
        Break;
      except
        inc(outputIndex);
      end;
      // Give up if the file access fails more than 5 times
      if outputIndex > 5 then Break;
    end;
  {$endif}

  Application.Scaled:=True;
  Application.Title:='hUGETracker';
  Application.Initialize;
  Application.CreateForm(TfrmTracker, frmTracker);
  Application.CreateForm(TfrmAboutHugeTracker, frmAboutHugetracker);
  Application.CreateForm(TfrmEffectEditor, frmEffectEditor);
  Application.CreateForm(TfrmRenderToWave, frmRenderToWave);
  Application.CreateForm(TfrmOptions, frmOptions);
  Application.Run;
end.
