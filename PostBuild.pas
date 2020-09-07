{
  This script runs after build.

  In a production build, it populates the directory with
  - Halt.gb
  - hUGEDriver
  - The manual
  - PixeliteTTF
  - Sample Songs

  In a development build, it hardlinks
  - Halt.gb
  - hUGEDriver
  - PixeliteTTF

  into the build output folder so that the driver and tracker can be developed
  simultaneously.
}

{$mode objfpc}{$H+}

uses SysUtils, StrUtils;

procedure HTCopyFile(Source: String);
begin
  {$ifdef WINDOWS}
    ExecuteProcess('cmd.exe', '/c "xcopy /e /Y ' + ExpandFileName(Source) + ' ' + ParamStr(2) + ExtractFileName(Source) + '" > NUL');
  {$endif}

  {$ifdef UNIX}
    ExecuteProcess('/bin/bash', '-c "cp -rf ' + Source + ' ' + ParamStr(2) + ExtractFileName(Source) + '" > /dev/null');
  {$endif}
end;

var
  I: Integer;
  Dev: Boolean;
  DestDir: String;
begin
  DestDir := ParamStr(2);
  Dev := ContainsText(ParamStr(2), 'Development');

  HTCopyFile('Resources/ROMs/halt.gb');
  HTCopyFile('Resources/Fonts/PixeliteTTF.ttf');

  if not Dev then begin
    HTCopyFile('hUGEDriver');
    HTCopyFile('Resources/SampleSongs');
  end else begin
    if not DirectoryExists(DestDir+'hUGEDriver') then
      Writeln('hUGEDriver not copied in development mode. Manually link it there yourself!');
  end;
end.
