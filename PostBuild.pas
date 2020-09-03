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

  into the build output folder so that the driver and tracker can be developed
  simultaneously.
}

{$mode objfpc}{$H+}

uses SysUtils,

{$ifdef UNIX}BaseUnix;{$endif}
{$ifdef WINDOWS}Windows;{$endif}

// Cringe
{$ifdef WINDOWS}
const
  SYMBOLIC_LINK_FLAG_DIRECTORY = $00000001;

function CreateSymbolicLinkA(
  lpSymlinkFileName: PAnsiChar;
  lpTargetFileName: PAnsiChar;
  dwFlags: DWORD
): Boolean; stdcall; external 'kernel32';
{$endif}

procedure Mklink(Src, Dst: AnsiString; Dir: Boolean = False);
begin
  writeln(src, ', ', dst, ', ', dir);
  {$ifdef UNIX}
  // TODO....
  {$endif}

  {$ifdef WINDOWS}
    if Dir then
      writeln('link dir ', CreateSymbolicLinkA(PAnsiChar(Dst), PAnsiChar(Src), SYMBOLIC_LINK_FLAG_DIRECTORY))
    else
      writeln('link file ', CreateSymbolicLinkA(PAnsiChar(Dst), PAnsiChar(Src), 0))
  {$endif}
end;

var
  I: Integer;
  Dev: Boolean;
  DestDir: String;
begin
  for I := 0 to ParamCount-1 do writeln(Paramstr(I));
  DestDir := ParamStr(2);
  Dev := True; ///(ParamCount > 2) and (ParamStr(2) = '-d');

  if Dev then begin
    Mklink('hUGETracker', DestDir+'hUGETracker', True);
    Mklink('Resources/ROMs/halt.gb', DestDir+'halt.gb');
  end else begin
    writeln('not implemented.');
  end;
end.
