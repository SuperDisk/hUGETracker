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

{$ifdef UNIX}uses BaseUnix;{$endif}
{$ifdef WINDOWS}uses Windows;{$endif}

procedure Mklink(Src, Dst: String);
begin
  {$ifdef UNIX}
  // TODO....
  {$endif}

  {$ifdef WINDOWS}
    // Todo...
  {$endif}
end;

begin

end.
