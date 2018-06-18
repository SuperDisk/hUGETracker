program GBEmu;

{$MODE Delphi}

uses
  Forms, Interfaces,
  UnitMain in 'UnitMain.pas' {frmGameboy},
  debugger in 'Debugger\debugger.pas',
  directdraw in 'Directx\directdraw.pas',
  dxcommon in 'DirectX\dxcommon.pas',
  ddraw_out in 'GPU\ddraw_out.pas',
  dib_out in 'GPU\dib_out.pas',
  gfx in 'GPU\gfx.pas',
  machine in 'CPU\machine.pas',
  Z80CPU in 'CPU\z80cpu.pas',
  sound in 'Sound\sound.pas',
  vars in 'Global\vars.pas',
  mainloop in 'mainloop.pas',
  UnitDebug in 'Debugger\UnitDebug.pas' {frmDebug},
  Direct3DRM in 'DirectX\Direct3DRM.pas',
  DirectInput in 'DirectX\DirectInput.pas',
  DirectMusic in 'DirectX\DirectMusic.pas',
  DirectPlay in 'DirectX\DirectPlay.pas',
  DirectSetup in 'DirectX\DirectSetup.pas',
  DirectSound in 'DirectX\DirectSound.pas',
  Direct3D in 'DirectX\Direct3D.pas',
  UnitMapview in 'Debugger\UnitMapview.pas' {frmMapview};

{.$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmGameboy, frmGameboy);
  Application.CreateForm(TfrmDebug, frmDebug);
  Application.CreateForm(TfrmMapview, frmMapview);
  Application.Run;
end.
