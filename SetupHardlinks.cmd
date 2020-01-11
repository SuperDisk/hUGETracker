pushd lib\Development\i386-win32
mklink/J hUGEDriver ..\..\..\hUGEDriver
mklink/H halt.gb ..\..\..\halt.gb

popd

pushd lib\Production\i386-win32
mklink/J hUGEDriver ..\..\..\hUGEDriver
mklink/H halt.gb ..\..\..\halt.gb

popd

pause