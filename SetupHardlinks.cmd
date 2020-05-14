if not exist lib\Development\i386-win32 goto prod32
pushd lib\Development\i386-win32
mklink/J hUGEDriver ..\..\..\hUGEDriver
mklink/H halt.gb ..\..\..\halt.gb

popd

:prod32
if not exist lib\Production\i386-win32 goto dev64
pushd lib\Production\i386-win32
mklink/J hUGEDriver ..\..\..\hUGEDriver
mklink/H halt.gb ..\..\..\halt.gb

popd

:dev64
if not exist lib\Development\x86_64-win64 goto prod64
pushd lib\Development\x86_64-win64
mklink/J hUGEDriver ..\..\..\hUGEDriver
mklink/H halt.gb ..\..\..\halt.gb

popd

:prod64
if not exist lib\Production\x86_64-win64 goto end
pushd lib\Production\x86_64-win64
mklink/J hUGEDriver ..\..\..\hUGEDriver
mklink/H halt.gb ..\..\..\halt.gb

popd

:end
pause