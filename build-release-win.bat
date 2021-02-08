rem @echo off

for /f %%i in ('where rgbasm') do set rgbasm=%%i
for /f %%i in ('where rgblink') do set rgblink=%%i
for /f %%i in ('where rgbfix') do set rgbfix=%%i

set outdir=Release

:: Build halt.gb
%rgbasm% -o halt.obj halt.asm
if not errorlevel 0 goto buildfail
%rgblink% -n %outdir%\halt.sym -o %outdir%\halt.gb halt.obj
if not errorlevel 0 goto buildfail

erase halt.obj

%rgbfix% -p0 -v %outdir%\halt.gb
if not errorlevel 0 goto fixfail

:: Copy needed stuff

copy %rgbasm% %outdir%\
copy %rgbalink% %outdir%\
copy %rgbfix% %outdir%\

copy Resources\Fonts\PixeliteTTF.ttf %outdir%\
if not errorlevel 0 goto copyfail

copy Resources\Libs\SDL2.dll %outdir%\
if not errorlevel 0 goto copyfail

Xcopy /E /I /Y hUGEDriver %outdir%\hUGEDriver
if not errorlevel 0 goto copyfail

Xcopy /E /I /Y "Resources\Sample Songs" "%outdir%\Sample Songs"
if not errorlevel 0 goto copyfail

:: Zip it up

powershell Compress-Archive Release\* hUGETracker-RELEASE.zip -Force

echo Done
exit/b 0

:norgbasm
echo rgbasm.exe not found!
exit/b 1

:norgblink
echo rgblink not found!
exit/b 1

:norgbfix
echo rgbfix not found!
exit/b 1

:buildfail
echo Building halt.gb and halt.sym failed!
exit/b 1

:fixfail
echo rgbfix failed while fixing halt.gb!
exit/b 1

:copyfail
echo Error while copying required things from resources!
exit/b 1