rem @echo off

for /f %%i in ('where rgbasm') do set rgbasm=%%i
for /f %%i in ('where rgblink') do set rgblink=%%i
for /f %%i in ('where rgbfix') do set rgbfix=%%i
for /f %%i in ('where ffmpeg') do set ffmpeg=%%i

set outdir=%1
if x%outdir%==x goto no_outdir

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
copy %rgblink% %outdir%\
copy %rgbfix% %outdir%\
copy %ffmpeg% %outdir%\

copy Resources\Fonts\PixeliteTTF.ttf %outdir%\
if not errorlevel 0 goto copyfail

copy Resources\Libs\SDL2.dll %outdir%\
if not errorlevel 0 goto copyfail

mkdir %outdir%\hUGEDriver
pushd %outdir%\hUGEDriver
curl -L https://api.github.com/repos/SuperDisk/hUGEDriver/tarball | tar xf - --strip 1
popd

Xcopy /E /I /Y "Resources\Sample Songs" "%outdir%\Sample Songs"
if not errorlevel 0 goto copyfail

:: Zip it up

powershell Compress-Archive %outdir%\* hUGETracker-%outdir%.zip -Force

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

:no_outdir
echo Output directory not specified!
exit/b 1