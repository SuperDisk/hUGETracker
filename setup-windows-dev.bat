set outdir=lib\Development\x86_64-win64
mkdir %outdir%

copy Resources\Fonts\PixeliteTTF.ttf %outdir%\
if not errorlevel 0 goto copyfail

copy Resources\Libs\SDL2.dll %outdir%\
if not errorlevel 0 goto copyfail

Xcopy /E /I /Y hUGEDriver %outdir%\hUGEDriver
if not errorlevel 0 goto copyfail

Xcopy /E /I /Y "Resources\Sample Songs" "%outdir%\Sample Songs"
if not errorlevel 0 goto copyfail

exit/b 0

:copyfail

echo Error while copying required files!
exit/b 1