set BUILD_DIR=src\lib\Development\x86_64-win64
set SDL_URL=https://github.com/libsdl-org/SDL/releases/download/release-2.26.2/SDL2-2.26.2-win32-x64.zip

mkdir %BUILD_DIR%

rem Get SDL
curl -LO %SDL_URL%
tar -Oxf SDL2-2.26.2-win32-x64.zip SDL2.dll > %BUILD_DIR%\SDL2.dll

rem Copy font
copy fonts\PixeliteTTF.ttf %BUILD_DIR%\

rem Compile halt.gb
rgbasm -E -i src\hUGEDriver -o hUGEDriver.obj src\hUGEDriver\hUGEDriver.asm
rgbasm -i src\hUGEDriver\include -o halt.obj src\halt.asm
rgblink -o %BUILD_DIR%\halt.gb -n %BUILD_DIR%\halt.sym halt.obj hUGEDriver.obj
rgbfix -vp0xFF %BUILD_DIR%\halt.gb

rem Link the hUGEDriver directory
mklink/J %BUILD_DIR%\hUGEDriver src\hUGEDriver