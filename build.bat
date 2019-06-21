@echo off
rgbasm -odriver.obj driver.z80
if %errorlevel% neq 0 call :exit 1
rgblink -mdriver.map -ndriver.sym -odriver.gb driver.obj
if %errorlevel% neq 0 call :exit 1
rgbfix -p0 -v driver.gb
if %errorlevel% neq 0 call :exit 1
call :exit 0

:exit
exit