echo \" <<'BATCH_SCRIPT' >/dev/null ">NUL "\" \`" <#"
@ECHO OFF
REM ===== Batch Script Begin =====
dir
where git
FOR /F "delims=" %%i IN ('git describe --tags --always') DO set gittag=%%i
echo '%gittag%' > revision.inc
REM ====== Batch Script End ======
GOTO :eof
TYPE CON >NUL
BATCH_SCRIPT
#> | Out-Null

set +o histexpand 2>/dev/null
# ===== Bash Script Begin =====
ls
which git
echo \'$(git describe --tags --always)\' > revision.inc
# ====== Bash Script End ======
case $- in *"i"*) cat /dev/stdin >/dev/null ;; esac
exit
#>