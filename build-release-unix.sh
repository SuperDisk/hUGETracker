#!/bin/bash
set -x #echo on

asm=$(which rgbasm)
link=$(which rgblink)
fix=$(which rgbfix)
ffm=$(which ffmpeg)

if [ -z "$asm" ]; then {echo "rgbasm not found!"; exit 1; } fi
if [ -z "$link" ]; then {echo "rgblink not found!"; exit 1; } fi
if [ -z "$fix" ]; then {echo "rgbfix not found!"; exit 1; } fi
if [ -z "$ffm" ]; then {echo "ffmpeg not found!"; exit 1; } fi

outdir="$1"
if [ -z "$outdir" ]
then
    echo "Output directory not specified!"
    exit 1
fi

# Build halt.gb

function check_fail() {
    if [ $? -eq 0 ]
    then
        echo "Failed, aborting."
        exit 1
    fi
}

rgbasm -o halt.obj halt.asm
check_fail

rgblink -n $outdir/halt.sym -o $outdir/halt.gb halt.obj
check_fail

rm halt.obj
check_fail

rgbfix -p0 -v $outdir/halt.gb
check_fail

# Copy needed stuff

cp $asm $outdir/
cp $link $outdir/
cp $fix $outdir/
cp $ffm $outdir/

cp Resources/Fonts/PixeliteTTF.ttf $outdir/
check_fail

mkdir $outdir/hUGEDriver
pushd $outdir/hUGEDriver
curl -L https://api.github.com/repos/SuperDisk/hUGEDriver/tarball | tar xzf - --strip 1
check_fail
popd

cp -r "Resources/Sample Songs" "$outdir/Sample Songs"
check_fail

tar czf hUGETracker-$outdir.tar.gz $outdir
check_fail

echo Done
