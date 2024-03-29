set -x

BUILD_DIR=src/lib/Development/x86_64-linux

mkdir -p $BUILD_DIR

# Copy font
cp fonts/PixeliteTTF.ttf $BUILD_DIR/

# Compile halt.gb
rgbasm -H -E -I src/hUGEDriver -o hUGEDriver.obj src/hUGEDriver/hUGEDriver.asm
rgbasm -H -I src/hUGEDriver/include -o halt.obj src/halt.asm
rgblink -o $BUILD_DIR/halt.gb -n $BUILD_DIR/halt.sym halt.obj hUGEDriver.obj
rgbfix -vp0xFF $BUILD_DIR/halt.gb

# Link the hUGEDriver directory
ln -s `realpath src/hUGEDriver` `realpath $BUILD_DIR/hUGEDriver`
