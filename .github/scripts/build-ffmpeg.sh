set -x

curl -Lo lame.tar.gz 'https://downloads.sourceforge.net/project/lame/lame/3.100/lame-3.100.tar.gz'
tar xf lame.tar.gz
pushd lame-3.100
./configure --prefix="`realpath ..`/lame-install" --disable-dependency-tracking \
            --enable-static --disable-shared --enable-nasm --disable-gtktest \
            --disable-decoder --disable-frontend
make
make install
popd

curl -Lo ffmpeg.tar.xz 'http://ffmpeg.org/releases/ffmpeg-4.4.tar.xz'
tar xf ffmpeg.tar.xz
cd ffmpeg-4.4

if [ "$(uname)" = "Darwin" ]; then
    STATIC_FLAG="--pkg-config-flags=--static"
else
    STATIC_FLAG="--extra-ldflags=-static"
fi

./configure $STATIC_FLAG \
            --disable-debug --enable-lto --disable-programs --enable-ffmpeg \
            --disable-doc --disable-everything --enable-protocol=pipe --enable-protocol=file \
            --enable-filter=aresample --enable-decoder=pcm_f32le --enable-demuxer=pcm_f32le \
            --enable-encoder=flac --enable-muxer=ogg --enable-muxer=flac --enable-encoder=pcm_s16le \
            --enable-muxer=wav --enable-libmp3lame --enable-encoder=libmp3lame --enable-muxer=mp3 \
            --extra-cflags="-I`realpath ../lame-install/include`" \
            --extra-ldflags="-L`realpath ../lame-install/lib`"
make
