# HACK: MacOS 12 doesn't have realpath. 13 does, but GH Actions don't have it in non-beta yet.
_realpath() {
cd "$(dirname "$0")" ; pwd -P
}

set -x

curl -Lo lame.tar.gz 'https://downloads.sourceforge.net/project/lame/lame/3.100/lame-3.100.tar.gz'
tar xf lame.tar.gz
pushd lame-3.100
./configure --prefix="`_realpath ..`/lame-install" --disable-dependency-tracking --disable-shared --enable-nasm --disable-gtktest --disable-decoder --disable-frontend
make
make install
popd
curl -Lo ffmpeg.tar.xz 'http://ffmpeg.org/releases/ffmpeg-4.4.tar.xz'
tar xf ffmpeg.tar.xz
cd ffmpeg-4.4
echo ./configure --pkg-config-flags=--static --disable-debug --enable-lto --disable-programs --enable-ffmpeg --disable-doc --disable-everything --enable-protocol=pipe --enable-protocol=file --enable-filter=aresample --enable-decoder=pcm_f32le --enable-demuxer=pcm_f32le --enable-encoder=flac --enable-muxer=ogg --enable-muxer=flac --enable-encoder=pcm_s16le --enable-muxer=wav --enable-libmp3lame --enable-encoder=libmp3lame --enable-muxer=mp3 --extra-cflags="-I`_realpath ../lame-install/include`" --extra-ldflags="-L`_realpath ../lame-install/lib`"
./configure --pkg-config-flags=--static --disable-debug --enable-lto --disable-programs --enable-ffmpeg --disable-doc --disable-everything --enable-protocol=pipe --enable-protocol=file --enable-filter=aresample --enable-decoder=pcm_f32le --enable-demuxer=pcm_f32le --enable-encoder=flac --enable-muxer=ogg --enable-muxer=flac --enable-encoder=pcm_s16le --enable-muxer=wav --enable-libmp3lame --enable-encoder=libmp3lame --enable-muxer=mp3 --extra-cflags="-I`_realpath ../lame-install/include`" --extra-ldflags="-L`_realpath ../lame-install/lib`" || cat ffbuild/config.log
make
