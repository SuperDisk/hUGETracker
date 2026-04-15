#!/usr/bin/env bash
# Clones and builds PortMidi as a shared library. The resulting install tree
# is placed in $GITHUB_WORKSPACE/portmidi-install so the packaging steps for
# each platform can copy the right files out of it.
#
# PortMidi is built against:
#   - ALSA on Linux (requires libasound2-dev)
#   - CoreMIDI on macOS
#   - WinMM on Windows via MSYS2/MinGW
set -eux

PORTMIDI_REF="v2.0.5"

if [ "$(uname -s)" = "Darwin" ]; then
    JOBS=$(sysctl -n hw.ncpu)
else
    JOBS=$(nproc 2>/dev/null || echo 2)
fi

# $GITHUB_WORKSPACE isn't set on all shells (e.g. MSYS2 with path-type strict),
# but the working directory at job start *is* the workspace. Resolve it
# explicitly so install paths are predictable on every runner.
WORKSPACE="$(pwd)"
INSTALL_PREFIX="${WORKSPACE}/portmidi-install"

if [ -d portmidi-src ]; then
    rm -rf portmidi-src
fi

git clone --depth 1 --branch "${PORTMIDI_REF}" https://github.com/PortMidi/portmidi.git portmidi-src
cd portmidi-src

# PortMidi defaults BUILD_SHARED_LIBS=OFF; flip it so we get a .so/.dylib/.dll
# we can ship next to the hUGETracker binary.
CMAKE_EXTRA=()
if [ "$(uname -s | head -c 5)" = "MINGW" ] || [ "$(uname -s | head -c 5)" = "MSYS_" ] || [ "${OS:-}" = "Windows_NT" ]; then
    CMAKE_EXTRA+=(-G "MSYS Makefiles")
fi

cmake -S . -B build \
      -DCMAKE_BUILD_TYPE=Release \
      -DBUILD_SHARED_LIBS=ON \
      -DCMAKE_INSTALL_PREFIX="${INSTALL_PREFIX}" \
      "${CMAKE_EXTRA[@]}"

cmake --build build --config Release -j${JOBS}
cmake --install build --config Release

ls -l "${INSTALL_PREFIX}"
find "${INSTALL_PREFIX}" -maxdepth 3 -type f -o -type l | sort
