![hUGETracker](https://nickfa.ro/images/HUGELogo.gif)
---

This is the repository for hUGETracker, the music editing suite for the Gameboy.

If you're looking for the music driver you can include in your homebrew game, check out [hUGEDriver!](https://github.com/SuperDisk/hUGEDriver)

# Build instructions

The project's main branch currently isn't in a production-ready state (it's broken) but that will change soon. So don't follow these instructions quite yet...

The only requirements to build hUGETracker are a recent version of [Lazarus](https://www.lazarus-ide.org/) for your platform.
If you plan on building for other platforms than your own, you'll need the FPC crosscompilers for those platforms. (I recommend using [FPCUpDeluxe](https://github.com/LongDirtyAnimAlf/fpcupdeluxe) but honestly you probably don't need to do this)

```
# Download this repo
git clone --recursive https://github.com/SuperDisk/hUGETracker

# Go into the project directory
cd hUGETracker

# Let Lazarus know about the dependencies that HT uses
lazbuild --add-package-link rackctls/RackCtlsPkg.lpk
lazbuild --add-package-link bgrabitmap/bgrabitmap/bgrabitmappack.lpk

# At this point if you want to develop HT, then open GBEmu.lpi in Lazarus, make sure you're in the 
# Development build mode, and everything should build correctly. However, in order to allow for concurrent
# development on the tracker (this repo) and the sound driver (https://github.com/SuperDisk/hUGEDriver),
# the hUGEDriver folder is not copied to the output directory, and you're expected to symlink it there yourself;
# Pick one of the following:

mklink/J lib\Development\x86_64-win64\hUGEDriver hUGEDriver
ln -s hUGEDriver lib/Development/x86_64-linux/hUGEDriver

# If you just want to build a release for whatever platform you have, pick one of the following:

lazbuild GBEmu.lpi --build-mode="Production Windows"
lazbuild GBEmu.lpi --build-mode="Production Mac"
lazbuild GBEmu.lpi --build-mode="Production Linux"

```
