![hUGETracker](https://nickfa.ro/images/HUGELogo.gif)
---

This is the repository for hUGETracker, the music editing suite for the Gameboy.

A real readme is coming real soon...

# Build instructions

The only requirements to build this project are a recent version of [Lazarus](https://www.lazarus-ide.org/) for your platform.
If you plan on building for other platforms than your own, you'll need the FPC crosscompilers for those projects. (I recommend using [FPCUpDeluxe](https://github.com/LongDirtyAnimAlf/fpcupdeluxe) but honestly you probably don't need to do this)

```
# Download this repo
git clone --recursive https://github.com/SuperDisk/hUGETracker

# Go into the project directory
cd hUGETracker

# Let Lazarus know about the dependencies that HT uses
lazbuild --add-package-link rackctls\RackCtlsPkg.lpk
lazbuild --add-package-link bgrabitmap\bgrabitmap\bgrabitmappack.lpk

# At this point if you want to develop HT, then open GBEmu.lpi in Lazarus, make sure you're in the 
# Development build mode, and everything should build and run!

# If you want to build a release for whatever platform you have; pick one of the following

lazbuild GBEmu.lpi --build-mode="Production Windows"
lazbuild GBEmu.lpi --build-mode="Production Mac"
lazbuild GBEmu.lpi --build-mode="Production Linux"

```
