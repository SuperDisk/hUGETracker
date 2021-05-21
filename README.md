![hUGETracker](https://nickfa.ro/images/HUGELogo.gif)
---

This is the repository for hUGETracker, the music editing suite for the Gameboy.

If you just want to download hUGETracker, check out [the homepage](https://nickfa.ro/index.php/hUGETracker) or the [releases section!](https://github.com/SuperDisk/hUGETracker/releases)

If you're looking for the music driver you can include in your homebrew game, check out [hUGEDriver!](https://github.com/SuperDisk/hUGEDriver)

If you want help using the tracker, driver, or just want to chat, join the [hUGETracker Discord server!](https://discord.gg/abbHjEj5WH)

# Build instructions

The general build instructions are yet to be 100% solidified, but these steps will guide you in the right direction.

The only requirements to build hUGETracker are a recent version of [Lazarus](https://www.lazarus-ide.org/) for your platform.
If you plan on building for other platforms than your own, you'll need the FPC crosscompilers for those platforms. (I recommend using [FPCUpDeluxe](https://github.com/LongDirtyAnimAlf/fpcupdeluxe) but honestly you probably don't need to do this)

```bat
:: Download this repo
git clone --recursive https://github.com/SuperDisk/hUGETracker

:: Go into the project directory
cd hUGETracker

:: Let Lazarus know about the dependencies that HT uses
lazbuild --add-package-link rackctls/RackCtlsPkg.lpk
lazbuild --add-package-link bgrabitmap/bgrabitmap/bgrabitmappack.lpk

:: At this point if you want to develop HT, then open hUGETracker.lpi in Lazarus, make sure you're in the 
:: Development build mode, and everything should build correctly. However, in order to allow for concurrent
:: development on the tracker (this repo) and the sound driver (https://github.com/SuperDisk/hUGEDriver),
:: the hUGEDriver folder is not copied to the output directory, and you're expected to symlink it there yourself;
:: Pick one of the following:

mklink/J lib\Development\x86_64-win64\hUGEDriver hUGEDriver
ln -s hUGEDriver lib/Development/x86_64-linux/hUGEDriver

:: If you just want to build a release for whatever platform you have, pick one of the following:

lazbuild hUGETracker.lpi --build-mode="Production Windows"
lazbuild hUGETracker.lpi --build-mode="Production Mac"
lazbuild hUGETracker.lpi --build-mode="Production Linux"

```

# License

hUGETracker and hUGEDriver are dedicated to the public domain.
