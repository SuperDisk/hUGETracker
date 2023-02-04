![hUGETracker](https://nickfa.ro/images/HUGELogo.gif)
---

This is the repository for hUGETracker, the music editing suite for the Gameboy.

If you just want to download hUGETracker, check out [the homepage](https://nickfa.ro/index.php/hUGETracker) or the [releases section!](https://github.com/SuperDisk/hUGETracker/releases)

If you're looking for the music driver you can include in your homebrew game, check out [hUGEDriver!](https://github.com/SuperDisk/hUGEDriver)

If you want help using the tracker, driver, or just want to chat, join the [hUGETracker Discord server!](https://discord.gg/abbHjEj5WH)

# Build instructions

The only requirements to build hUGETracker are a recent version of [Lazarus](https://www.lazarus-ide.org/) for your platform, and [RGBDS.](https://rgbds.gbdev.io/)

```bat
:: Download this repo
git clone --recursive https://github.com/SuperDisk/hUGETracker

:: Go into the project directory
cd hUGETracker

:: Let Lazarus know about the dependencies that HT uses
lazbuild --add-package-link src/rackctls/RackCtlsPkg.lpk
lazbuild --add-package-link src/bgrabitmap/bgrabitmap/bgrabitmappack.lpk

:: At this point, you'll successfully be able to build hUGETracker.
:: However, in order to run properly, it needs some extra files (SDL, halt.gb, fonts, etc)
:: so run the following script to automatically set that up.

setup-windows.cmd
:: or
./setup-linux.sh

:: Now, you can either build and run hUGETracker from within Lazarus,
:: or run one of the following to just build a binary:

lazbuild hUGETracker.lpi --build-mode="Production Windows"
lazbuild hUGETracker.lpi --build-mode="Production Mac"
lazbuild hUGETracker.lpi --build-mode="Production Linux"

```

# License

hUGETracker and hUGEDriver are dedicated to the public domain.
