# Integration

Integrating hUGEDriver into your project depends on what you are using.

<details><summary>RGBDS (assembly)</summary>

Import [`hUGEDriver.asm`](https://github.com/SuperDisk/hUGEDriver/blob/master/hUGEDriver.asm) and [`hUGE.inc`](https://github.com/SuperDisk/hUGEDriver/blob/master/include/hUGE.inc) (in the `include` directory) into your project (songs need the latter as well).
You will additionally need [`hardware.inc`](https://github.com/gbdev/hardware.inc) 4.2 or later, if you don't already.

Then, simply compile `hUGEDriver.asm` with the rest of your code, and you're done!

</details>

<details><summary>GBDK (C)</summary>

> **Note**: some people circulate pre-compiled versions of `hUGEDriver.o` / `hUGEDriver.lib`; this is fine, but you must be sure to use the right version.
Also, if you want to modify the driver yourself for any reason, you will need to compile it yourself afterwards.

hUGEDriver is written in RGBDS assembly, which is not compatible with SDCC's assembler (SDAS); so a few extra steps are necessary.

0. You will need [RGBDS](https://rgbds.gbdev.io), and the [FreePascal](https://www.freepascal.org) compiler.
1. Assemble hUGEDriver: `rgbasm -o hUGEDriver.obj hUGEDriver.asm`
2. Compile `rgb2sdas`: `make -C tools`
3. Convert the object file: `tools/rgb2sdas hUGEDriver.obj`
4. Import `hUGEDriver.h` into your project
5. Simply link `hUGEDriver.o` with the rest of your code, and you're done!

</details>

## Usage

There are two parts to using hUGEDriver: *initializing* a song, and *playing* a song.

### Initialization

You begin playing a song by passing a pointer to it to the `hUGE_init` function.
How can you get such a pointer, though?
Simple!
The "song descriptor" that you choose when exporting your song names a label (assembly) / variable (C) that points to the song!
So if your song descriptor was, say, `ryukenden`:

<table><thead><tr><th>Assembly</th><th>C</th></tr></thead><tbody><tr><td>

```avrasm
ld de, ryukenden
call hUGE_init
```

</td><td>

```c
hUGE_init(ryukenden);
```

</td></tr></tbody></table>

### Playing

The function `hUGE_dosound` plays a single tick of the current song when called.
Sounds simple?
Well, unfortunately, there are a few of gotchas.

First and foremost, *how often* should that function be called?
That actually depends on what the song expects!

**TODO: screenshot**

If the song does not use "timer playback", then `hUGE_dosound` must be called once per frame.
This is usually done either by calling it from your game's main loop (or whatever), or by calling it from your VBlank interrupt handler.
However, if the song *does* use timer playback, then you must set the timer registers appropriately (TODO), and call `hUGE_dosound` from the timer interrupt handler.

Using any interrupt handler exposes you to two additional gotchas:

- `hUGE_dosound` must not run in the middle of `hUGE_init`.
- `hUGE_dosound` must not be called before the first call ever to `hUGE_init` complete.

Preferably, create a variable that you set to 0 on boot &amp; before calling `hUGE_init`, and to 1 after `hUGE_init` returns; in the interrupt handler, skip calling `hUGE_dosound` if the variable isn't 0.
An alternative (popular, but with several drawbacks) is to disable interrupts (ASM: `di`+`ei`, GBDK: `__critical`) while you call `hUGE_init`.
