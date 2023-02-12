# Sound effects

hUGEDriver itself does not provide sound effects, but is compatible with existing sound effect solutions. Both music and sound effects need to contend for the Game Boy's four sound channels, so a little cooperation is required.

The general outline of how to play sound effects and music is as follows:

1. Tell hUGEDriver to "release" a channel; that is, hUGEDriver refrains from touching it at all, which leaves it free for other code to use (say, an SFX engine, or [a sample player](https://github.com/DevEd2/SamplePlayer)).
2. Use your SFX engine to play a sound effect on that channel
3. After the sound effect is complete, tell hUGEDriver to use the channel again, so music will resume playing on that channel

"Releasing" a channel is also known as "muting."

## How-to

### RGBDS (assembly)

Call `hUGE_mute_channel` with `b` containing the channel's ID minus one (so between 0 and 3 inclusive), and `c` being `0` to release the channel, or `1` to unmute it.

### GBDK (C)

Call `hUGE_mute_channel`; see `hUGEDriver.h` for the corresponding arguments.

## Notes

- **IMPORTANT** precaution for CH3: if you release this channel and modify wave RAM, you **must** inform hUGEDriver, by writing `hUGE_NO_WAVE` to `hUGE_current_wave`.
  This will tell hUGEDriver to reload wave RAM when the channel is un-released; otherwise, you may get the wrong waveform when the music resumes.
- "Global" [effects](../hUGETracker/effect-reference.md) (5, 8, B, D, and F) are still processed, even if the channel they are attached to is released.
  Importantly, this means hUGEDriver *will* write to `NR50` (5) and `NR51` (8).

## SFX Engines

These projects aren't part of hUGETracker, but they're known solutions that work well with it:

1. [VGM2GBSFX](https://github.com/untoxa/VGM2GBSFX) - Can use VGM files as sound effects. Since hUGETracker can export to `.vgm`, you can make your sound effects in hUGETracker!
2. [CBT-FX](https://github.com/datguywitha3ds/CBT-FX) - Sound effect engine compatible with FX Hammer
3. [Libbet's SFX Engine](https://github.com/pinobatch/libbet/blob/master/src/audio.z80) - Simple engine for assembly projects
