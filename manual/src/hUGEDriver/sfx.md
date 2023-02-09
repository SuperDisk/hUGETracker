# Sound effects

hUGEDriver itself does not support sound effects; there exist many solutions for that out there!
But, both music and sound effects need to contend for the Game Boy's four sound channels, so a little cooperation is required.

> **Note**: at the moment, there are a few bugs where hUGEDriver will still write to a channel when it's "released".
> If you encounter any, please [report them](../contact.md)!

hUGEDriver cooperates by being able to arbitrarily "release" channels.
While a channel is "released", hUGEDriver refrains from touching it at all—which leaves it free for other code to use (say, a SFX engine, or [a sample player](https://github.com/DevEd2/SamplePlayer)).

## How-to

A channel can be released by modifying a variable:

RGBDS (assembly)  | GBDK (C)
------------------|----------
`_hUGE_mute_mask` | `hUGE_mute_mask`

Bit 0 of the mask represents CH1, bit 1 represents CH2, bit 2 represents CH3, and bit 3 represents CH4.
For as long as a channel's bit is 0, that channel is released.

A convenience function, `hUGE_mute_channel`, is also provided:

<details><summary>RGBDS (assembly)</summary>

Call `hUGE_mute_channel` with `b` containing the channel's ID minus one (so between 0 and 3 inclusive), and `c` being `0` to release the channel, or `1` to unmute it.

</details>

<details><summary>GBDK (C)</summary>

Call `hUGE_mute_channel`; see `hUGEDriver.h` for the corresponding arguments.

</details>

## Notes

- **IMPORTANT** precaution for CH3: if you release this channel and modify wave RAM, you **must** inform hUGEDriver, by writing `hUGE_NO_WAVE` to `hUGE_current_wave`.
  This will tell hUGEDriver to reload wave RAM when the channel is un-released; otherwise, you may get corrupted notes.
- "Global" [effects](../hUGETracker/effect_reference.md) (5, 8, B, D, and F) still play, even if the channel they are attached to is released.
  Importantly, this means hUGEDriver *will* write to `NR50` (5) and `NR51` (8).
