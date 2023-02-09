# Effect reference

The hUGETracker effect codes are intentionally similar to ProTracker and FastTracker's.
If you know those, then many of these effects will look familiar to you.

Some things to keep in mind:
- Effects are only active on the row that they are on; if you want an effect to remain active for several rows, you must re-enter it on each one.
- Some effects mention "the playing note", which is the last note played on the channel thus far (possibly in the same row).
  For example:

  Note | Instr | Effect
  -----|-------|-------
  C-5  |  1    | ...
  ...  |  .    | 047

  ...the `047` arpeggio will apply with the `C-5` note as its base.

Without further ado, here is the list of effects supported by hUGETracker:

Effect | Name              | Description
-------|-------------------|------------
`0xy`  | Arpeggio          | While this effect is active, the player will cycle on each tick between the playing note, the note + <var>x</var> semitones, and the note + <var>y</var> semitones, in this order. Can be used to create "chords" or a strum effect.
`1xx`  | Portamento up     | Slide the pitch up by <var>xx</var> units every tick. If the row contains a note, then the effect is skipped on the row's first tick; *if the tempo is 1, this means the effect will not do anything at all*.
`2xx`  | Portamento down   | Same, but the pitch is slid down instead of up.
`3xx`  | Tone portamento   | *Instead of playing the cell's note*, slide the pitch towards that note by <var>xx</var> units every tick. Stops exactly at the note, though.
`4xy`  | Vibrato           | Every <var>x</var> + 1 ticks, switch between the playing note and note + <var>y</var> units. This is similar to arpeggio, except you can control the frequency, and the offset is specified in units rather than semitones.
`5xx`  | Set master volume | Set the [master volume control][NR50] of the Game Boy for the left and right speakers. Consider using the effect editor. *Note that a volume of 0 is not completely silent!*
`6xy`  | Call routine      | Call the [user-defined routine](./routines.md) number <var>y</var>.
`7xx`  | Note delay        | Wait <var>xx</var> ticks before playing the note in this cell. *If `xx` is strictly greater than the tempo, the note will not play at all!*
`8xx`  | Set panning       | Set [which channels play on which speakers][NR51]. Consider using the effect editor. Setting a channel to neither left nor right will mute it, but is not recommended[^nr51_mute].
`9xx`  | Change timbre    | *For pulse channels* (CH1 &amp; 2), this changes the duty cycle[^nrx1]; *for the wave channel* (CH3), this loads wave <var>xx</var>[^wave_retrig]; *for the noise channel* (CH4), this changes [the LFSR's width][NR43] (caution! [^lfsr_lockup]).
`Axy`  | Volume slide      | Slide the note's volume[^slide_base] up by <var>x</var> units, or down by <var>y</var> units (either <var>x</var> or <var>y</var> must be 0). *The active note will be retriggered on each tick*, which may sound bad if envelope and/or length are present. It is recommended to use instead either instrument envelopes, or the `C` effect, if possible. **This effect is not available on the wave channel (CH3)!**
`Bxx`  | Position jump     | Jump to order <var>xx</var>.
`Cxy`  | Set volume        | Set the volume of the channel to <var>y</var>, *and retrigger the active note*. If <var>x</var> is not 0, <var>x</var> will be written to [the envelope bits][NR12]. (To stop the envelope instead, effect `A` can be used; see below.)
`Dxx`  | Pattern break     | Jump to the next order, and start on row <var>xx</var>.
`Exx`  | Note cut          | Cut the note short after <var>xx</var> ticks; *the note won't be cut if <var>xx</var> is not strictly less than the tempo!*
`Fxx`  | Set tempo         | Set the number of ticks per row to <var>xx</var>. Can be used in an alternating fashion to create a swing beat.

[^nr51_mute]:
Muting a channel via NR51 may cause an audio pop, and also tends not to play nice with sound effect engines that don't override NR51.
Further, since NR51 is a global effect, it's still applied even if the channel is "muted".

[^nrx1]:
<var>xx</var> is written directly to the channel's [length register][NR11]; if the active instrument has "length" enabled, the length will be reloaded immediately.

[^wave_retrig]:
Due to hardware limitations, changing the wave requires restarting the active note.

[^lfsr_lockup]:
Switching the LFSR from "long mode" to "short mode" at a certain time ["locks up" the noise channel](https://gbdev.io/pandocs/Audio_details.html#noise-channel-ch4), silencing it until it's retriggered.
This should happen consistently for an affected song, and may not appear in the tracker.

[^slide_base]:
Due to hardware limitations, `A` bases itself off of the instrument's *initial* volume, and doesn't take its envelope, if any, into account.

## Tips and tricks

- `A` stops the current instrument's envelope if one is active.
  `A00` is probably not desirable (see footnote above[^slide_base]), so consider using it in a subpattern, so that it is only active for a single tick.
- Using `C` causes a click, so prefer baking the volume into the instrument when possible.
- Vibrato using repeated `2xx` and `1xx` effects can give better results and more intricate/detailed vibrato than `4xy`, especially in [subpatterns](./subpatterns.md).
- Alternated `Fxx` effects can help with reaching decimal speeds. For example, alternating between `F04` and `F03` yields a speed of 3.5; cycling through `F01`, `F01`, `F02` and `F01` yields a speed of 1.25; and so on.
- Notes without an instrument on their row can help with reducing noise, as the note isn't retriggered.
  Of course, this only works if you weren't planning to change the instrument.

[NR11]: https://gbdev.io/pandocs/Audio_Registers.html#ff11--nr11-channel-1-length-timer--duty-cycle
[NR12]: https://gbdev.io/pandocs/Audio_Registers.html#ff12--nr12-channel-1-volume--envelope
[NR43]: https://gbdev.io/pandocs/Audio_Registers.html#ff22--nr43-channel-4-frequency--randomness
[NR50]: https://gbdev.io/pandocs/Audio_Registers.html#ff24--nr50-master-volume--vin-panning
[NR51]: https://gbdev.io/pandocs/Audio_Registers.html#ff25--nr51-sound-panning
