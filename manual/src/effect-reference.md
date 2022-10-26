# Effect reference

The hUGETracker effect codes are intentionally similar to ProTracker and FastTracker's.
If you know those, then many of these effects will look familiar to you.

Effect | Name              | Description
-------|-------------------|------------
`0xy`  | Arpeggio          | On every tick switch between the playing note, <math><mi>note</mi> <mo>+</mo> <mi>x</mi></math>, and <math><mi>note</mi> <mo>+</mo> <mi>y</mi></math>, where <var>x</var> and <var>y</var> are values in semitones. Can be used to create "chords" or a strum effect.
`1xx`  | Portamento up     | Slide the pitch up by <var>xx</var> units every tick.
`2xx`  | Portamento down   | Slide the pitch down by <var>xx</var> units every tick.
`3xx`  | Tone portamento   | *Instead of playing the cell's note*, slide the pitch towards that note by <var>xx</var> units every tick. Stop when the note is reached. **This effect cannot be used in a cell with an instrument value.**
`4xy`  | Vibrato           | Rapidly switch between the playing note and <math><mi>note</mi> <mo>+</mo> <mi>y</mi></math>, at the rate of <var>x</var>, where <var>y</var> is a value in units. Valid values for <var>x</var> are 0, 1, 3, 7, and F. This is similar to arpeggio, except you can control the frequency, and the amount is specified in units rather than semitones.
`5xx`  | Set master volume | Set the [master volume control][NR50] of the Game Boy for the left and right speakers. Use the effect editor to create one of these effects. *Note that a volume of 0 is not completely silent!*
`6xx`  | Call routine      | Call a user-defined [routine](./routines.md). Will crash if an invalid routine is specified.
`7xx`  | Note delay        | Wait <var>xx</var> ticks before playing the note in this cell. *The note won't be played at all if `xx` is strictly greater than the tempo!*
`8xx`  | Set panning       | Set [which channels play on which speakers][NR51]. Use the effect editor to create one of these effects. Can also be used as a mute for a channel, by setting it to output on neither left nor right.
`9xx`  | Set duty cycle    | Select the duty cycle for either channel 1 or channel 2. If this effect appears on the noise or wave channels, it will affect channel 2. Valid values for <var>xx</var> are `00`, `40`, `80`, or `C0`. Under the hood, the <var>xx</var> value is written directly into CH1 or CH2's [length register][NR11], so you could theoretically achieve other effects than just duty cycle changing.
`Axy`  | Volume slide      | Slide the note's volume up by <var>x</var> units, and then down by <var>y</var> units. *This effect actually retriggers the note on each tick*, which might not be noticeable for instruments without length or envelope, but may sound bad if those are present. It is recommended to use either instrument envelopes, or the `C` command instead, if you can. **This effect does not work in the same cell as a note/instrument!**
`Bxx`  | Position jump     | Jump to order <var>xx</var>.
`Cxx`  | Set volume        | Set the volume of the channel to <var>xx</var>. **Must be accompanied by a note and instrument to work** (except on channel 3). Valid values range from `00` to `0F`.
`Dxx`  | Pattern break     | Jump to the next order, and start on row <var>xx</var>.
`Exx`  | Note cut          | Cut the note short after <var>xx</var> ticks.
`Fxx`  | Set tempo         | Set the number of ticks per row to <var>xx</var>. Can be used in an alternating fashion to create a swing beat.

[NR11]: https://eldred.fr/pandocs/Audio_Registers.html#ff11--nr11-channel-1-length-timer--duty-cycle
[NR50]: https://eldred.fr/pandocs/Audio_Registers.html#ff24--nr50-master-volume--vin-panning
[NR51]: https://eldred.fr/pandocs/Audio_Registers.html#ff25--nr51-sound-panning
