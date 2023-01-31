# Subpatterns

Subpatterns are one of hUGETracker's most powerful features.
They are like mini-tracker grids attached to instruments, which allow you to modulate them quite precisely.
They fulfill the same role as instrument macros in FamiTracker.

What are subpatterns useful for?
- Percussion! (Subpatterns replace noise macros)
- Cooler arpeggios than [effect](./effect-reference.md) `0`!
- Cooler vibratos than effect `4`!
- "Pluck" sounds! (Start one octave up, then go back to base)
- Serves as an extra FX channel in a pinch!
- Changing timbre really quickly! (Use effect `9`)

**TODO: insert screenshot**

Rows are composed of three columns, similiar *but not identical* to the tracker grid's:
- an offset,
- an optional jump to a specific row,
- and an effect.

The offset is a positive number of semitones that is applied to the base note (though only for this row).

Subpatterns automatically loop back to the beginning, or can be made to jump backwards earlier via the jump column.
If you do not want this, you can make a row that jumps to itself.

## Interaction with the "regular" tracker grid

Subpatterns override effects `0`, `1`, `2`, `3` (which will instead jump directly to the target note), and `4`.

## Effects

Effects in subpatterns work just like in the regular tracker grid ([reference](./effect-reference.md)), except for two things:
- each effect only lasts for a single tick (just like the rows);
- not all effects can be used in subpatterns:

<style>.y { color: green; } .n { color: red; }</style>

Effect | Name              | Usable?              | Notes
-------|-------------------|----------------------|-------
`0xy`  | Arpeggio          | <b class="y">Yes</b> | Technically you can use this, but 1) you would have to use it on many rows consecutively, and 2) you can just use the offset column instead.
`1xx`  | Portamento up     | <b class="y">Yes</b> | The "units" are finer than semitones, so this can be used to increase granularity; it is reset on the following row, however.
`2xx`  | Portamento down   | <b class="y">Yes</b> | Same as above.
`3xx`  | Tone portamento   | <b class="n">No</b>  | There is no "target note" in the first place.
`4xy`  | Vibrato           | <b class="y">Yes</b> | This can give finer control than the offset column, but you will have to use this effect on many consecutive rows, and the portamento effects may work better in the first place.
`5xx`  | Set master volume | <b class="y">Yes</b> |
`6xy`  | Call routine      | <b class="y">Yes</b> |
`7xx`  | Note delay        | <b class="n">No</b>  | There is no note in the first place.
`8xx`  | Set panning       | <b class="y">Yes</b> |
`9xx`  | Change timbre     | <b class="y">Yes</b> |
`Axy`  | Volume slide      | <b class="y">Yes</b> | `C` may be more appropriate, since this will only be active for a single tick.
`Bxx`  | Position jump     | <b class="n">No</b>  | Use the jump column instead.
`Cxy`  | Set volume        | <b class="y">Yes</b> |
`Dxx`  | Pattern break     | <b class="n">No</b>  | Use the jump column instead.
`Exx`  | Note cut          | <b class="n">No</b>  | Use `C00` instead.
`Fxx`  | Set tempo         | <b class="y">Yes</b> | ...but why would you do this?
