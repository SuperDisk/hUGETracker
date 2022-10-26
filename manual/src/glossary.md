# Glossary

Term       | Definition
-----------|------------
Cell             | A note, an instrument, and an effect. This is a single row in a **pattern**.
Channel          | One of the Game Boy's 4 voices for producing sound. There are 2 pulse channels, one wave channel, and one noise channel.
Duty             | The parameter that determines the waveform of a pulse channel. A pulse channel has two states (on or off), and the [duty cycle](https://en.wikipedia.org/wiki/Duty_cycle) specifies what percentage of the time it's on. A pulse channel with 50% duty would emit a square wave.
Effect           | Consists of an **effect code** and **effect parameter**. Used for a variety of reasons, including changing the way a specific note sounds, changing global settings such as master volume, affecting song **tempo**, or calling your own custom code.
Effect code      | A hexadecimal digit which specifies which **effect** to use.
Effect parameter | Two hexadecimal digits which tweak the **effect**'s behavior.
Instrument       | A bunch of parameters which change the way a **channel** produces sound. Each **cell** must include an instrument number.
Octave offset    | When entering **note** values into the **tracker grid**, the value of the note is increased by <math><mn>12</mn> <mo>×</mo> <mi>octave offset</mi></math>, to allow for more natural entry of higher notes.
Order            | A row of four **pattern** indices. An **order** is how you arrange **patterns** into a structured **song**.
Order table      | A list of **orders**, representing the structure of the **song**.
Pattern          | A list of 64 **cells**, used to represent 2 measures of music. This is the basic building block of your **song**.
Render           | Exporting a **song** as a `.wav` or `.mp3` file, so anybody can listen to them without hUGETracker or an emulator installed.
Routine          | A custom **effect** written in [Game Boy assembly](https://eldred.fr/gb-asm-tutorial). An advanced featuer that would typically be used when integrating hUGETracker into a homebrew game, or perhaps for making custom effects.
Song             | The whole track, which includes **patterns**, **orders**, **instruments**, **waves**, and **routines**.
Sweep            | A change of pitch over time. The Game Boy sound hardware provides the ability for the first pulse **channel** to perform a sweep as specified by some **instrument** parameters.
Tick             | Every time the sound driver update function is called, it advances the **song** by one **tick**. This is usually done at around 60 Hz.
Tempo            | The tempo of a song specifies how many **ticks** have to elapse before switching to the next row. The greater it is, the slower the song is.
Wave             | A waveform which changes the timbre of the wave channel when selected. You can draw these in the wave tab. They must be associated to an **instrument** in the instruments tab.
