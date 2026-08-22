# hUGETracker .UGE v5/v6/v7 format spec
## Data types

| Name          | Byte length | Description                                                                                                                    |
|---------------|-------------|--------------------------------------------------------------------------------------------------------------------------------|
| `uint8`       | 1           | Also known as char, ranges from 0 to 255.                                                                                      |
| `uint32`      | 4           | Also known as word, ranges from 0 to 4,294,967,295.                                                                            |
| `int8`        | 1           | Ranges from -127 to 127.                                                                                                       |
| `bool`        | 1           | If not-zero, then True.                                                                                                        |
| `shortstring` | 256         | Consists of a byte defining the readable length and then 255 characters.                                                       |
| `string`      | -           | Consists of a uint32 defining the number of characters, and then a stream of characters, with 0x00 being the terminator value. |
|               |             |                                                                                                                                |

All types are little endian unless noted otherwise.

## Header
 - `uint32` Version number
 - `shortstring` Song name
 - `shortstring` Song artist
 - `shortstring` Song comment

## Duty Instruments

 - Repeat 15 times:
     - `uint32` Type (0)
     - `shortstring` Instrument name
     - `uint32` Length
     - `bool` Length enabled
     - `uint8` Initial volume
     - `uint32` Volume sweep direction (0 = Increase, 1 = Decrease)
     - `uint8` Volume sweep change
     - `uint32` Frequency sweep time
     - `uint32` Sweep enabled (1 = Enabled, 0 = Disabled)
     - `uint32` Frequency sweep shift
     - `uint8` Duty cycle
     - `uint32` Unused
     - `uint32` Unused
     - If `Version number` < 6:
         - `uint32` Unused
     - `uint32` Unused
     - If `Version number` < 6:
         - `uint32` Unused
     - Else:
         - `bool` Subpattern enabled
             - Repeat 64 times:
                 - `uint32` Row note (0 through 72, 90 if not used)
                 - `uint32` Unused
                 - `uint32` Jump command value (0 if empty)
                 - `uint32` Effect code
                 - `uint8` Effect parameter
     - If `Version number` >= 4 and `Version number` < 6:
         - Repeat 6 times:
             - `int8` unused

## Wave Instruments

 - Repeat 15 times:
     - `uint32` Type (1)
     - `shortstring` Instrument name
     - `uint32` Length
     - `bool` Length enabled
     - `uint8` Unused
     - `uint32` Unused
     - `uint8` Unused
     - `uint32` Unused
     - `uint32` Unused
     - `uint32` Unused
     - `uint8` Unused
     - `uint32` Volume
     - `uint32` Wave index
     - If `Version number` < 6:
         - `uint32` Unused
     - `uint32` Unused
     - If `Version number` < 6:
         - `uint32` Unused
     - Else:
         - `bool` Subpattern enabled
             - Repeat 64 times:
                 - `uint32` Row note (0 through 72, 90 if not used)
                 - `uint32` Unused
                 - `uint32` Jump command value (0 if empty)
                 - `uint32` Effect code
                 - `uint8` Effect parameter
     - If `Version number` >= 4 and `Version number` < 6:
         - Repeat 6 times:
             - `int8` Unused

## Noise Instruments

 - Repeat 15 times:
     - `uint32` Type (2)
     - `shortstring` Instrument name
     - `uint32` Length
     - `bool` Length enabled
     - `uint8` Initial volume
     - `uint32` Volume sweep direction (0 = Increase, 1 = Decrease)
     - `uint8` Volume sweep change
     - `uint32` Unused
     - `uint32` Unused
     - `uint32` Unused
     - `uint8` Unused
     - `uint32` Unused
     - `uint32` Unused
     - If `Version number` < 6:
         - `uint32` Unused
     - `uint32` Noise mode (0 = 15 bit, 1 = 7 bit)
     - If `Version number` < 6:
         - `uint32` Unused
     - Else:
         - `bool` Subpattern enabled
             - Repeat 64 times:
                 - `uint32` Row note (0 through 72, 90 if not used)
                 - `uint32` Unused
                 - `uint32` Jump command value (0 if empty)
                 - `uint32` Effect code
                 - `uint8` Effect parameter
     - If `Version number` >= 4 and `Version number` < 6:
         - Repeat 6 times:
             - `int8` Noise macro data

## Wavetable data
 - Repeat 16 times:
     - Repeat 32 times:
         - `uint8` Wavetable nibble data
     - If `Version Number` < 3:
         - `uint8` Off by one filler

## Song Patterns
 - If `Version number` < 7:
     - `uint32` Initial ticks per row
 - Else:
     - Repeat 4 times (Duty 1, Duty 2, Wave, Noise):
         - `uint32` Initial ticks per row
 - If `Version number` >= 6:
     - `bool` Timer based tempo enabled
     - `uint32` Timer based tempo divider
 - `uint32` Number of song patterns
 - Repeat `Number of song patterns` times:
     - `uint32` Pattern index
     - Repeat 64 times:
         - `uint32` Row note (0 through 72, 90 if not used)
         - `uint32` Instrument value (0 if not used)
         - If `Version number` >= 6:
             - `uint32` Unused
         - `uint32` Effect code
         - `uint8` Effect parameter

## Pattern Sets and Song Order

For version 7 and later:

 - `uint32` Number of pattern sets
 - Repeat `Number of pattern sets` times:
     - `uint32` User-visible pattern index
     - Repeat 4 times (Duty 1, Duty 2, Wave, Noise):
         - `uint32` Song pattern index
 - `uint32` Order length
 - Repeat `Order length` times:
     - `uint32` User-visible pattern index

For version 6 and earlier:

 - Repeat 4 times (Duty 1, Duty 2, Wave, Noise):
     - `uint32` Order length + 1 (off-by-one bug)
     - Repeat `Order length` times:
         - `uint32` Song pattern index
     - `uint32` Off-by-one filler

## Routines
 - Repeat 16 times:
     - `string` Routine code data
