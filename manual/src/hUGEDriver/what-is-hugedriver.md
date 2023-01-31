# What is hUGEDriver?

[hUGEDriver](https://github.com/SuperDisk/hUGEDriver) is a *driver* for hUGETracker.
What does that mean?

Well, think like a MP3 file: a program is required to play those back.
Similarly, hUGETracker "only" generates music data, and something has to interpret it to actually produce sound.

This is also why some call hUGEDriver a "player" for hUGETracker, though this is slightly inexact, because hUGEDriver is not a complete playback solution: it's meant to integrate with *your* code, that will decide what music must be played, when, and how.
For more information about that, please read the next chapter.

## Notes

**Make sure to always use the same version of hUGETracker and hUGEDriver together!!**
For example, you *cannot* use hUGETracker 1.0b10 with hUGEDriver 1.0b9.
At best you will get garbled output, at worst your ROM will crash.

hUGEDriver is the reference driver, meaning that it will always be kept up to date.
Other implementations are available, but they are not officially supported.

hUGETracker plays back your songs by actually embarking a copy of hUGEDriver, and emulating a Game Boy running hUGEDriver!
Doing it this way instead of interpreting the music data directly helps avoiding any discrepancies between playback in the tracker and on the console—if you find any, it's probably a bug, [please tell us](../contact.md)!
