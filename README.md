# UGE
Unnamed Gameboy Emulator

I'm dumping the source code of an old Gameboy emulator from 2000. It was written by Christian Hackbart and formerly located at http://www.tu-ilmenau.de/~hackbart but the site seems to have gone down long ago. So to preserve it, here it is now. My interest was piqued because despite the glut of Gameboy emulators out there for almost every language, Pascal doesn't seem to have a contender in the ring... except this one.

I ran the original Delphi source through Lazarus' converter tool and made a few tweaks so it would compile with Lazarus 32-bit. DirectDraw support is busted, so DIB is on by deafult. The emulator runs insanely fast, and actually crashes if you leave the sound option on, since (I think) the sound buffer fills up and it starts locking up the whole program.

My eventual goal is to fix the speed and sound issues, then frankenstein it into the sound engine for a Gameboy music tracker.
