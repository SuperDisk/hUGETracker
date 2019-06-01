import wave
import sys

sys.argv.append('808nokickbreak.wav')

TARGET_RATE = int(8192)

with wave.open(sys.argv[1]) as w:
    nchannels, sample_width, framerate, nframes, _, _ = w.getparams()
    outdata = bytearray()
    gbdata = bytearray()

    frames_per_note = framerate // TARGET_RATE
    nybble = None
    while True:
        data = w.readframes(frames_per_note)
        if not data:
            break

        n = 0
        total = 0
        # Left and right channels are interleaved; this will pick up data from only channel 0
        for i in range(0, len(data), nchannels * sample_width):
            frame = int.from_bytes(data[i : i + sample_width], 'little', signed=True)
            n += 1
            total += frame

        # Crush the new sample to a nybble
        crushed_frame = int(total / n) >> (sample_width * 8 - 4)
        # Expand it back to the full sample size, to make a WAV simulating how it should sound
        encoded_crushed_frame = (crushed_frame << (sample_width * 8 - 4)).to_bytes(2, 'little', signed=True)
        outdata.extend(encoded_crushed_frame * (nchannels * frames_per_note))

        # Combine every two nybbles together.  The manual shows that the high nybble plays first.
        # WAV data is signed, but Game Boy nybbles are not, so add the rough midpoint of 7
        if nybble is None:
            nybble = crushed_frame + 7
        else:
            byte = (nybble << 4) | (crushed_frame + 7)
            #gbdata.append(byte)
            nybble = None

    with wave.open(f'{sys.argv[1]}crush.wav', 'wb') as wout:
        wout.setparams(w.getparams())
        wout.writeframes(outdata)

with open(f'{sys.argv[1]}crush.dat', 'wb') as f:
    f.write(gbdata)
