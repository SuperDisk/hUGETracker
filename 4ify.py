file = 'test_wave'

orig = open(f'{file}.raw', 'rb').read()
newf = open(f'{file}2.raw', 'wb')
destf = open(f'{file}3.raw', 'wb')

ls1 = []
for byte in orig:
    new = int((byte/255)*15)
    ls1.append(new)
    newf.write(bytes([new]))

for pair in zip(ls1[::2], ls1[1::2]):
    h, l = pair
    new = (h << 4) | l
    destf.write(bytes([new]))

newf.close()
destf.close()
