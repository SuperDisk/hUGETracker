orig = open('meaty.raw', 'rb').read()
newf = open('meaty2.raw', 'wb')

for byte in orig:
    new = int((byte/255)*15)
    newf.write(bytes([new]))

newf.close()
