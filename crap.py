'''def saw(t):
    return t & 255

def saw2(t):
    return t & 31

for i in range(0,64,1):
    print(saw2(i))
'''

def percentage(n):
    x = 0
    for i in range(0, 5):
        if (i & n) == 0:
            x += 1
    #print(n, x/100, sep='\t')
    return x/5

d = {}
for i in range(16):
    p = percentage(i)
    if p not in d:
        d[p] = bin(i) + ", " + hex(i)

for item in d.items():
    print(item)
