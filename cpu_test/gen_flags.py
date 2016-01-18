#!/usr/bin/python3
s='ODITSZAPC'
p='ba9876420'

for comb in range(0, 2**9):
    x = comb
    i = 0
    c = ''
    v = 0
    while x != 0:
        f = x % 2
        if f == 1:
            c += s[i]
            v += 1<<int(p[i], 16)
        i += 1
        x = int(x/2)
    if c == '':
        continue
    print('Z%s = 0x%x' % (c, v))

