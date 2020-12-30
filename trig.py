# Evaluate trigonometric functions using taylor polynomials
from math import pi
from math import factorial as f
import math

# sin(x)/0! + cos(x)x/1! - sin(x)x^2/2! - cos(x)x^3/3! + sin(x)x^4/4!...
# at x=0

def r(x):
    return round(x, 4)

def sin(x, acc=0.001):
    # todo: smarter (use trig identities sin(-x) = -sin(x) etc.)
    known = {
        0: 0,
        r(pi/2): 1,
        r(pi/4): (2**.5/2),
        r(3*pi/4): (2**.5/2)
    }
    if known.get(r(x)) is not None:
        return known[r(x)]

    c = pi/4 # closest known (NOTE: must modify if c != 0)

    est = 0
    d = 0
    while True:
        # written with eval instead of lambda's for debugging
        # will rewrite later maybe

        exp = 'sin(c)' if d%2==0 else 'cos(c)'

        if (d-2)%4==0 or (d-3)%4 == 0:
            exp = '-' + exp

        exp += '* (x-c)**d / f(d)'

        v = eval(exp)

        # print(f'{exp} = {v}')
        est += v

        # v can be considered error of previous term, (by alternating series)
        # check v != 0 since if c=0 then sin(0)=0 (but this is not error)
        if v != 0 and abs(v) < acc:
            print(f'break, degree {d} v={v}')
            break

        d += 1

    return est

def cos(x):
    return sin(x + pi/2)



tests = [
    (1, 0.001),
    (2, 0.001),
]

for t in tests:
    n, acc = t
    v = sin(n, acc=acc)
    err = abs(v - math.sin(n))
    print(f'sin({n}) = {r(v)} err = {err}')
    print('-'*50)
