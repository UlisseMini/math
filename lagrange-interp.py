from collections import namedtuple

def prod(xs):
    acc = 1
    for x in xs: acc *= x
    return acc


P = namedtuple('Point', 'x y')
T = namedtuple('Term', 'y div factors')

points = [P(1,2), P(2,3), P(4,11)]

terms = []

for i, point in enumerate(points):
    excl = points[i+1:] + points[:i]
    div = prod(point.x - other.x for other in excl)
    terms.append(T(y=point.y, div=div, factors=[o.x for o in excl]))


print('\n'.join(map(repr, terms)))

def f(terms, x):
    acc = 0
    for t in terms:
        acc += t.y * prod(x - f for f in t.factors) / t.div

    return acc


for p in points:
    assert f(terms, p.x) == p.y

# TODO: Compress terms into a single polynomial.

