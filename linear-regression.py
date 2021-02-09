import numpy as np
import matplotlib.pyplot as plt


# linear regression over points, via linear algebra
# https://youtu.be/Y_Ac6KiQ1t0
# basically we want to solve Ax = b where b is not in C(A)
# the vector in the column space of A closest to b is the projection
# of b unto C(A), since (b - p) is orthogonal to C(A), we know (b - p)
# must be in N(A^T), therefor A^T(b - p) = 0, since we're solving Ax=p now,
# we can rewrite this as A^T(b - Ax) = 0 or A^TAx = A^Tb (which is what I've used here)

X = [1, 2, 3, 4]
Y = [1, 2, 2, 3]

A = np.ones((len(X), 2))
A[:,0] = X

b = Y

x_hat = np.linalg.solve(A.T @ A, A.T @ b)
l_m, l_b = x_hat

print(f'm: {l_m:.3f} b: {l_b:.3f}')

plt.ylim([0, 5])
plt.xlim([0, 5])

plt.plot([0, 5], [l_b, l_m*5 + l_b])
plt.scatter(X, Y)
plt.show()
