def bsqrt(x, tol=0.0001):
    high = x
    low  = 0

    mid = (high+low)/2
    while (err := mid**2 - x) and abs(err) > tol:
        if err > 0: # mid > x
            high = mid
            mid = (mid+low)/2
        elif err < 0: # mid < x
            low = mid
            mid = (high+mid)/2


    return mid


for x in [1349**2, 5, 7]:
    a = bsqrt(x)
    b = x**.5
    print(f'bsqrt({x}) = {a}')
    print(f'sqrt({x}) = {b}')
    print(f'error: {abs(a - b)}')
    print(('-'*20) + '\n')



