// Find gcd using euclid's algorithm.
// let a>b, gcd(a,b) = gcd(a-b, b)
// we apply this fact recursively until gcd(a,a) = a
// (I've added some optimization by using floored division)
function gcd(a, b) {
  console.log(`gcd(${a}, ${b})`)
  while (a !== b && a !== 0 && b !== 0) {
    if (a > b) { a -= ~~(a/b) * b }
    if (b > a) { b -= ~~(b/a) * a }
  }

  console.log(`= ${Math.max(a,b)}`)
  return Math.max(a,b)
}

gcd(11*3 * 2**27, 9)


