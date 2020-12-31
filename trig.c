#include <math.h>
#include <stdio.h>

#define DEG 6

int factorial(int n) {
  int r = 1;
  for (int i = n; i > 0; i--) {
    r *= i;
  }

  return r;
}

/* Yes, I'm serious */
float bsin(float x) {
  float sum = 0;
  for (int n = 0; n < DEG; n++) {
    float v = pow(-1, n) * pow(x, (2*n + 1))/factorial(2*n + 1);
    sum += v;
  }
  return sum;
}

void show(float x) {
  float v = bsin(x);
  printf("-------------------------------------------------\n");
  printf("bsin(%f): %f err: %f\n", x, v, v-sin(x));
}

int main() {
  printf("Degree %d\n", DEG);

  show(1.0);
  show(2.0);
  show(3.0);

  return 0;
}

