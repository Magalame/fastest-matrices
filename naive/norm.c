// https://superstrings.io/once-more-on-haskell-vs-c-performance-a54498bfa91f
#include <time.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#define x_size 100*100

double dot(double *x, double *y, int n) {
  double theSum = 0.0;
  int i;
  for (i=0; i<n; i++) {
    theSum += x[i]*y[i];
  }
  return theSum;
}

int main() {
  double *x = malloc(x_size * sizeof(double));
  int i;
  for(i=0; i<x_size; i++) {
    x[i] = (double) 100 * rand() / (RAND_MAX + 1.);
  }
  double result = 0;
  clock_t start = clock();
  for (i = 0; i <100; i++)
    result = dot(x, x, x_size);
  clock_t end = clock();
  printf("%3.2f\n", result);
  printf("%3.20f us\n", (end-start)/(double)CLOCKS_PER_SEC * 10 * 1000);
  free(x);
  return 0;
}
