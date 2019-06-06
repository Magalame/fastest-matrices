//https://jemnz.com/matrixmultiply.html
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

void mmult(double *A, double *B, double *C, int N){

    int i, j, k;

    for (i = 0; i < N; i++){
        for (j = 0; j < N; j++) {
            C[i + j*N] = 0;
            for (k = 0; k < N; k++){
                C[i + j*N] += A[i + k*N]*B[k + N*j];
            }
        }
    }
 

}

int main ()
{
    time_t t;
    int N, col1, row2, col2;
    srand ((unsigned) time (&t));
    int i, j;
    N=100;
    double A[N*N];
    double B[N*N];

    for (i = 0; i < N*N; i++) {
        A[i] = rand();
    }

    for (j = 0; j < N*N; j++){
        B[i] = rand();
    }

    clock_t start = clock();
    double C[N*N];
    mmult(A,B,C,N);
    clock_t end = clock();

    printf("%3.20f ms\n", (end-start)/(double)CLOCKS_PER_SEC * 1000  );

    /* Printing the contents of third matrix. */

    return (0);
}
