#include "mmult_test.h"
#include <time.h>

// Naive Matrix Multiply in C.
int* multiplyMatrixMatrix(int* m1, int* m2, int n)
{
  int* m3 = (int*) malloc(n*n*sizeof(int));
  for (int i = 0; i < n; i++){
    for (int j = 0; j < n; j++){
      for (int k = 0; k < n; k++){
        m3[i*n+j] += m1[i*n+k] * m2[k*n+j];
      }
    }
  }
  return m3;
}

int main(){
  int NUM_ELEMS = 1 << 10; // 1024 to start. 
  int len = NUM_ELEMS*NUM_ELEMS;
  int* m3_dag  = (int*) malloc(NUM_ELEMS*NUM_ELEMS*sizeof(int));

  int* m1 = initRandomArrayi(len);
  int* m2 = initRandomArrayi(len);

  printf("Multiplying Matrix (C)\n");
  clock_t start_c = clock();
  int* m3_c = multiplyMatrixMatrix(m1,m2,NUM_ELEMS);
  clock_t end_c = clock();
  printf("Time %f (s)\n",1000.0 * (double) (end_c - start_c) / CLOCKS_PER_SEC);

  printf("Multiplying Matrix (DAG)\n");
  clock_t start_dag = clock();
  dag_multiplyMatrixMatrix(m3_dag,NUM_ELEMS,NUM_ELEMS,
                            m1,NUM_ELEMS,NUM_ELEMS,
                            m2,NUM_ELEMS,NUM_ELEMS);
  clock_t end_dag = clock();
  printf("Time %f (ss)\n",(double) (end_dag - start_dag) / CLOCKS_PER_SEC);


  verifyArrays("mmult",m3_c,m3_dag,len);
}
