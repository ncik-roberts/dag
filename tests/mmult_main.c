#include "mmult_test.h"
#include <sys/time.h>

// Naive Matrix Multiply in C.
void multiplyMatrixMatrix(int* m3, int* m1, int* m2, int n)
{
  for (int i = 0; i < n; i++){
    for (int j = 0; j < n; j++){
      int temp = 0;
      for (int k = 0; k < n; k++){
        temp += m1[i*n+k] * m2[k*n+j];
      }
      m3[i*n+j] = temp;
    }
  }
}

int main(){
  int NUM_ELEMS = 1 << 10; // 1024 to start. 
  int NUM_RUNS = 3;

  int len = NUM_ELEMS*NUM_ELEMS;
  int* m3_dag  = (int*) malloc(len*sizeof(int));
  int* m3_c = (int*) malloc(len*sizeof(int));

  long timemmd = __LONG_MAX__;
  long timemmc = __LONG_MAX__;

  int* m1 = initRandomArrayiRange(len,0,100);
  int* m2 = initRandomArrayiRange(len,0,100);

  struct timeval t0;
  struct timeval t1;
  long elapsed;

    gettimeofday(&t0,NULL);
    multiplyMatrixMatrix(m3_c,m1,m2,NUM_ELEMS);
    gettimeofday(&t1,NULL);
    elapsed = (t1.tv_sec-t0.tv_sec)*1000000 + t1.tv_usec-t0.tv_usec;
    timemmc = minl(elapsed,timemmc);

  for (int i = 0; i < NUM_RUNS; i++){

    gettimeofday(&t0,NULL);
    dag_multiplyMatrixMatrix(m3_dag,NUM_ELEMS,NUM_ELEMS,
                                 m1,NUM_ELEMS,NUM_ELEMS,
                                 m2,NUM_ELEMS,NUM_ELEMS);
    gettimeofday(&t1,NULL);
    elapsed = (t1.tv_sec-t0.tv_sec)*1000000 + t1.tv_usec-t0.tv_usec;
    timemmd = minl(elapsed,timemmd);
  }

  printf("Matmult  (C)    \t%ld\n",timemmc);
  printf("Matumult (DAG)  \t%ld\n",timemmd);

  verifyArrays("mmult",m3_c,m3_dag,len);
}
