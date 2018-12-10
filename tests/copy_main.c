#include "copy_test.h"

void copy(int* m3, int* m1, int n)
{
  for (int i = 0; i < n; i++){
    m3[i] = m1[i];
  }
}

int main(){
  int NUM_ELEMS = 1024; // 1024 to start. 

  int* m3_dag  = (int*) malloc(NUM_ELEMS*sizeof(int));
  int* m3_c    = (int*) malloc(NUM_ELEMS*sizeof(int));

  int* m1 = initRandomArrayi(NUM_ELEMS);

  copy(m3_c,m1,NUM_ELEMS);
  dag_copy(m3_dag,m1,NUM_ELEMS);

  verifyArrays("copy",m3_c,m3_dag,NUM_ELEMS);
}
