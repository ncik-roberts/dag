#include "copy_test.h"

int* copy(int* m1, int n)
{
  int* m3 = (int*) malloc(n*n*sizeof(int));
  for (int i = 0; i < n; i++){
    m3[i] = m1[i];
  }
  return m3;
}

int main(){
  int NUM_ELEMS = 1 << 10; // 1024 to start. 
  int len = NUM_ELEMS*NUM_ELEMS;
  int* m3_dag  = (int*) malloc(NUM_ELEMS*NUM_ELEMS*sizeof(int));

  int* m1 = initRandomArrayi(len);
  int* m2 = initRandomArrayi(len);

  int* m3_c = copy(m1,NUM_ELEMS);

  dag_copy(m3_dag,m1,NUM_ELEMS);

  verifyArrays("mmult",m3_c,m3_dag,len);
}
