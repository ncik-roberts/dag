#include <mmult_test.h>


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

  int* m3_c = multiplyMatrixMatrix(m1,m2,NUM_ELEMS);

  dag_multiplyMatrixMatrix(m3_dag,NUM_ELEMS,NUM_ELEMS,
                            m1,NUM_ELEMS,NUM_ELEMS,
                            m2,NUM_ELEMS,NUM_ELEMS);

  verifyArrays("mmult",m3_c,m3_dag,len);
}