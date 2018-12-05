#include <mmult_test.h>


// Naive Matrix Multiply in C.
int* multiplyMatrixMatrix_C(int* m1, int* m2, int n)
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

  int* m1 = (int*) malloc(len*sizeof(int));
  int* m2 = (int*) malloc(len*sizeof(int));

  // Allocate random elements into the array.
  for (int i = 0; i < len; i++){
      m1[i] = rand();
      m2[i] = rand();
  }

  int* m3_c = multiplyMatrixMatrix_C(m1,m2,NUM_ELEMS);

  dag_nd_array_t* dag_m1 = allocateSquareMatrix(2,NUM_ELEMS,(void*) m1);
  dag_nd_array_t* dag_m2 = allocateSquareMatrix(2,NUM_ELEMS,(void*) m2);
  dag_nd_array_t* result = multiplyMatrixMatrix_DAG(dag_m1,dag_m2);
  int* m3_dag = (int*) result->data;

  verifyArrays(m3_c,m3_dag,len);
}