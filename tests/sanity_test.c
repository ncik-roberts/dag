#include <sanity_test.h>

// Tests a variety of simple dag functions.
// 
// These are all 1D arrays with very limited translation options.
// They contain both int and float varieties, to verify that both
// int and float arithmetic is working out.
// 
// We increase the size accordingly to run a larger benchmark.

int* saxpy_int(int* a,int* b,int* c, int len){
  int* result = (int*) malloc(len * sizeof(int));
  for (int i = 0; i < len; i++){
    result[i] = a[i]*b[i] + c[i];
  }
  return result;
}

float* saxpy_float(float* a,float* b,float* c, int len){
  float* result = (float*) malloc(len * sizeof(float));
  for (int i = 0; i < len; i++){
    result[i] = a[i]*b[i] + c[i];
  }
  return result;
}

int sum_int(int* a, int* b, int len){
  int result = 0;
  for (int i = 0; i < len; i++){
    result += a[i] = b[i];
  }
  return result;
}

float sum_float(float* a, float* b, int len){
  float result = 0;
  for (int i = 0; i < len; i++){
    result += a[i] = b[i];
  }
  return result;
}


int main(){
  int NUM_ELEMS = 1 << 20; // A solid million.

  int* iarray1 = (int*) malloc(NUM_ELEMS * sizeof(int));
  int* iarray2 = (int*) malloc(NUM_ELEMS * sizeof(int));
  int* iarray3 = (int*) malloc(NUM_ELEMS * sizeof(int));
  float* farray1 = (int*) malloc(NUM_ELEMS * sizeof(float));
  float* farray2 = (int*) malloc(NUM_ELEMS * sizeof(float));
  float* farray3 = (int*) malloc(NUM_ELEMS * sizeof(float));

  for (int i = 0; i < NUM_ELEMS; i++){
    iarray1[i] = rand();
    iarray2[i] = rand();
    iarray3[i] = rand();
    farray1[i] = (float) rand();
    farray2[i] = (float) rand();
    farray3[i] = (float) rand();
  }

  dag_nd_array_t* dag_ia1 = allocateSquareMatrix(1,NUM_ELEMS,iarray1);
  dag_nd_array_t* dag_ia2 = allocateSquareMatrix(1,NUM_ELEMS,iarray2);
  dag_nd_array_t* dag_ia3 = allocateSquareMatrix(1,NUM_ELEMS,iarray3);
  dag_nd_array_t* dag_fa1 = allocateSquareMatrix(1,NUM_ELEMS,farray1);
  dag_nd_array_t* dag_fa2 = allocateSquareMatrix(1,NUM_ELEMS,farray2);
  dag_nd_array_t* dag_fa3 = allocateSquareMatrix(1,NUM_ELEMS,farray3);


  int* result_saxpy_i_C = saxpy_int(iarray1,iarray2,iarray3,NUM_ELEMS);
  float* result_saxpy_f_C = saxpy_float(farray1,farray2,farray3,NUM_ELEMS);
  dag_nd_array_t* result_saxpy_i_DAG = saxpy_int_DAG(dag_ia1,dag_ia2,dag_ia3);
  dag_nd_array_t* result_saxpy_f_DAG = saxpy_float_DAG(dag_fa1,dag_fa2,dag_fa3);

  int result_sum_i_DAG = sum_int_DAG(dag_ia1,dag_ia2);
  float result_sum_f_DAG = sum_float_DAG(dag_fa1,dag_fa2);
  int result_sum_i_C = sum_int(iarray1,iarray2,NUM_ELEMS);
  float result_sum_f_C = sum_float(farray1,farray2,NUM_ELEMS);

  verifyArrays(result_saxpy_i_DAG->data,result_saxpy_i_C,NUM_ELEMS);
  verifyFloatArrays(result_saxpy_f_DAG->data,result_saxpy_f_C,NUM_ELEMS);
  
  int passed = 1;
  if (result_sum_i_DAG != result_sum_i_C){
    printf("SUM mismatch, expected %d, got %d\n",result_saxpy_i_C,result_sum_i_DAG);
    passed = 0;
  }

  if (!almostEquals(result_sum_f_C,result_sum_f_DAG)){
    printf("SUM mismatch, expected %f, got %f\n",result_saxpy_f_C,result_sum_f_DAG);
    passed = 0;
  }

  if (passed){
    printf ("Test passed.\n");
  }


}