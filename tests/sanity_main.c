#include "sanity_test.h"

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
  int NUM_ELEMS = 1 << 10; // (not) A solid million.

  int* iarray1 = initRandomArrayiRange(NUM_ELEMS,0,5);
  int* iarray2 = initRandomArrayiRange(NUM_ELEMS,0,5);
  int* iarray3 = initRandomArrayiRange(NUM_ELEMS,0,5);
  float* farray1 = initRandomArrayfRange(NUM_ELEMS,0.f,5.f);
  float* farray2 = initRandomArrayfRange(NUM_ELEMS,0.f,5.f);
  float* farray3 = initRandomArrayfRange(NUM_ELEMS,0.f,5.f);

  int* saxpyCi = saxpy_int(iarray1,iarray2,iarray3,NUM_ELEMS);
  float* saxpyCf = saxpy_float(farray1,farray2,farray3,NUM_ELEMS);
  int sumCi = sum_int(iarray1,iarray2,NUM_ELEMS);
  float sumCf = sum_float(farray1,farray2,NUM_ELEMS);

  int* saxpyDAGi = (int*) calloc(NUM_ELEMS,sizeof(int));
  dag_saxpy(saxpyDAGi,NUM_ELEMS,
                iarray1,NUM_ELEMS,
                iarray2,NUM_ELEMS,
                iarray3,NUM_ELEMS);

  float* saxpyDAGf = (float*) calloc(NUM_ELEMS,sizeof(float));                   
  dag_saxpy_float(saxpyDAGf,NUM_ELEMS,
                  farray1,NUM_ELEMS,
                  farray2,NUM_ELEMS,
                  farray3,NUM_ELEMS);

  int sumDAGi = dag_sum(iarray1,NUM_ELEMS,iarray2,NUM_ELEMS);
  float sumDAGf = dag_sum_float(farray1,NUM_ELEMS,farray2,NUM_ELEMS);

  verifyArrays("saxpy",saxpyCi,saxpyDAGi,NUM_ELEMS);
  verifyFloatArrays("saxpyf",saxpyCf,saxpyDAGf,NUM_ELEMS);
  verifyInt("sum",sumCi,sumDAGi);
  verifyFloat("sumf",sumCf,sumDAGf);

}