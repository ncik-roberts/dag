#include "sanity_test.h"
#include <sys/time.h>

// Tests a variety of simple dag functions.
// 
// These are all 1D arrays with very limited translation options.
// They contain both int and float varieties, to verify that both
// int and float arithmetic is working out.
// 
// We increase the size accordingly to run a larger benchmark.

int* saxpy_int(int* result, int* a,int* b,int* c, int len){
  for (int i = 0; i < len; i++){
    result[i] = (a[i]*b[i]) + c[i];
  }
  return result;
}

float* saxpy_float(float* result, float* a,float* b,float* c, int len){
  for (int i = 0; i < len; i++){
    result[i] = (a[i]*b[i]) + c[i];
  }
  return result;
}

int sum_int(int* a, int* b, int len){
  int result = 0;
  for (int i = 0; i < len; i++){
    result += a[i] + b[i];
  }
  return result;
}

float sum_float(float* a, float* b, int len){
  float result = 0;
  for (int i = 0; i < len; i++){
    result += a[i] + b[i];
  }
  return result;
}

static inline long minl(long a, long b){
  return a < b ? a : b;
}

int main(){
  int NUM_ELEMS = 1 << 22; // A solid million.
  int NUM_RUNS = 5; // Normalize the timing a bit.

  struct timeval t0;
  struct timeval t1;
  long elapsed;

  long timesaxic = __LONG_MAX__;
  long timesaxfc = __LONG_MAX__;
  long timesaxid = __LONG_MAX__;
  long timesaxfd = __LONG_MAX__;
  long timesumic = __LONG_MAX__;
  long timesumfc = __LONG_MAX__;
  long timesumid = __LONG_MAX__;
  long timesumfd = __LONG_MAX__;

  int* iarray1 = initRandomArrayiRange(NUM_ELEMS,0,5);
  int* iarray2 = initRandomArrayiRange(NUM_ELEMS,0,5);
  int* iarray3 = initRandomArrayiRange(NUM_ELEMS,0,5);
  float* farray1 = initRandomArrayfRange(NUM_ELEMS,0.f,5.f);
  float* farray2 = initRandomArrayfRange(NUM_ELEMS,0.f,5.f);
  float* farray3 = initRandomArrayfRange(NUM_ELEMS,0.f,5.f);


  int* saxpyDAGi = (int*) calloc(NUM_ELEMS,sizeof(int));
  float* saxpyDAGf = (float*) calloc(NUM_ELEMS,sizeof(float)); 
  int* saxpyCi = (int*) malloc(NUM_ELEMS * sizeof(int));  
  float* saxpyCf = (float*) malloc(NUM_ELEMS * sizeof(float));

  if(iarray1 == NULL || iarray2 == NULL || iarray3 == NULL || 
     farray1 == NULL || farray2 == NULL || farray3 == NULL || 
     saxpyDAGi == NULL || saxpyDAGf == NULL || saxpyCi == NULL || 
     saxpyCf == NULL){
       printf("Malloc returned null. Try fewer elements.\n");
       return -1;
     }

  int sumCi; int sumDAGi;
  int sumCf; int sumDAGf;

  // Warm up the GPU before benchmark.
  dag_saxpy(saxpyDAGi,NUM_ELEMS,
              iarray1,NUM_ELEMS,
              iarray2,NUM_ELEMS,
              iarray3,NUM_ELEMS);

  for (int i = 0; i < NUM_RUNS; i++){
    gettimeofday(&t0,NULL);
    saxpy_int(saxpyCi,iarray1,iarray2,iarray3,NUM_ELEMS);
    gettimeofday(&t1,NULL);
    elapsed = (t1.tv_sec-t0.tv_sec)*1000000 + t1.tv_usec-t0.tv_usec;
    timesaxic = minl(elapsed,timesaxic);

    gettimeofday(&t0,NULL);
    dag_saxpy(saxpyDAGi,NUM_ELEMS,
                  iarray1,NUM_ELEMS,
                  iarray2,NUM_ELEMS,
                  iarray3,NUM_ELEMS);
    gettimeofday(&t1,NULL);
    elapsed = (t1.tv_sec-t0.tv_sec)*1000000 + t1.tv_usec-t0.tv_usec;
    timesaxid = minl(elapsed,timesaxid);

    gettimeofday(&t0,NULL);
    saxpy_float(saxpyCf,farray1,farray2,farray3,NUM_ELEMS);
    gettimeofday(&t1,NULL);
    elapsed = (t1.tv_sec-t0.tv_sec)*1000000 + t1.tv_usec-t0.tv_usec;
    timesaxfc = minl(elapsed,timesaxfc);

    gettimeofday(&t0,NULL);              
    dag_saxpy_float(saxpyDAGf,NUM_ELEMS,
                    farray1,NUM_ELEMS,
                    farray2,NUM_ELEMS,
                    farray3,NUM_ELEMS);
    gettimeofday(&t1,NULL);
    elapsed = (t1.tv_sec-t0.tv_sec)*1000000 + t1.tv_usec-t0.tv_usec;
    timesaxfd = minl(elapsed,timesaxfd);

    gettimeofday(&t0,NULL);  
    sumCi = sum_int(iarray1,iarray2,NUM_ELEMS);
    gettimeofday(&t1,NULL);
    elapsed = (t1.tv_sec-t0.tv_sec)*1000000 + t1.tv_usec-t0.tv_usec;
    timesumic = minl(elapsed,timesumic);

    gettimeofday(&t0,NULL);  
    sumDAGi = dag_sum(iarray1,NUM_ELEMS,iarray2,NUM_ELEMS);
    gettimeofday(&t1,NULL);
    elapsed = (t1.tv_sec-t0.tv_sec)*1000000 + t1.tv_usec-t0.tv_usec;
    timesumid = minl(elapsed,timesumid);

    gettimeofday(&t0,NULL);  
    sumCf = sum_float(farray1,farray2,NUM_ELEMS);
    gettimeofday(&t1,NULL);
    elapsed = (t1.tv_sec-t0.tv_sec)*1000000 + t1.tv_usec-t0.tv_usec;
    timesumfc = minl(elapsed,timesumfc);
  
    gettimeofday(&t0,NULL); 
    sumDAGf = dag_sum_float(farray1,NUM_ELEMS,farray2,NUM_ELEMS);
    gettimeofday(&t1,NULL);
    elapsed = (t1.tv_sec-t0.tv_sec)*1000000 + t1.tv_usec-t0.tv_usec;
    timesumfd = minl(elapsed,timesumfd);
  }

  printf("Saxpy Int (C)    \t%ld\n",timesaxic);
  printf("Saxpy Int (DAG)  \t%ld\n",timesaxid);
  printf("Saxpy Float (C)  \t%ld\n",timesaxfc);
  printf("Saxpy Float (DAG)\t%ld\n",timesaxfd);
  printf("Sum Int (C)      \t%ld\n",timesumic);
  printf("Sum Int (DAG)    \t%ld\n",timesumid);
  printf("Sum Float (C)    \t%ld\n",timesumfc);
  printf("Sum Float (DAG)  \t%ld\n",timesumfd);

  verifyArrays("saxpy",saxpyCi,saxpyDAGi,NUM_ELEMS);
  verifyFloatArrays("saxpyf",saxpyCf,saxpyDAGf,NUM_ELEMS);
  verifyInt("sum",sumCi,sumDAGi);
  verifyFloat("sumf",sumCf,sumDAGf);

}