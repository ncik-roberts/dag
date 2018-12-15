#include "thrust_scan_test.h"
#include <sys/time.h>

void cscan(int* scanned, int* input, int len){
  int total = 0;
  for (int i = 0; i < len; i++){
    scanned[i] = total;
    total += input[i];
  }
}


int main(){
  int NUM_ELEMS = 1 << 26;
  int NUM_RUNS = 5;
  int* input = initRandomArrayiRange(NUM_ELEMS,0,5);
  int* result_c = (int*) malloc (NUM_ELEMS * sizeof(int));
  int* result_d = (int*) malloc (NUM_ELEMS * sizeof(int));

  struct timeval t0;
  struct timeval t1;
  long elapsed;

  long timemmd = __LONG_MAX__;
  long timemmc = __LONG_MAX__;

  gettimeofday(&t0,NULL);
  cscan(result_c,input,NUM_ELEMS);
  gettimeofday(&t1,NULL);
  elapsed = (t1.tv_sec-t0.tv_sec)*1000000 + t1.tv_usec-t0.tv_usec;
  timemmc = minl(elapsed,timemmc);

  for (int i = 0; i < NUM_RUNS; i++){
    gettimeofday(&t0,NULL);
    dag_thrust_scan(result_d,NUM_ELEMS,input,NUM_ELEMS);
    gettimeofday(&t1,NULL);
    elapsed = (t1.tv_sec-t0.tv_sec)*1000000 + t1.tv_usec-t0.tv_usec;
    timemmd = minl(elapsed,timemmd);
  }

  printf("Scan  (C)   \t%ld\n",timemmc);
  printf("Scan (DAG)  \t%ld\n",timemmd);

  verifyArrays("thrust_scan",result_c,result_d,NUM_ELEMS);
}