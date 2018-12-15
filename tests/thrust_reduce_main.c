#include "thrust_reduce_test.h"
#include <sys/time.h>

int credx(int* input, int len){
  int total = 0;
  for (int i = 0; i < len; i++){
    total += input[i];
  }
  return total;
}


int main(){
  int NUM_ELEMS = 1 << 22;
  int NUM_RUNS = 5;
  int* input = initRandomArrayiRange(NUM_ELEMS,0,5);
  int result_c;
  int result_d;

  struct timeval t0;
  struct timeval t1;
  long elapsed;

  long timemmd = __LONG_MAX__;
  long timemmc = __LONG_MAX__;

  gettimeofday(&t0,NULL);
  result_c = credx(input,NUM_ELEMS);
  gettimeofday(&t1,NULL);
  elapsed = (t1.tv_sec-t0.tv_sec)*1000000 + t1.tv_usec-t0.tv_usec;
  timemmc = minl(elapsed,timemmc);

  for (int i = 0; i < NUM_RUNS; i++){
    gettimeofday(&t0,NULL);
    result_d = dag_thrust_reduce(input,NUM_ELEMS);
    gettimeofday(&t1,NULL);
    elapsed = (t1.tv_sec-t0.tv_sec)*1000000 + t1.tv_usec-t0.tv_usec;
    timemmd = minl(elapsed,timemmd);
  }

  printf("Reduce  (C)   \t%ld\n",timemmc);
  printf("Reduce (DAG)  \t%ld\n",timemmd);

  verifyInt("thrust_scan",result_c,result_d);
}