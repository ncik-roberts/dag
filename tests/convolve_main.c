#include "convolve_test.h"
#include <sys/time.h>

// Naive Convolution in C (Square image)
// Mask Dim must == 3 (for now).
void convolve(float* result, float* source, float* mask, int source_dim, int mask_dim)
{
  int n = source_dim;
  for (int i = 1; i < source_dim-1; i++){
    for (int j = 1; j < source_dim-1; j++){
      //printf("i %d, j %d\n",i,j);
      float reduce_temp = 0.f; 

      int i0 = i-1; int i1 = i; int i2 = i + 1;
      int j0 = j-1; int j1 = j; int j2 = j + 1;

      reduce_temp += source[i0*n+j0] * mask[0];
      reduce_temp += source[i0*n+j1] * mask[1];
      reduce_temp += source[i0*n+j2] * mask[2];
      reduce_temp += source[i1*n+j0] * mask[3];
      reduce_temp += source[i1*n+j1] * mask[4];
      reduce_temp += source[i1*n+j2] * mask[5];
      reduce_temp += source[i2*n+j0] * mask[6];
      reduce_temp += source[i2*n+j1] * mask[7];
      reduce_temp += source[i2*n+j2] * mask[8];

      //printf("reduce: %f, idx %d\n",reduce_temp,i*n+j);
      result[i0*(n-2)+j0] = reduce_temp;
    }
  }
}

/*

// Box Blur =
//  {  0.111f, 0.111f, 0.111f, 
//     0.111f, 0.111f, 0.111f,
//     0.111f, 0.111f, 0.111f  };

// Gaussian Blur = 
// {  0.060f, 0.130f, 0.060f, 
//    0.130f, 0.250f, 0.130f,  
//    0.060f, 0.130f, 0.060f };

// Horiztonal Edge Detect = 
// {  1.f,  0.f, -1.f,
//    1.f,  0.f, -1.f,  
//    1.f,  0.f, -1.f  }

// Vertical Edge Detect = 
// {   1.f,  1.f ,  1.f,
//     0.f,  0.f ,  0.f,    
//    -1.f, -1.f , -1.f  };

// General Edge Detect =
// {  0.f,  1.f, 0.f, 
//    1.f, -8.f, 1.f, 
//    0.f,  1.f, 0.f  }; 

// Sharpen =
// {   0.f, -1.f,  0.f,
//    -1.f,  5.f, -1.f,
//     0.f, -1.f,  0.f  };

*/

static inline long minl(long a, long b){
  return a < b ? a : b;
}


int main(){
  int NUM_ELEMS = 1 << 11; // 1024 to start. 
  int NUM_RUNS = 20;
  int len = NUM_ELEMS*NUM_ELEMS;
  int sub_len = (NUM_ELEMS - 2) * (NUM_ELEMS - 2);

  struct timeval t0;
  struct timeval t1;
  long elapsed;
  long timecc = __LONG_MAX__;
  long timedd = __LONG_MAX__;

  printf("Allocating %d elements.\n",len);
  float* result = (float*) calloc(sub_len,sizeof(float));
  float* result_dag = (float*) calloc(sub_len,sizeof(float));
  float* to_convolve = (float*) calloc(len,sizeof(float));

  if (result == NULL || result_dag == NULL || to_convolve == NULL){
    printf("Malloc returned null. Terminating.\n");
    return -1;
  }

  randomizeFloatArray(to_convolve,len,0.0,500.0);
  float mask[] = { 0.111f, 0.111f, 0.111f,
                   0.111f, 0.111f, 0.111f,
                   0.111f, 0.111f, 0.111f };

  //printf("Convolving (C)\n");
  for (int i = 0; i < NUM_RUNS; i++){
    gettimeofday(&t0,NULL);
    convolve(result,to_convolve,mask,NUM_ELEMS,3);
    gettimeofday(&t1,NULL);
    elapsed = (t1.tv_sec-t0.tv_sec)*1000000 + t1.tv_usec-t0.tv_usec;
    timecc = minl(timecc,elapsed);

    //printf("Convolving (DAG)\n");
    gettimeofday(&t0,NULL);
    dag_convolve(result_dag,sub_len,
                to_convolve,len,
                mask,9,
                NUM_ELEMS);
    gettimeofday(&t1,NULL);
    elapsed = (t1.tv_sec-t0.tv_sec)*1000000 + t1.tv_usec-t0.tv_usec;
    timedd = minl(timedd,elapsed);
  }

  printf("Convolve (C)    \t%ld\n",timecc);
  printf("Convolve (DAG)  \t%ld\n",timedd);

  verifyFloatArrays("convolve",result,result_dag,sub_len,0); 
}
