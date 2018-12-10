#include "convolve_test.h"
#include <time.h>

// Naive Convolution in C (Square image)
// Mask Dim must == 3 (for now).
void convolve(float* result, float* source, float* mask, int source_dim, int mask_dim)
{
  for (int i = 1; i < source_dim - 1; i++){
    for (int j = 1; j < source_dim -1; j++){
      float reduce_temp = 0.f;
      for (int k = -1; k <= 1; k++){
        for (int l = -1; l <= 1; l++){
          reduce_temp += source[(i+k)*source_dim + (j+l)] * mask[(k+1 * mask_dim) + l+1];
        }
      }
      result[i*source_dim+j] = reduce_temp;
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

int main(){
  int NUM_ELEMS = 1 << 10; // 1024 to start. 
  int len = NUM_ELEMS*NUM_ELEMS;

  float* result = (float*) calloc(len,sizeof(float));
  float* result_dag = (float*) calloc(len,sizeof(float));
  float* to_convolve = initRandomArrayfRange(len,0.0,500.0);
  float mask[] = { 0.111f, 0.111f, 0.111f,
                   0.111f, 0.111f, 0.111f,
                   0.111f, 0.111f, 0.111f };

  printf("Convolving (C)\n");
  clock_t start_c = clock();
  convolve(result,to_convolve,mask,NUM_ELEMS,3);
  clock_t end_c = clock();
  printf("Time %f (s)\n",1000.0 * (double) (end_c - start_c) / CLOCKS_PER_SEC);

  printf("Convolving (DAG)\n");
  clock_t start_dag = clock();
  dag_convolve(result_dag,NUM_ELEMS,NUM_ELEMS,
               to_convolve,NUM_ELEMS,NUM_ELEMS,
               mask,3,3);
  clock_t end_dag = clock();
  printf("Time %f (ss)\n",(double) (end_dag - start_dag) / CLOCKS_PER_SEC);

  verifyArrays("convolve",result,result_dag,len);
}
