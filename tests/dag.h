#include <stdlib.h>
#include <stdio.h>

typedef struct{
  int dimension;
  int* lengths;
  void* data; // Polymorphic? 
} dag_nd_array_t;

dag_nd_array_t* allocateSquareMatrix(int dimension, int num_elems, void* data){
  dag_nd_array_t* matrix = (dag_nd_array_t *) malloc(sizeof(dag_nd_array_t));
  matrix->dimension = dimension;
  matrix->lengths = (int*) malloc(dimension * sizeof(int));
  for (int i = 0; i < dimension; i++){
    matrix->lengths[i] = num_elems;
  }
  matrix->data = data;
  return matrix;
}

// Useful in a variety of testing scenarios.
int verifyArrays(int* standard, int* test, int len){
  int passed = 1;
  for (int i = 0; i < len; i++){
      if (standard[i] != test[i]){
        printf("MISMATCH: Expected %d, Got %d\n",standard[i],test[i]);
        passed = 0;
      }
  }

  if (passed){
    printf("Test Passed.\n");
  } else{
    printf("TEST FAILED.\n");
  }
}

int almostEquals(float a, float b){
  return abs(b - a) < 1e-4;
}

int verifyFloatArrays(float* standard, float* test, int len){
  int passed = 1;
  for (int i = 0; i < len; i++){
      if (!almostEquals(standard[i],test[i])){
        printf("MISMATCH: Expected %d, Got %d\n",standard[i],test[i]);
        passed = 0;
      }
  }

  if (passed){
    printf("Test Passed.\n");
  } else{
    printf("TEST FAILED.\n");
  }
}


