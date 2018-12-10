#include <stdlib.h>
#include <stdio.h>

int* initRandomArrayi(int NUM_ELEMS)
{
  int* array = (int*) malloc(NUM_ELEMS * sizeof (int));
  for (int i=0; i<NUM_ELEMS; i++){
    array[i] = rand();
  }
  return array;
}

float* initRandomArrayf(int NUM_ELEMS)
{
  float* array = (float*) malloc(NUM_ELEMS * sizeof (float));
  for (int i=0; i<NUM_ELEMS; i++){
    array[i] = (float) rand();
  }
  return array;
}


// Useful in a variety of testing scenarios.
void verifyArrays(const char* name, int* standard, int* test, int len){
  int passed = 1;
  for (int i = 0; i < len; i++){
      if (standard[i] != test[i]){
        printf("(%s) MISMATCH: Expected %d, Got %d\n",name,standard[i],test[i]);
        passed = 0;
      }
  }

  if (passed){
    printf("(%s) Test Passed.\n", name);
  } else{
    printf("(%s) Test FAILED.\n", name);
  }
}

int almostEquals(float a, float b){
  return abs(b - a) < 1e-4;
}

void verifyFloatArrays(const char* name, float* standard, float* test, int len){
  int passed = 1;
  for (int i = 0; i < len; i++){
      if (!almostEquals(standard[i],test[i])){
        printf("(%s) MISMATCH: Expected %f, Got %f\n",name,standard[i],test[i]);
        passed = 0;
      }
  }

  if (passed){
    printf("(%s) Test Passed.\n", name);
  } else{
    printf("(%s) Test FAILED.\n", name);
  }
}

void verifyInt(const char* name, int a, int b){
  if (a != b){
    printf("%s) mismatch, expected %d, got %d\n",name,a,b);
    return;
  }
}

void verifyFloat(const char* name, float a, float b){
  if (!almostEquals(a,b)){
    printf("%s) mismatch, expected %f, got %f\n",name,a,b);
    return;
  }
}

