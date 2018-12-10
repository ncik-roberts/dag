#include <dag.h>

void saxpy_int_DAG(int* result, int result_len, int* a,int a_len, int* b, int b_len, int* c, int c_len);
void saxpy_float_DAG(int* result, int result_len, float* a,int a_len, float* b, int b_len, float* c, int c_len);
int sum_int_DAG(int* a,int a_len, int* b, int b_len);
float sum_float_DAG(float* a,int a_len, float* b, int b_len);