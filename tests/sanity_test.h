#include "dag.h"

void dag_saxpy_int(int* result, int result_len, int* a,int a_len, int* b, int b_len, int* c, int c_len);
void dag_saxpy_float(int* result, int result_len, float* a,int a_len, float* b, int b_len, float* c, int c_len);
int dag_sum_int(int* a,int a_len, int* b, int b_len);
float dag_sum_float(float* a,int a_len, float* b, int b_len);