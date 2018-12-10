#include <cuda.h>
#include <cuda_runtime.h>
#include <math.h>
#include <algorithm>
#include <stdio.h>
#include <string>
#include <stdbool.h>
#include <thrust/host_vector.h>
#include <thrust/device_vector.h>
#include <thrust/execution_policy.h>
#include <thrust/extrema.h>
#include <vector>

static unsigned int _dag_ilog2(unsigned int x) {
  int result = 0;
  while (x >>= 1) result++;
  return result;
}

static int _dag_imin(int x, int y) {
  return x <= y ? x : y;
}

static int _dag_imax(int x, int y) {
  return x >= y ? x : y;
}

static float _dag_fmin(float x, float y) {
  return x <= y ? x : y;
}

static float _dag_fmax(float x, float y) {
  return x >= y ? x : y;
}
