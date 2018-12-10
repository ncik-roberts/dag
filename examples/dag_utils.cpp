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

static unsigned int
