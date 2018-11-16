
 /*
  * This file is an attempt at producing what the generated target code 
  * should look like for the multiplyMatrixMatrix routine. This includes
  * a number of bits:
  * 
  *  - How to implement primitives (usually - NVIDIA examples)
  *  - Internal C++ representations (dag_array_t) ?
  *  - Interaction between host code and device code:
  *     -> Is it possible to transform each function at the call site 
  *        based off of how many nested parallel blocks it is in?
  *  - C++ results of the "<-" operator
  *  - Parallelizing inner loops (e.g: dot product)
  * 
  */

struct dag_array_t{
  size_t rows;
  size_t cols; 
  bool isMutable;
  int* matrix;
}

// Device code
__global__ void VecAdd(float* A, float* B, float* C, int N)
{
    int i = blockDim.x * blockIdx.x + threadIdx.x;
    if (i < N)
        C[i] = A[i] + B[i];
}
 
/* 
   DAG Primitive. Here, we leverage the NVIDIA developer examples 
   to obtain a high-bandwith operation. 

   TILE_DIM = 32
   BLOCK_ROWS = 8

   https://devblogs.nvidia.com/efficient-matrix-transpose-cuda-cc/
*/
const int tp_TILE_DIM = 32;
const int tp_BLOCK_ROWS = 8;

__global__ void transposeCoalesced(int *result, const float *in)
{
  int TILE_DIM = tp_TILE_DIM;
  int BLOCK_ROWS = tp_BLOCK_ROWS;

  __shared__ int tile[TILE_DIM][TILE_DIM];

  int x = blockIdx.x * TILE_DIM + threadIdx.x;
  int y = blockIdx.y * TILE_DIM + threadIdx.y;
  int width = gridDim.x * TILE_DIM;

  for (int j = 0; j < TILE_DIM; j += BLOCK_ROWS)
     tile[threadIdx.y+j][threadIdx.x] = in[(y+j)*width + x];

  __syncthreads();

  x = blockIdx.y * TILE_DIM + threadIdx.x;  // transpose block offset
  y = blockIdx.x * TILE_DIM + threadIdx.y;

  for (int j = 0; j < TILE_DIM; j += BLOCK_ROWS)
     result[(y+j)*width + x] = tile[threadIdx.x][threadIdx.y + j];
}

// Not parallel. I guess it could be.
__global__ int dotProduct(int* v1, int*v2, int n)
{
  int sum = 0;
  for (int i = 0; i < n; i++){
    sum += v1[n] * v2[n];
  }
  return sum;
}



// We use single-dimensional lists.
dag_array_t* matrixMultiply(dag_array_t* result, dag_array_t* m1, dag_array_t* m2){

    if (m1->cols != m2->rows){
      printf("Invalid Matmult Dimensions : (%u x %u) x (%u x %u)\n"
            m1->rows,m1->cols,m2->rows,m2->cols);
      assert(false);
    }
    // Precompute size information
    size_t size_m1 = m1->rows * m1->cols;
    size_t size_m2 = m2->rows * m2->cols;

    int* d_m2;  
    cudaMalloc(&d_m2,size_m2);
    int* d_col; // We know that transpose will return same # of elem.
    cudaMalloc(&d_col,size_m2)
    
    // A cruical optimization involves removing extraneous cudaMemcpy and cudaMallocs.

    dim3 dimGrid(m2->rows/tp_TILE_DIM, m2->cols/tp_TILE_DIM, 1);
    dim3 dimBlock(tp_TILE_DIM, tp_BLOCK_ROWS, 1);
    transposeCoalesced<<<dimGrid,dimBlock>>>(d_col,d_m2);

    int threadsPerBlock;
    int blocksPerGrid = (size_m1 + threadsPerBlock - 1) / threadsPerBlock;
}

/*
    // Allocate function parameters
    int* d_m1;
    cudaMalloc(&d_m1,m1->rows * m1->cols);
    int* d_m2;
    cudaMalloc(&d_m2,m2->rows * m2->cols);

    result->rows = m1->rows;
    result->cols = m2->cols;

    // Allocate result parameters
    int* d_result;
    cudaMalloc(&d_result,result->rows * result->cols);

    // Copy vectors from host memory to device memory
    cudaMemcpy(d_m1, m1->matrix, m1->rows * m1->cols, cudaMemcpyHostToDevice);
    cudaMemcpy(d_m2, m2->matrix, m2->rows * m2->cols, cudaMemcpyHostToDevice);

    // Invoke kernel
    int threadsPerBlock = 256; // Todo: Heuristic this thing
    int blocksPerGrid = (N + threadsPerBlock - 1) / threadsPerBlock;
    VecAdd<<<blocksPerGrid, threadsPerBlock>>>(d_m1, d_m2, d_C, N);

    // Copy result from device memory to host memory
    // h_C contains the result in host memory
    cudaMemcpy(result->matrix, d_C, size, cudaMemcpyDeviceToHost);
    // Free device memory
    cudaFree(d_A);
    cudaFree(d_B);
    cudaFree(d_C);

*/