#include <stdio.h>
 /*
  * This file is an attempt at producing what the generated target code 
  * should look like for the multiplyMatrixMatrix routine.
  */

/* Prototype matrix representation. */
struct dag_array_t{
  size_t rows;
  size_t cols; 
  int* matrix;
};

 /* 
   DAG Primitive. Here, we leverage the NVIDIA developer examples 
   to obtain a high-bandwith operation. They make use of shared memory
   to avoid strided global memory accesses, and instead perform the
   strided access in the shared block, which is roughly a ~3x improvement.

   TILE_DIM = 32
   BLOCK_ROWS = 8

   https://devblogs.nvidia.com/efficient-matrix-transpose-cuda-cc/
*/
const int tp_TILE_DIM = 32;
const int tp_BLOCK_ROWS = 8;

__global__ void transposeCoalesced(int *result, const int *in)
{
  const int TILE_DIM = tp_TILE_DIM;
  const int BLOCK_ROWS = tp_BLOCK_ROWS;

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


__global__ void multiplyMatrixVector(int* result, int* matrix, int* vector, int cols)
{
  __shared__ int reduce_array[256]; // Within a block

  int vector_slice_offset = blockIdx.x * cols + threadIdx.x; 
  int matrix_slice_offset = blockIdx.y * cols + threadIdx.x;
  reduce_array[threadIdx.x] = matrix[matrix_slice_offset] * vector[vector_slice_offset];

  __syncthreads();

  // Sequential reduce.
  if (threadIdx.x == 0){
    int accumulator = 0;
    for (int i = 0; i < blockDim.x; i++)
    {
      accumulator += reduce_array[i];
    }
    result[blockIdx.x * cols + blockIdx.y] = accumulator;
  }  
}

// We use single-dimensional lists.
void matrixMultiply(dag_array_t* result, dag_array_t* m1, dag_array_t* m2){

    // Precompute size information
    size_t size_m1 = m1->rows * m1->cols;
    size_t size_m2 = m2->rows * m2->cols;
    size_t size_result = m1->rows * m2->cols;

    // Copy onto device
    int* d_m1;  
    cudaMalloc(&d_m1,size_m1);
    cudaMemcpy(d_m1,m1->matrix,size_m1,cudaMemcpyHostToDevice);

    int* d_m2;  
    cudaMalloc(&d_m2,size_m2);
    cudaMemcpy(d_m2,m2->matrix,size_m2,cudaMemcpyHostToDevice);

    int* d_col; // We know that transpose will return same # of elem.
    cudaMalloc(&d_col,size_m2);

    int* d_result; // Allocate our result.
    cudaMalloc(&d_result,size_result);
    // A cruical optimization involves removing extraneous cudaMemcpy and cudaMallocs.

    dim3 dimGrid(m2->rows/tp_TILE_DIM, m2->cols/tp_TILE_DIM, 1);
    dim3 dimBlock(tp_TILE_DIM, tp_BLOCK_ROWS, 1);
    transposeCoalesced<<<dimGrid,dimBlock>>>(d_col,d_m2);

    const int threadsPerBlock = 256;
    dim3 dimBlock2(threadsPerBlock,1,1); // 256 threads per row
    dim3 dimGrid2((m1->rows + threadsPerBlock - 1) / threadsPerBlock,
                 (m2->rows + threadsPerBlock - 1) / threadsPerBlock,1);
    multiplyMatrixVector<<<dimGrid2,dimBlock2>>>(d_result,d_m1,d_col,m1->cols);

    cudaMemcpy(result->matrix,d_result,size_result,cudaMemcpyDeviceToHost);
    result->rows = m1->rows;
    result->cols = m2->cols;

    cudaFree(d_m1);
    cudaFree(d_m2);
    cudaFree(d_result);
    cudaFree(d_col);
}

int main(){
  int* a = (int*) malloc(100*sizeof(int));
  int* b = (int*) malloc(100*sizeof(int));

  dag_array_t A;
  A.rows = 10;
  A.cols = 10;
  A.matrix = a;

  dag_array_t B;
  B.rows = 10;
  B.cols = 10;
  B.matrix = b;

  dag_array_t C;
  C.rows = 10;
  C.cols = 10;
  C.matrix = (int*) malloc(100*sizeof(int));

  for (int i = 0; i < 10; i++)
  {
    for (int j = 0; j < 10; j++)
    {
      a[i*10+j] = 1;
      b[i*10+j] = 2;
    }
  }

  matrixMultiply(&C,&A,&B);

  for (int i = 0; i < 10; i++)
  {
    for (int j = 0; j < 10; j++)
    {
      printf(" %d ",C.matrix[i*10+j]);
    }
  }
}