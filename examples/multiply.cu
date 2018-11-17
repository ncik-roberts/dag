
 /*
  * This file is an attempt at producing what the generated target code 
  * should look like for the multiplyMatrixMatrix routine. This includes
  * a number of bits:
  
  *  - How to implement primitives (usually - NVIDIA examples)
  *  - Internal C++ representations (dag_array_t) ?
  *  - Interaction between host code and device code:
        -> Is it possible to transform each function at the call site 
           based off of how many nested parallel blocks it is in?
           (Here: we inline dot-product. In general, I suspect an extremely aggressive
           inlining policy will make our life easier.)
  *  - C++ results of the "<-" operator
        -> We want to avoid nested kernel launches. It's technically possible,
           but the results are unpredictable.
        -> We instead transform the entire thing into CUDA's nested block structure using
           dim3 (x,y,_) to denote the 'quadratic' parallelism.
        -> This abstraction hits a wall when the subroutines are so far composed as to offer
           more than 'cubic' parallelism (3 nested levels). 
  */

/* Prototype matrix representation. */
struct dag_array_t{
  size_t rows;
  size_t cols; 
  int* matrix;
}

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


__global__ void multiplyMatrixVector(int* result, int* matrix, int* vector, int cols)
{
  __shared__ int reduce_array[blockDim.x]; // Within a block

  int idx = blockDim.x * blockIdx.x + threadIdx.x;

  //Todo: how do we merge nested parallel blocks into a single kernel?
  // (By propagating the index and blocks - but more precisely)
  
  // int[] col <- transpose (m2) produces "vector slices"
  // If we use CUDA's block indexing syntax: 

  // or whatever size is passed in ~~~~~~~v
  int vector_slice_offset = blockIdx.x * cols + threadIdx.x; 
  int matrix_slice_offset = blockIdx.y * cols + threadIdx.x;
  reduce_array[threadIdx.x] = matrix[matrix_slice_offset] * vector[vector_slice_offset];

  __syncthreads();

  // Can this be parallelised? Probably. We don't do that now.
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

    if (m1->cols != m2->rows){
      printf("Invalid Matmult Dimensions : (%u x %u) x (%u x %u)\n"
            m1->rows,m1->cols,m2->rows,m2->cols);
      assert(false);
    }
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
    cudaMalloc(&d_col,size_m2)

    int* d_result; // Allocate our result.
    cudaMalloc(&d_result,size_result);
    // A cruical optimization involves removing extraneous cudaMemcpy and cudaMallocs.

    dim3 dimGrid(m2->rows/tp_TILE_DIM, m2->cols/tp_TILE_DIM, 1);
    dim3 dimBlock(tp_TILE_DIM, tp_BLOCK_ROWS, 1);
    transposeCoalesced<<<dimGrid,dimBlock>>>(d_col,d_m2);

    const int threadsPerBlock = 256;
    dim3 dimBlock(threadsPerBlock,1,1); // 256 threads per row
    dim3 dimGrid((m1->rows + threadsPerBlock - 1) / threadsPerBlock,
                 (m2->rows + threadsPerBlock - 1) / threadsPerBlock,1);
    multiplyMatrixVector<<<dimGrid,dimBlock>>>(d_result,d_m1,d_col,m1->cols);

    cudaMemcpy(result->matrix,d_result,size_result,cudaMemcpyDeviceToHost);
    result->rows = m1->rows;
    result->cols = m2->cols;

    cudaFree(d_m1);
    cudaFree(d_m2);
    cudaFree(d_result);
    cudaFree(d_col);
}