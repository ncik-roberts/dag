#include <julia_test.h>

typedef struct { 
  float r;
  float i;
} complex_t;

complex_t complex_mult(complex_t a, complex_t b){
  complex_t result;
  result.r = a.r * b.r - a.i * b.i; 
  result.i = a.i * b.r + a.r * b.i;
  return result;
}

complex_t complex_add(complex_t a,complex_t b){
  complex_t result;
  result.r = a.r + b.r;
  result.i = a.i + b.i;
  return result;
}

// We do some basic flipping with complex numbers.
int julia (int x, int y, int DIM) {
  const float scale = 1.5;
  float jx = scale * (float)(DIM/2 - x)/(DIM/2);
  float jy = scale * (float)(DIM/2 - y)/(DIM/2);
  
  complex_t c;
  complex_t a; 
  c.r = -0.8; c.i =  0.156;
  a.r = jx; a.i = jy;

  int i = 0;
  for (i=0; i<200; i++) {
    a = complex_add(complex_mult(a,a),c);
  }

  float mag = a.r * a.r + a.i * a.i; 
  if (mag > 1000.f)
    return 0;
  
  return 1;
}

int* render_julia(int DIM){
  int* image = (int*) malloc(DIM*DIM*sizeof(int));
  for (int y=0; y<DIM; y++) {
    for (int x=0; x<DIM; x++) {
      int offset = x + y * DIM;
      int juliaValue = julia( x, y, DIM );
      image[offset] = 255 * juliaValue;
    }
  }
  return image;
}

int main(){
  int DIM = 1024; 

  int* julia_c = render_julia(DIM);
  // Nice part is, this thing's already flattened.
  int* julia_dag = (int*) render_julia_DAG(DIM);

  verifyArrays("julia",julia_c,julia_dag,DIM*DIM);
}

