#include "julia_test.h"

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
bool julia (int x, int y, int DIM) {
  const float scale = 1.5f;
  float jx = scale * (float)(DIM - x)/(float) (DIM);
  float jy = scale * (float)(DIM - y)/(float) (DIM);
  
  complex_t c;
  complex_t a; 
  c.r = -0.8f; c.i =  0.156f;
  a.r = jx; a.i = jy;

  int i = 0;
  for (i=0; i < 50; i++) {
    a = complex_add(complex_mult(a,a),c);
  }

  float mag = a.r * a.r + a.i * a.i; 
  return mag <= 1000.f;
}

bool* render_julia(int DIM){
  bool* image = (bool*) malloc(DIM*DIM*sizeof(bool));
  for (int y=0; y<DIM; y++) {
    for (int x=0; x<DIM; x++) {
      int offset = x + y * DIM;
      int juliaValue = julia( x, y, DIM );
      image[offset] = juliaValue;
    }
  }
  return image;
}

int main(){
  int DIM = 1024; 

  bool* julia_c = render_julia(DIM);
  int trues = 0; int falses = 0;
  for (int i = 0; i < DIM * DIM; i++){
    if(julia_c[i]) trues++; else falses++;
  }
  printf("\t  (C) Trues: %d, Falses %d\n",trues,falses);
  trues = 0; falses = 0;
  bool* julia_dag = (bool*) calloc(DIM*DIM,(sizeof(bool)));
  dag_render_julia(julia_dag,DIM,DIM,DIM); // this is a little awkward
  for (int i = 0; i < DIM * DIM; i++){
    if(julia_dag[i]) trues++; else falses++;
  }
  printf("\t(DAG) Trues: %d, Falses %d\n",trues,falses);

  verifyBoolArrays("julia",julia_c,julia_dag,DIM*DIM);
}

