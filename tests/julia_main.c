#include "julia_test.h"
#include <sys/time.h>
#include "ppm.cpp"

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

void render_julia(bool* image, int DIM){
  for (int y=0; y<DIM; y++) {
    for (int x=0; x<DIM; x++) {
      int offset = x + y * DIM;
      int juliaValue = julia( x, y, DIM );
      image[offset] = juliaValue;
    }
  }
}

int main(){
  int DIM = 1024; 
  int RUNS = 3;

  long timec = __LONG_MAX__;
  long timed = __LONG_MAX__;

  struct timeval t0;
  struct timeval t1;
  long elapsed;

  //int trues = 0; int falses = 0;
  //for (int i = 0; i < DIM * DIM; i++){
  //  if(julia_c[i]) trues++; else falses++;
  //}
  //printf("\t  (C) Trues: %d, Falses %d\n",trues,falses);
  //trues = 0; falses = 0;
  bool* julia_c = (bool*) calloc(DIM*DIM,(sizeof(bool)));
  bool* julia_dag = (bool*) calloc(DIM*DIM,(sizeof(bool)));

  for (int i = 0; i < RUNS; i++){
    gettimeofday(&t0,NULL);
    render_julia(julia_c,DIM);
    gettimeofday(&t1,NULL);
    elapsed = (t1.tv_sec-t0.tv_sec)*1000000 + t1.tv_usec-t0.tv_usec;
    timec = minl(elapsed,timec);

    gettimeofday(&t0,NULL);
    dag_render_julia(julia_dag,DIM,DIM,DIM); // this is a little awkward
    gettimeofday(&t1,NULL);
    elapsed = (t1.tv_sec-t0.tv_sec)*1000000 + t1.tv_usec-t0.tv_usec;
    timed = minl(elapsed,timed);
  }
  
  int* julia_img = (int*) calloc(DIM*DIM,sizeof(int));
  for (int i = 0; i < DIM * DIM; i++){
   if(julia_dag[i]) julia_img[i] = 255;
  }
  //printf("\t(DAG) Trues: %d, Falses %d\n",trues,falses);

  writePPMImage(julia_img, DIM, DIM, "julia.ppm", 256);
  verifyBoolArrays("julia",julia_c,julia_dag,DIM*DIM);

  printf("Julia (C)    \t%ld\n",timec);
  printf("Julia (DAG)  \t%ld\n",timed);
}

