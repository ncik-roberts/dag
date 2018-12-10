#i/bin/bash

TESTNAME="$1"

EXECUTABLE="dagtest"
CU_FILES="saxpy_int_test.cu sum_int_test.cu saxpy_float_test.cu sum_float_test.cu"
CC_FILES="sanity_test.h sanity_main.c"

OBJ_MAIN="objs/sanity_main.o"
OBJ_TEST="objs/sanity_test.o"

mkdir -p objs/
g++ -m64 $CC_FILES -O3 -Wall -c -o $OBJ_MAIN
nvcc $CU_FILES -O3 -m64 --gpu-architecture compute_20 -c -o $OBJ_TEST
g++ -m64 -O3 -Wall -o $EXECUTABLE $OBJ_MAIN $OBJ_TEST -L/usr/local/cuda/lib64/ -lcudart 
echo "Built Test: $TESTNAME"