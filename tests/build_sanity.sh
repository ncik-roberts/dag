#i/bin/bash

TESTNAME="$1"

EXECUTABLE="dagtest"
CC_FILES="sanity_test.h sanity_main.c"
OBJFILES="saxpy_int_test.o sum_int_test.o saxpy_float_test.o sum_float_test.o"
OBJ_MAIN="objs/sanity_main.o"

mkdir -p objs/
g++ -m64 $CC_FILES -O3 -Wall -c -o $OBJ_MAIN
nvcc "saxpy_int_test.cu" -O3 -m64 --gpu-architecture compute_20 -c -o "saxpy_int_test.o" 
nvcc "sum_int_test.cu" -O3 -m64 --gpu-architecture compute_20 -c -o "sum_int_test.o" 
nvcc "saxpy_float_test.cu" -O3 -m64 --gpu-architecture compute_20 -c -o "saxpy_float_test.o" 
nvcc "sum_float_test.cu" -O3 -m64 --gpu-architecture compute_20 -c -o "sum_float_test.o" 
g++ -m64 -O3 -Wall -o $EXECUTABLE $OBJ_MAIN $OBJFILES  -L/usr/local/cuda/lib64/ -lcudart 
echo "Built Test: $TESTNAME"