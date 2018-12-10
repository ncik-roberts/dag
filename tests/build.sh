#i/bin/bash

TESTNAME="$1"

EXECUTABLE="dagtest"
CU_FILES=$TESTNAME"_test.cu"
CC_FILES=$TESTNAME"_test.h "$TESTNAME"_main.c"

OBJ_MAIN="objs/"$TESTNAME"_main.o"
OBJ_TEST="objs/"$TESTNAME"_test.o"

mkdir -p objs/
g++ -m64 $CC_FILES -O3 -Wall -c -o $OBJ_MAIN
nvcc $CU_FILES -O3 -m64 --gpu-architecture compute_20 -c -o $OBJ_TEST
g++ -m64 -O3 -Wall -o $EXECUTABLE $OBJ_MAIN $OBJ_TEST -L/usr/local/cuda/lib64/ -lcudart 
echo "Built Test: $TESTNAME"