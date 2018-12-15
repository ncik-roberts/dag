## What is Dag?

Dag is a programming language that can be compiled to CUDA.
The compilation scheme for Dag involves generating
many alternatives, both in terms of instruction orderings and in
terms of structuring the sequence of kernel launches to achieve
maximal parallelism.

This GitHub repository contains the source code for `dagc`, a
Dag-to-CUDA compiler. We also direct the interested reader to
the [website for this project](https://nick-and-dan.github.io/418/),
which includes a more detailed technical discussion of this
compiler.

## Why?

Structuring a GPU program requires many conscious decisions on the
programmer's part. What sections should be kernel launches? Where
should I allocate an array to store the results of this kernel launch?
Once these decisions are made, the programmer must re-implement
the code to assign partitions of data to each CUDA thread (involving
manual indexing) and to copy data back and forth between the host
and device.

Providing a high-level source language for structuring CUDA programs
allows for a compiler to automatically try many alternative strategies
for structuring the output CUDA program, and to equally-automatically
partition and transfer the data.

## Installation instructions

To build this compiler, you will need to install OCaml version 4.05.0,
along with the `core` and `ppx_jane` packages. Run `make install` to
accomplish this&mdash;this will require `sudo` access, so please
inspect the Makefile before running this.

Once you have installed this, you can build the compiler binary by
running `make`. This creates a binary `dagc` that can be run on
input Dag programs.

## Usage

Run `./dagc <file-name>` to compile a dag file, examples of which are
included in the `examples` and `tests` directory. This treats the
last function declaration in the file as the function that will
be called externally by the cpp main function. To compile a different
function, run `./dagc <file-name> -f <func-name>`.

Pass the `-verbose` flag to see all alternatives considered, including
the intermediate representations.

## Walkthrough of an example program

This is what a Dag program looks like:

```cpp
  int[][] multiplyMatrixMatrix(int[][] m1, int[][] m2) {
    return for (int[] row : m1) {
      return for (int[] col : transpose(m2)) {
	return reduce(+, 0, zip_with(*, row, col));
      };
    };
  }
```

An equivalent Dag program with descriptive function names would be this:
```cpp
  int dot(int[] xs, int[] ys) {
    return reduce(+, 0, zip_with(*, xs, ys));
  }

  int[] multiplyMatrixVector(int[][] m, int[] v) {
    return for (int[] row : m) {
      return dot(row, v);
    };
  }

  int[][] multiplyMatrixMatrix(int[][] m1, int[][] m2) {
    return for (int[] row : m1) {
      return multiplyMatrixVector(transpose(m2), row);
    };
  }
```

The fundamental building blocks of a Dag program are the
`for`-expression and the parallel primitives, like `reduce`,
`zip_with`, and `filter_with`.

A `for`-expression (not statement!) semantically corresponds to a map operation
and operationally corresponds to either a CUDA kernel launch or a sequential
for loop. For example, take the expression:

```cpp
  for (int d : zip_with(-, xs, ys)) {
    int d_squared = d * d;
    return d_squared;
  }
```

This is an expression of type `int[]` and can be used anywhere an int array
is expected. The expression denotes an array where the `i`th element is
the square of the difference between the `i`th element of `xs` and the
`i`th element of `ys`.

## Run tests

We include a testing harness for verifying the correctness and performance
of the output CUDA. The harness is set up for running CUDA version 7.5,
compute architecture 2.0.

Here is an example usage of the testing harness:

```
$ cd tests
$ ../dagc mmult_test.dag # Creates mmult_test.cu
$ ./build.sh mmult # Creates binary from mmult_test.cu and mmult_main.c
$ ./dagtest # Runs the output binary to verify correctness/performance
```

## Authors

[Nick Roberts](https://github.com/ncik-roberts), [Dan Cascaval](https://github.com/dcascaval/)

This project was made for the Fall 2018 iteration of the 15-418 Parallel Computing course at Carnegie Mellon University.
