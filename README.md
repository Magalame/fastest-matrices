# fastest-matrices

This project benchmarks the following libraries:
- [hmatrix](http://hackage.haskell.org/package/hmatrix)
- [dense-linear-algebra](https://hackage.haskell.org/package/dense-linear-algebra)
- [numhask](https://hackage.haskell.org/package/numhask)
- [matrix](https://hackage.haskell.org/package/matrix-0.3.6.1)

instead of relying on hackage, the project's dependencies fetch the libraries directly from github (see `stack.yaml`).

To run:

(allocation)
`stack build :bench-alloc && stack exec bench-alloc`

(runtime)
`stack build :bench-runtime && stack exec bench-runtime`

The implementations of the "naive C" parts can be found in `/naive`. They were compiled with `-O3`

## Results

### Runtime

#### Matrix-matrix multiplication

| Library | n = 10 | n = 50 | n = 100 | 
| --- | --- | --- | --- |
| DLA | 3.400 μs | 339.2 μs | 2.377 ms |
| hmatrix | 1.179 μs | 56.76 μs | 297.6 μs |
| NumHask | 659.4 μs | 87.64 ms | 310.4 ms |
| Matrix | 10.84 μs | 974.9 μs | 8.329 ms |
| Naive C | 51 us | 323 us | 4.78 ms |

#### Repeated matrix-matrix multiplication

| Library | n = 10 | n = 50 | n = 100 |
| --- | --- | --- | --- |
| DLA | 9.217 μs | 1.050 ms | 7.566 ms |
| hmatrix | 3.945 μs | 168.5 μs | 905.6 μs |
| NumHask | 1.060 ms | 139.9 ms | 691.9 ms |

#### Matrix-vector multiplication

| Library | n = 10 | n = 50 | n = 100 |
| --- | --- | --- | --- | 
| DLA | 257.7 ns | 5.904 μs | 21.36 μs | 
| hmatrix | 450.3 ns | 1.867 μs | 11.54 μs | 

#### QR factorization

| Library | n = 10 | n = 50 | n = 100 |
| --- | --- | --- | --- |
| DLA | 3.050 μs | 312.8 μs | 2.375 ms |
| hmatrix | 68.20 μs | 5.653 ms | 46.28 ms |

#### Transpose

| Library | n = 10 | n = 50 | n = 100 |
| --- | --- | --- | --- |
| DLA | 1.285 μs | 29.84 μs | 130.8 μs | 
| hmatrix | 12.02 ns | 10.14 ns | 11.93 ns | 
| NumHask | 446.9 ns | 10.85 μs | 35.28 μs | 
| Matrix | 2.267 μs | 58.81 μs | 355.8 μs | 

#### Norm

| Library | n = 10 | n = 50 | n = 100 |
| --- | --- | --- | --- |
| DLA | 154.3 ns | 3.716 μs | 15.83 μs |
| hmatrix | 99.41 ns | 824.0 ns | 3.579 μs |
| NumHask | 20.76 μs | 605.9 μs | 3.428 ms |
| Naive C | 350 ns | 12.65 us | 40.96 μs |

#### Row

| Library | n = 10 | n = 50 | n = 100 |
| --- | --- | --- | --- |
| DLA | 16.01 ns | 16.32 ns | 17.05 ns |
| hmatrix | 704.6 ns | 696.4 ns | 739.7 ns |
| NumHask | 41.40 ns | 175.1 ns | 377.8 ns |
| Matrix | 42.94 ns | 185.9 ns | 340.5 ns |

#### Column

| Library | n = 10 | n = 50 | n = 100 | 
| --- | --- | --- | --- | 
| DLA | 38.04 ns | 134.5 ns | 195.2 ns |
| hmatrix | 817.4 ns | 811.2 ns | 1.078 μs |
| NumHask | 192.4 ns | 1.151 μs | 1.708 μs |
| Matrix | 222.0 ns | 887.0 ns  | 1.954 μs |

#### Identity

| Library | n = 10 | n = 50 | n = 100 |
| --- | --- | --- | --- |
| DLA | 90.64 ns | 1.021 μs | 2.841 μs |
| hmatrix | 1.804 μs | 28.01 μs | 150.9 μs |
| Matrix | 1.845 μs | 38.73 μs | 210.5 μs |
#### Diagonal

| Library | n = 10 | n = 50 | n = 100 |
| --- | --- | --- | --- |
| DLA | 87.47 ns | 839.2 ns | 2.816 μs |
| hmatrix | 1.665 μs | 27.49 μs | 143.3 μs |

### Allocation

#### Matrix-matrix multiplication

| Library | n = 10 | n = 50 | n = 100 | 
| --- | --- | --- | --- |
| DLA | 976 | 20,176 | 80,176 |
| hmatrix | 904 | 20,936 | 80,936|
| NumHask | 1,691,432 | 179,093,816 | 1,400,273,872 |
| Matrix | 18,160 | 392,288 | 1,544,056 |

#### QR factorization

| Library | n = 10 | n = 50 | n = 100 |
| --- | --- | --- | --- |
| DLA | 1,848 | 40,248 | 160,248 |
| hmatrix | 201,192 | 9,074,048 | 67,457,120 |

#### Transpose

| Library | n = 10 | n = 50 | n = 100 |
| --- | --- | --- | --- |
| DLA | 880 | 20,080 | 80,080| 
| hmatrix | 64 | 64 | 64 | 
| NumHask | 0 | 0 | 0 | 
| Matrix | 9,840 | 239,952 | 959,664 | 


#### Norm

| Library | n = 10 | n = 50 | n = 100 |
| --- | --- | --- | --- |
| DLA | 16 | 16 | 16 |
| hmatrix | 232 | 232 | 232 |
| NumHask | 12,512 | 90,400 | 146,800|

#### Row

| Library | n = 10 | n = 50 | n = 100 |
| --- | --- | --- | --- |
| DLA | 64 | 64 | 64 |
| hmatrix | 2,128 | 2,128 | 2,128 |
| NumHask | 256 | 256 | 256 |
| Matrix | 896 | 20,112 | 80,168|

#### Column

| Library | n = 10 | n = 50 | n = 100 | 
| --- | --- | --- | --- | 
| DLA | 160 | 480 | 880 |
| hmatrix | 2,128 | 2,128 | 2,128 |
| NumHask | 800 | 2,720 | 5,120 |
| Matrix | 1,648 | 23,744  | 87,400 |

#### Identity

| Library | n = 10 | n = 50 | n = 100 |
| --- | --- | --- | --- |
| DLA | 1,008 | 20,528  |80,928 |
| hmatrix | 3,208 | 66,440 | 252,440 |
| Matrix | 5,752 | 139,848 | 559,504 |

