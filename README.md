# fastest-matrices

This project benchmarks the following libraries:
- [hmatrix](http://hackage.haskell.org/package/hmatrix)
- [dense-linear-algebra](https://hackage.haskell.org/package/dense-linear-algebra) (refered to as DLA henceforth)
- [numhask](https://hackage.haskell.org/package/numhask)
- [massiv](http://hackage.haskell.org/package/massiv) ("Massiv (Par)" refers to [parallel computation](http://hackage.haskell.org/package/massiv-0.3.2.0/docs/Data-Massiv-Core.html#t:Comp)
- [matrix](https://hackage.haskell.org/package/matrix-0.3.6.1)


To run:

(allocation)
`stack build :bench-alloc && stack exec bench-alloc`

(runtime)
`stack build :bench-runtime && stack exec bench-runtime`

## Results

### Runtime

#### Matrix-matrix multiplication

| Library | n = 10 | n = 50 | n = 100 |
| --- | --- | --- | --- |
| DLA | 2.65 us | 289.0 us | 2.24 ms |
| Hmatrix | 1.32 us | 55.8 us | 292.0 us |
| NumHask | 714.0 us | 63.5 ms | 593.0 ms |
| Massiv | 12.0 us | 205.0 us | 1.52 ms |
| Massiv (Par) | 76.1 us | 220.0 us | 866.0 us |
| Matrix | 12.6 us | 1.1 ms | 8.44 ms |
| Naive C | 51 us | 323 us | 4.78 ms |

#### Repeated matrix-matrix multiplication

| Library | n = 10 | n = 50 | n = 100 |
| --- | --- | --- | --- |
| DLA | 8.25 us | 852.0 us | 6.92 ms |
| Hmatrix | 5.41 us | 170.0 us | 889.0 us |
| NumHask | 1.46 ms | 152.0 ms | 1.42 s |
| Massiv | 38.9 us | 629.0 us | 4.48 ms |
| Massiv (Par) | 358.0 us | 816.0 us | 2.8 ms |

#### Matrix-vector multiplication

| Library | n = 10 | n = 50 | n = 100 |
| --- | --- | --- | --- |
| DLA | 302.0 ns | 4.12 us | 16.1 us |
| Hmatrix | 706.0 ns | 2.27 us | 11.1 us |

#### QR factorization

| Library | n = 10 | n = 50 | n = 100 |
| --- | --- | --- | --- |
| DLA | 3.32 us | 233.0 us | 1.7 ms |
| Hmatrix | 94.0 us | 6.62 ms | 60.3 ms |

#### Transpose

| Library | n = 10 | n = 50 | n = 100 |
| --- | --- | --- | --- |
| DLA | 330.0 ns | 8.46 us | 25.9 us |
| Hmatrix | 24.9 ns | 24.4 ns | 17.6 ns |
| NumHask | 309.0 ns | 7.29 us | 28.1 us |
| Massiv | 7.29 us | 35.7 us | 122.0 us |
| Matrix | 4.58 us | 130.0 us | 699.0 us |

#### Norm
| Library | n = 10 | n = 50 | n = 100 |
| --- | --- | --- | --- |
| DLA | 189.0 ns | 4.15 us | 16.8 us |
| Hmatrix | 285.0 ns | 1.15 us | 4.32 us |
| NumHask | 40.3 us | 1.79 ms | 9.72 ms |
| Massiv | 128.0 ns | 3.3 us | 13.0 us |
| Naive C | 350 ns | 12.65 us | 40.96 μs |

#### Row

| Library | n = 10 | n = 50 | n = 100 |
| --- | --- | --- | --- |
| DLA | 26.4 ns | 19.5 ns | 19.5 ns |
| Hmatrix | 1.43 us | 1.63 us | 1.7 us |
| NumHask | 39.4 ns | 170.0 ns | 305.0 ns |
| Massiv | 3.97 us | 5.08 us | 4.74 us |
| Matrix | 40.9 ns | 167.0 ns | 310.0 ns |

#### Column

| Library | n = 10 | n = 50 | n = 100 |
| --- | --- | --- | --- |
| DLA | 61.0 ns | 279.0 ns | 295.0 ns |
| Hmatrix | 1.43 us | 1.7 us | 1.84 us |
| NumHask | 221.0 ns | 1.04 us | 2.38 us |
| Massiv | 4.63 us | 5.04 us | 5.04 us |
| Matrix | 350.0 ns | 1.59 us | 3.09 us |

#### Identity

| Library | n = 10 | n = 50 | n = 100 |
| --- | --- | --- | --- |
| DLA | 157.0 ns | 4.75 us | 11.2 us |
| Hmatrix | 2.31 us | 34.5 us | 132.0 us |
| Matrix | 2.94 us | 65.9 us | 492.0 us |

#### Diagonal

| Library | n = 10 | n = 50 | n = 100 |
| --- | --- | --- | --- |
| DLA | 124.0 ns | 5.04 us | 11.2 us |
| Hmatrix | 2.15 us | 33.7 us | 132.0 us |

----------------------------------------------------------------------------------
### Allocation

#### Matrix-matrix multiplication

| Library | n = 10 | n = 50 | n = 100 | 
| --- | --- | --- | --- |
| DLA | 976 | 20,176 | 80,176 |
| hmatrix | 904 | 20,936 | 80,936|
| NumHask | 1,691,432 | 179,093,816 | 1,400,273,872 |
| Massiv | 5,816 | 140,216 | 560,216 |
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
| Massiv | 872 | 20,072 | 80,072 | 
| Matrix | 9,840 | 239,952 | 959,664 | 


#### Norm

| Library | n = 10 | n = 50 | n = 100 |
| --- | --- | --- | --- |
| DLA | 16 | 16 | 16 |
| hmatrix | 232 | 232 | 232 |
| NumHask | 146,800 | 2,919,552 | 11,641,752 |
| Massiv | 16 | 16 | 16 |

#### Row

| Library | n = 10 | n = 50 | n = 100 |
| --- | --- | --- | --- |
| DLA | 64 | 64 | 64 |
| hmatrix | 2,128 | 2,128 | 2,128 |
| NumHask | 256 | 256 | 256 |
| Massiv | 144 | 464 | 864 |
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

Relevant details:

- the implementations of the "naive C" parts can be found in `/naive`. They were compiled with `-O3`
- the `massiv` benchmarks use the `Primitive` representation, which *seems* to be the fastest among what massiv offers
- the benchmarked functions from DLA are taken from the `Fast` module when available
- the `norm` function is called on `n*n` vectors
- instead of relying on hackage, the project's dependencies fetch the libraries directly from github (see `stack.yaml`).

Formely included:
- `bed-and-breakfast` (abandoned because too slow)
- `matrices` (abandoned because too slow, also see https://github.com/kaizhang/matrices/issues/8)


TODO:
- have a cleaner/more abstract interface for the benches
