# fastest-matrices

To run:
`stack build :bench-alloc && stack exec bench-alloc`

Or:
`stack build :bench-runtime && stack exec bench-runtime`

## Results

### Runtime

#### Matrix-matrix multiplication

| Library | n = 10 | n = 50 | n = 100 | n = 1000 |
| --- | --- | --- | --- | --- |
| DLA | 3.400 μs | 339.2 μs | | |
| hmatrix | 1.179 μs | 56.76 μs | | |
| NumHask | 47.37 ms | 87.64 ms | | |
| Matrix | 10.84 μs | 974.9 μs | | |

#### Repeated matrix-matrix multiplication

| Library | n = 10 | n = 50 | n = 100 | n = 1000 |
| --- | --- | --- | --- | --- |
| DLA | 9.217 μs | 1.050 ms | | |
| hmatrix | 3.945 μs | 168.5 μs | | |
| NumHask | 83.90 ms | 139.9 ms  | | |

#### Matrix-vector multiplication

| Library | n = 10 | n = 50 | n = 100 | n = 1000 |
| --- | --- | --- | --- | --- |
| DLA | 257.7 ns | 5.904 μs | | |
| hmatrix | 450.3 ns | 1.867 μs | | |

#### QR factorization

| Library | n = 10 | n = 50 | n = 100 | n = 1000 |
| --- | --- | --- | --- | --- |
| DLA | 3.050 μs | 312.8 μs | | |
| hmatrix | 68.20 μs | 5.653 ms | | |

#### Transpose

| Library | n = 10 | n = 50 | n = 100 | n = 1000 |
| --- | --- | --- | --- | --- |
| DLA | 1.285 μs | 29.84 μs | | |
| hmatrix | 12.02 ns | 10.14 ns | | |
| NumHask | 9.356 μs | 10.85 μs | | |
| Matrix | 2.267 μs | 58.81 μs | | |


#### Norm

| Library | n = 10 | n = 50 | n = 100 | n = 1000 |
| --- | --- | --- | --- | --- |
| DLA | 197.0 ns | 5.067 μs | | |
| hmatrix | 89.56 ns | 101.4 ns | | |
| NumHask | 12.90 μs | 12.64 μs | | |

#### Row

| Library | n = 10 | n = 50 | n = 100 | n = 1000 |
| --- | --- | --- | --- | --- |
| DLA | 16.01 ns | 16.32 ns | | |
| hmatrix | 704.6 ns | 696.4 ns | | |
| NumHask | 194.9 ns | 175.1 ns | | |
| Matrix | 42.94 ns | 185.9 ns | | |

#### Column

| Library | n = 10 | n = 50 | n = 100 | n = 1000 |
| --- | --- | --- | --- | --- |
| DLA | 38.04 ns | 134.5 ns | | |
| hmatrix | 817.4 ns | 811.2 ns | | |
| NumHask | 834.3 ns | 1.151 μs | | |
| Matrix | 222.0 ns | 887.0 ns  | | |

#### Identity

| Library | n = 10 | n = 50 | n = 100 | n = 1000 |
| --- | --- | --- | --- | --- |
| DLA | 90.64 ns | 1.021 μs | | |
| hmatrix | 1.804 μs | 28.01 μs | | |
| Matrix | 1.845 μs | 38.73 μs | | |

#### Diagonal

| Library | n = 10 | n = 50 | n = 100 | n = 1000 |
| --- | --- | --- | --- | --- |
| DLA | 87.47 ns | 839.2 ns | | |
| hmatrix | 1.665 μs | 27.49 μs | | |
