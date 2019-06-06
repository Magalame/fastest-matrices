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
| DLA | | 339.2 μs | | |
| hmatrix | | 56.76 μs | | |
| NumHask | | 87.64 ms | | |
| Matrix | | 974.9 μs | | |

#### Repeated matrix-matrix multiplication

| Library | n = 10 | n = 50 | n = 100 | n = 1000 |
| --- | --- | --- | --- | --- |
| DLA | | 1.050 ms | | |
| hmatrix | | 168.5 μs | | |
| hmatrix | | 139.9 ms  | | |

#### Matrix-vector multiplication

| Library | n = 10 | n = 50 | n = 100 | n = 1000 |
| --- | --- | --- | --- | --- |
| DLA | | 5.904 μs | | |
| hmatrix | | 1.867 μs | | |

#### QR factorization

| Library | n = 10 | n = 50 | n = 100 | n = 1000 |
| --- | --- | --- | --- | --- |
| DLA | | 312.8 μs | | |
| hmatrix | | 5.653 ms | | |

#### Transpose

| Library | n = 10 | n = 50 | n = 100 | n = 1000 |
| --- | --- | --- | --- | --- |
| DLA | | 29.84 μs | | |
| hmatrix | | 10.14 ns | | |
| NumHask | | 10.85 μs | | |
| Matrix | | 58.81 μs | | |


#### Norm

| Library | n = 10 | n = 50 | n = 100 | n = 1000 |
| --- | --- | --- | --- | --- |
| DLA | | 5.067 μs | | |
| hmatrix | | 101.4 ns | | |
| NumHask | | 12.64 μs | | |

#### Row

| Library | n = 10 | n = 50 | n = 100 | n = 1000 |
| --- | --- | --- | --- | --- |
| DLA | | 16.32 ns | | |
| hmatrix | | 696.4 ns | | |
| NumHask | | 175.1 ns | | |
| Matrix | | 185.9 ns | | |

#### Column

| Library | n = 10 | n = 50 | n = 100 | n = 1000 |
| --- | --- | --- | --- | --- |
| DLA | | 134.5 ns | | |
| hmatrix | | 811.2 ns | | |
| NumHask | | 1.151 μs | | |
| Matrix | | 887.0 ns  | | |

#### Identity

| Library | n = 10 | n = 50 | n = 100 | n = 1000 |
| --- | --- | --- | --- | --- |
| DLA | | 1.021 μs | | |
| hmatrix | | 28.01 μs | | |
| Matrix | | 38.73 μs | | |

#### Diagonal

| Library | n = 10 | n = 50 | n = 100 | n = 1000 |
| --- | --- | --- | --- | --- |
| DLA | | 839.2 ns | | |
| hmatrix | | 27.49 μs | | |
