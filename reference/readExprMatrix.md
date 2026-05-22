# Read expression matrix

Function to read an expression matrix into a sparse matrix.

## Usage

``` r
readExprMatrix(
  path,
  cores = determine_cores(),
  transpose = FALSE,
  feat_type = "rna",
  expression_matrix_class = c("dgCMatrix", "DelayedArray", "dbSparseMatrix")
)
```

## Arguments

- path:

  path to the expression matrix

- cores:

  number of cores to use

- transpose:

  transpose matrix

- feat_type:

  feature type (e.g. "rna", "dna", "protein")

- expression_matrix_class:

  class of expression matrix to use (e.g. 'dgCMatrix', 'DelayedArray')

## Value

sparse matrix

## Details

The expression matrix needs to have both unique column names and row
names

## Examples

``` r
x <- matrix(seq_len(100), nrow = 10)
temporal_dir <- tempdir()
write.csv(x, paste0(temporal_dir, "/mymatrix.csv"))

readExprMatrix(paste0(temporal_dir, "/mymatrix.csv"))
#> 10 x 10 sparse Matrix of class "dgCMatrix"
#>   [[ suppressing 10 column names ‘V1’, ‘V2’, ‘V3’ ... ]]
#>                                  
#> 1   1 11 21 31 41 51 61 71 81  91
#> 2   2 12 22 32 42 52 62 72 82  92
#> 3   3 13 23 33 43 53 63 73 83  93
#> 4   4 14 24 34 44 54 64 74 84  94
#> 5   5 15 25 35 45 55 65 75 85  95
#> 6   6 16 26 36 46 56 66 76 86  96
#> 7   7 17 27 37 47 57 67 77 87  97
#> 8   8 18 28 38 48 58 68 78 88  98
#> 9   9 19 29 39 49 59 69 79 89  99
#> 10 10 20 30 40 50 60 70 80 90 100
```
