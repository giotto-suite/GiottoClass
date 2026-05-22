# rowMeans_flex

rowMeans_flex

## Usage

``` r
rowMeans_flex(mymatrix, ...)
```

## Arguments

- mymatrix:

  matrix to use

- ...:

  other arguments passed to underlying functions

## Value

numeric

## Examples

``` r
m <- matrix(rnorm(100), nrow = 10)

rowMeans_flex(m)
#>  [1] -0.07286566  0.36276125  0.25406230 -0.52179589 -0.31474459  0.02546356
#>  [7]  0.54074089 -0.29813520  0.56022467  0.08732056
```
