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
#>  [1]  0.001245224 -0.100128293  0.285642822 -0.275688954 -0.037151861
#>  [6]  0.198519294  0.469547486  0.190818799  0.020088524 -0.159281208
```
