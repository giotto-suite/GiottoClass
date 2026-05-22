# rowSums_flex

rowSums_flex

## Usage

``` r
rowSums_flex(mymatrix, ...)
```

## Arguments

- mymatrix:

  matrix to use

- ...:

  other arguments passed to
  [`rowSums`](https://rdrr.io/r/base/colSums.html)

## Value

numeric

## Examples

``` r
m <- matrix(rnorm(100), nrow = 10)

rowSums_flex(m)
#>  [1] -1.48687024 -3.16861153 -1.23712519  4.69458589  0.72426389 -0.37902390
#>  [7]  0.03970751  3.64465682 -0.81523302 -4.59233215
```
