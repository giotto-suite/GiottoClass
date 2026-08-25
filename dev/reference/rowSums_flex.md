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
```
