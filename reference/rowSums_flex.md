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
#>  [1]  4.1367869  5.0751540  2.9238742 -2.3873500  3.1626959 -3.7542709
#>  [7] -1.7915895 -0.8345224 -4.1899272 -1.6540201
```
