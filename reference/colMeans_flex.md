# colMeans_flex

colMeans_flex

## Usage

``` r
colMeans_flex(mymatrix, ...)
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

colMeans_flex(m)
#>  [1] -0.33020071 -0.14415435  0.13720593  0.38627780 -0.15823532 -0.21262668
#>  [7] -0.04357001  0.35755432 -0.45103539  0.06068702
```
