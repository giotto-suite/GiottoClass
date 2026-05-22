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
#>  [1] -0.1115180 -0.2079954  0.2838068  0.3373178 -0.1238467 -0.2577087
#>  [7] -0.2453714  0.5362443 -0.4952991  0.2693857
```
