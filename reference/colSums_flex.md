# colSums_flex

colSums_flex

## Usage

``` r
colSums_flex(mymatrix, ...)
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

colSums_flex(m)
#>  [1]   0.5626158   2.5194923 -10.1333315  -2.4267322   3.8784106  -1.9650656
#>  [7]  -2.8342559  -1.9619765   6.9062217  -2.2261136
```
