# my_rowMeans

my_rowMeans

## Usage

``` r
my_rowMeans(x, method = c("arithmic", "geometric"), offset = 0.1)
```

## Arguments

- x:

  data to use

- method:

  method is either "arithmic" or "geometric"

- offset:

  offset

## Value

numeric

## Examples

``` r
m <- matrix(rnorm(100), nrow = 10)

my_rowMeans(m)
#>  [1] -0.15311986 -0.38571475  0.03847178  0.02709917 -0.18849404 -0.06858837
#>  [7]  0.18932197 -0.13986224  0.38478620  0.10686590
```
