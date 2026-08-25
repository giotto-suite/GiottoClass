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
```
