# my_growMeans

my_growMeans

## Usage

``` r
my_growMeans(x, offset = 0.1)
```

## Arguments

- x:

  data to use

- offset:

  offset

## Value

numeric

## Examples

``` r
m <- matrix(rnorm(100), nrow = 10)

my_growMeans(abs(m))
```
