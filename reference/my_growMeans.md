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
#>  [1] 0.6163492 0.7027002 0.5971551 0.5708321 0.5767433 0.4898005 0.7127045
#>  [8] 0.7016844 0.5560531 0.4844880
```
