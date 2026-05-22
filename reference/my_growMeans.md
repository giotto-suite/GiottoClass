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
#>  [1] 0.9633923 0.5414421 1.0759292 0.5109339 0.5580623 0.4159464 0.9562026
#>  [8] 0.7513000 0.9175313 0.7790556
```
