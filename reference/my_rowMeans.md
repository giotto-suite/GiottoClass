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
#>  [1]  0.200770123  0.059300706 -0.108827833  0.475920115 -0.007621864
#>  [6] -0.214898287 -0.111271765  0.522163939 -0.144365292  0.508659802
```
