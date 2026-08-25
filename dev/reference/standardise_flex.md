# standardise_flex

Matrix scaling.

## Usage

``` r
standardise_flex(x, center = TRUE, scale = TRUE)
```

## Arguments

- x:

  matrix

- center:

  center data

- scale:

  scale data

## Value

`ScaledMatrix` or `IterableMatrix`

## Examples

``` r
m <- matrix(rnorm(100), nrow = 10)

standardise_flex(m)
```
