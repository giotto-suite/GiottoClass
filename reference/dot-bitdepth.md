# Guess likely bitdepth from value(s)

Guess likely bitdepth from value(s)

## Usage

``` r
.bitdepth(x, return_max = FALSE)
```

## Arguments

- x:

  numeric vector. Values representative of the data to be assessed for
  bitdepth. This is usually a sampled subset of values from the raster.

- return_max:

  logical. default is `FALSE`. Whether to return the maximum possible
  value for the detected bitdepth instead of the bitdepth itself

## Value

numeric
