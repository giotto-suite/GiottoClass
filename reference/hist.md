# Histogram

Create a histogram of the pixel values of a giottoLargeImage. Wrapper
around
[`terra::hist()`](https://rspatial.github.io/terra/reference/hist.html)

## Usage

``` r
# S4 method for class 'giottoLargeImage'
hist(x, show_max = TRUE, ...)
```

## Arguments

- x:

  giottoLargeImage

- show_max:

  logical. Plot the set max intensity as a vertical red line

- ...:

  Arguments passed on to
  [`terra::hist`](https://rspatial.github.io/terra/reference/hist.html)

  `layer`

  :   positive integer or character to indicate layer numbers (or
      names). If missing, all layers up to `maxnl` are used

  `maxcell`

  :   integer. To regularly sample very large objects

  `plot`

  :   logical. Plot the histogram or only return the histogram values

  `maxnl`

  :   positive integer. The maximum number of layers to use. Ignored if
      `layer` is not missing

  `main`

  :   character. Main title(s) for the plot. Default is the value of
      [`names`](https://rspatial.github.io/terra/reference/names.html)

## Value

histogram

## See also

[`density()`](https://giotto-suite.github.io/GiottoClass/reference/density.md)

## Examples

``` r
f <- system.file(package = "GiottoClass", "extdata/toy_intensity.tif")
gimg <- createGiottoLargeImage(f, use_rast_ext = TRUE, verbose = FALSE)

hist(gimg)
#> Warning: [hist] a sample of 83% of the cells was used
```
