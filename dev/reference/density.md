# Density plot

Create density plots of the pixel values of a giottoLargeImage. Wrapper
around
[`terra::density()`](https://rspatial.github.io/terra/reference/density.html).

## Usage

``` r
# S4 method for class 'giottoLargeImage'
density(x, show_max = TRUE, ...)
```

## Arguments

- x:

  giottoLargeImage

- show_max:

  logical. Plot the set max intensity as a vertical red line

- ...:

  Arguments passed on to
  [`terra::density`](https://rspatial.github.io/terra/reference/density.html)

  `maxcells`

  :   the maximum number of (randomly sampled) cells to be used for
      creating the plot

  `plot`

  :   if `TRUE` produce a plot, else return a density object

  `main`

  :   character. Caption of plot(s)

## Value

density plot

## See also

[`hist()`](https://giotto-suite.github.io/GiottoClass/dev/reference/hist.md)

## Examples

``` r
f <- system.file(package = "GiottoClass", "extdata/toy_intensity.tif")
gimg <- createGiottoLargeImage(f, use_rast_ext = TRUE)

density(gimg)
```
