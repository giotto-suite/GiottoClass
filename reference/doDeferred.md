# Perform deferred/lazy operations

Force deferred/lazy operations.

## Usage

``` r
# S4 method for class 'giottoAffineImage'
doDeferred(x, size = 5e+05, filename = NULL, ...)
```

## Arguments

- x:

  object to force deferred operations in

- size:

  numeric. Minimum number of image pixels to render when evaluating

- filename:

  character. Full filepath to write the rendered image to. If `NULL`, a
  file in [`tempdir()`](https://rdrr.io/r/base/tempfile.html) will be
  generated.

- ...:

  additional args to pass

## Value

giottoLargeImage

## Examples

``` r
gimg <- GiottoData::loadSubObjectMini("giottoLargeImage")
#> Warning: [rast] unknown extent
affimg <- spin(gimg, 45) # lazily performs affine
#> Error: package 'magick' is not yet installed
#> 
#>  To install:
#> install.packages(c("magick"))

# force the affine operation and render the output with at least 5e5 px
gimg2 <- doDeferred(affimg, size = 5e5)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'doDeferred': object 'affimg' not found
# **This is mainly intended for visualization.**
# This process saves with image depth of 8.
# Spatially transformed raster values are not preferred for analysis
```
