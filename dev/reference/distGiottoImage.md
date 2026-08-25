# Plot distribution of image intensity values

Plot distribution of intensity values using either a density plot or a
histogram. Useful for finding image artefact outliers and determining
reasonable scaling cutoffs.

## Usage

``` r
distGiottoImage(
  gobject = NULL,
  image_name = NULL,
  giottoLargeImage = NULL,
  method = c("dens", "hist"),
  show_max = TRUE,
  ...
)
```

## Arguments

- gobject:

  giotto object

- image_name:

  name of image object to use

- giottoLargeImage:

  giotto large image object

- method:

  plot type to show image intensity distribution

- show_max:

  logical. Plot the set max intensity as a vertical red line

- ...:

  additional params to pass to
  [`terra::hist()`](https://rspatial.github.io/terra/reference/hist.html)
  or
  [`terra::density()`](https://rspatial.github.io/terra/reference/density.html)

## Value

density or histogram plot

## Details

Plot is generated from a downsampling of the original image

## Plotting method `'dens'`

Density plot of intensity values for image objects. **N** total values
examined. **Bandwidth** refers to the curve smoothing value applied.

## Plotting method `'hist'`

Histogram of intensity values for image objects.

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")

distGiottoImage(g, image_name = "image")
```
