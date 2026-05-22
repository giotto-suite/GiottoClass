# createGiottoImage

**SUPERSEDED**
*[`createGiottoLargeImage()`](https://giotto-suite.github.io/GiottoClass/reference/createGiottoLargeImage.md)
is preferred for most uses*. This function may be renamed and/or
replaced in the future.

Creates a
[giottoImage](https://giotto-suite.github.io/GiottoClass/reference/giottoImage-class.md)
that can be added to a Giotto object and/or used to add an image to the
spatial plotting functions.

## Usage

``` r
createGiottoImage(
  gobject = NULL,
  spat_unit = NULL,
  spatial_locs = NULL,
  spat_loc_name = NULL,
  mg_object,
  name = "image",
  image_transformations = NULL,
  negative_y = TRUE,
  do_manual_adj = FALSE,
  xmax_adj = 0,
  xmin_adj = 0,
  ymax_adj = 0,
  ymin_adj = 0,
  scale_factor = 1,
  x_shift = NULL,
  y_shift = NULL,
  scale_x = NULL,
  scale_y = NULL,
  order = c("first_scale", "first_adj"),
  xmin_set = NULL,
  xmax_set = NULL,
  ymin_set = NULL,
  ymax_set = NULL,
  verbose = TRUE
)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit (e.g. "cell")

- spatial_locs:

  spatial locations (alternative if `gobject = NULL`)

- spat_loc_name:

  name of spatial locations within gobject

- mg_object:

  magick image object

- name:

  name for the image

- image_transformations:

  vector of sequential image transformations

- negative_y:

  Map image to negative y spatial values if TRUE during automatic
  alignment. Meaning that origin is in upper left instead of lower left.

- do_manual_adj:

  (default = FALSE) flag to use manual adj values instead of automatic
  alignment when given a gobject or spatlocs

- xmax_adj, xmin_adj, ymax_adj, ymin_adj:

  adjustment of the maximum or maximum x or y-value to align the image

- scale_factor:

  scaling of image dimensions relative to spatial coordinates

- x_shift, y_shift:

  shift image along x or y axes

- scale_x, scale_y:

  independently scale image in x or y direction

- order:

  perform scaling or adjustments and shifts first

- xmin_set, xmax_set, ymin_set, ymax_set:

  values to override image minmax spatial anchors when doing adjustments

- verbose:

  be verbose

## Value

a giottoImage object

## Details

image_transformations: transformation options from magick library
\[**flip_x_axis**\] flip x-axis
([`image_flop`](https://docs.ropensci.org/magick/reference/transform.html))
\[**flip_y_axis**\] flip y-axis
([`image_flip`](https://docs.ropensci.org/magick/reference/transform.html))
Example: image_transformations = c(flip_x_axis, flip_y_axis); first flip
x-axis and then y-axis

## Examples

``` r
image_test <- system.file("extdata/toy_intensity.tif",
    package = "GiottoClass"
)

createGiottoImage(mg_object = image_test)
#> Warning: gobject or spatial locations are not provided 
#> Arbitrary values will be given 
#> An object of class ' giottoImage ' with name  image 
#>  
#> Min and max values are: 
#>  Max on x-axis:  10 
#>  Min on x-axis:  0 
#>  Max on y-axis:  10 
#>  Min on y-axis:  0 
#>  
#> Boundary adjustment are: 
#>  Max adjustment on x-axis:  0 
#>  Min adjustment on x-axis:  0 
#>  Max adjustment on y-axis:  0 
#>  Min adjustment on y-axis:  0 
#>  
#> Boundaries are: 
#>  Image x-axis max boundary:  10 
#>  Image x-axis min boundary:  0 
#>  Image y-axis max boundary:  10 
#>  Image y-axis min boundary:  0 
#>  
#> Scale factor: 
#> x y 
#> 1 1 
#> 
#>  Resolution: 
#> x y 
#> 1 1 
#> 
#>  File Path: 
#> [1] "/home/runner/work/_temp/Library/GiottoClass/extdata/toy_intensity.tif"
```
