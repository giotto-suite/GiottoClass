# createGiottoImage

**SUPERSEDED**
*[`createGiottoLargeImage()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createGiottoLargeImage.md)
is preferred for most uses*. This function may be renamed and/or
replaced in the future.

Creates a
[giottoImage](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoImage-class.md)
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
```
