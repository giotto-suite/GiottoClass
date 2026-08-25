# createGiottoLargeImage

Creates a large giotto image that can be added to a Giotto subcellular
object. Generates deep copy of SpatRaster

## Usage

``` r
createGiottoLargeImage(
  raster_object,
  name = "image",
  negative_y = TRUE,
  extent = NULL,
  use_rast_ext = FALSE,
  image_transformations = NULL,
  flip_vertical = FALSE,
  flip_horizontal = FALSE,
  xmax_bound = NULL,
  xmin_bound = NULL,
  ymax_bound = NULL,
  ymin_bound = NULL,
  scale_factor = 1,
  verbose = TRUE
)
```

## Arguments

- raster_object:

  filepath to an image, a terra `SpatRaster` or, other format openable
  via
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)

- name:

  name for the image

- negative_y:

  Map image to negative y spatial values if TRUE. Meaning that origin is
  in upper left instead of lower left.

- extent:

  SpatExtent object to assign spatial extent. Takes priority unless
  use_rast_ext is TRUE.

- use_rast_ext:

  Use extent from input raster object

- image_transformations:

  vector of sequential image transformations - under construction

- flip_vertical:

  flip raster in a vertical manner

- flip_horizontal:

  flip raster in a horizontal manner

- xmax_bound, xmin_bound, ymax_bound, ymin_bound:

  assign min and max x and y values for image spatial placement

- scale_factor:

  scaling of image dimensions relative to spatial coordinates

- verbose:

  be verbose

## Value

a giottoLargeImage object

## Examples

``` r
image_test <- system.file("extdata/toy_intensity.tif",
    package = "GiottoClass"
)

createGiottoLargeImage(raster_object = image_test)
```
