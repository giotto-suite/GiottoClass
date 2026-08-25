# addGiottoImage

Adds lists of giottoImages and giottoLargeImages to gobjects

## Usage

``` r
addGiottoImage(
  gobject = NULL,
  images = NULL,
  largeImages = NULL,
  spat_loc_name = NULL,
  scale_factor = NULL,
  negative_y = TRUE
)
```

## Arguments

- gobject:

  gobject to add images objects to

- images:

  list of giotto images to add

- largeImages:

  deprecated

- spat_loc_name:

  provide spatial location slot in Giotto to align giottoImages.
  Defaults to first one

- scale_factor:

  provide scale of image pixel dimensions relative to spatial
  coordinates.

- negative_y:

  Map image to negative y spatial values if TRUE during automatic
  alignment. Meaning that origin is in upper left instead of lower left.

## Value

an updated Giotto object with access to the list of images

## See also

Other basic image functions:
[`plotGiottoImage()`](https://giotto-suite.github.io/GiottoClass/dev/reference/plotGiottoImage.md),
[`reconnectGiottoImage()`](https://giotto-suite.github.io/GiottoClass/dev/reference/reconnectGiottoImage.md),
[`updateGiottoImage()`](https://giotto-suite.github.io/GiottoClass/dev/reference/updateGiottoImage.md)

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")
g_image <- getGiottoImage(g, name = list_images(g)$name[1])

addGiottoImage(g, largeImages = list(g_image))
```
