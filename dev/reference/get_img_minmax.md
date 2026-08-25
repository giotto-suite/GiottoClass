# get_img_minmax

get_img_minmax

## Usage

``` r
get_img_minmax(mg_img, negative_y = TRUE)
```

## Arguments

- mg_img:

  magick object

- negative_y:

  Map image to negative y spatial values if TRUE during automatic
  alignment. Meaning that origin is in upper left instead of lower left.

## Value

numeric

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")
g_image <- getGiottoImage(g, name = "image")
mgimg <- as(g_image, "giottoImage")

get_img_minmax(slot(mgimg, "mg_object"))
```
