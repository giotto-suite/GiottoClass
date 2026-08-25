# list_images_names

return the available image names for a given image type that are
attached to the Giotto object

## Usage

``` r
list_images_names(gobject, img_type = NULL)
```

## Arguments

- gobject:

  a giotto object

- img_type:

  passing NULL (default) gets all image names. Can further specify
  "image" or "largeImage"

## Value

vector with names of available image names

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")
list_images_names(g)
list_images_names(g, img_type = "largeImage")
```
