# list_images

Prints the available giotto images that are attached to the Giotto
object

## Usage

``` r
list_images(gobject, img_type = NULL)
```

## Arguments

- gobject:

  giotto object

- img_type:

  "image" or "largeImage"

## Value

data.table of giotto image names attached to the giotto object

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")
list_images(g)
list_images(g, img_type = "largeImage")
```
