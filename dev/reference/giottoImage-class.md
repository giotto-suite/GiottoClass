# S4 giottoImage Class

Framework of giotto object to store and work with spatial expression
data

## Value

giottoImage

## Details

\[**mg_object**\] Core object is any image that can be read by the
magick package

\[**boundaries**\] Boundary adjustments can be used to manually or
automatically through a script adjust the image with the spatial data.

## Slots

- `name`:

  name of Giotto image

- `mg_object`:

  magick image object

- `minmax`:

  minimum and maximum of associated spatial location coordinates

- `boundaries`:

  x and y coordinate adjustments (default to 0)

- `scale_factor`:

  image scaling relative to spatial locations

- `resolution`:

  spatial location units covered per pixel

- `file_path`:

  file path to the image if given

- `OS_platform`:

  Operating System to run Giotto analysis on

## Examples

``` r
giottoImage()
```
