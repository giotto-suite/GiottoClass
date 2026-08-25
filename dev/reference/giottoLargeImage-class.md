# S4 giottoLargeImage Class

Image class for Giotto that uses terra `SpatRaster` as a backend. If
images are loaded from a file on disk then they are worked with lazily,
where only the values needed at any moment are loaded/sampled into
memory. Since `SpatRaster` objects are C pointers, `giottoLargeImage`
and inheriting classes need to run
[`reconnect()`](https://giotto-suite.github.io/GiottoClass/dev/reference/reconnect.md)
after loading from a saved object.

## Value

giottoLargeImage

## Slots

- `name`:

  name of large Giotto image

- `raster_object`:

  terra `SpatRaster` object

- `extent`:

  tracks the extent of the raster object. Note that most processes
  should rely on the extent of the raster object instead of this.

- `overall_extent`:

  terra extent object covering the original extent of image

- `scale_factor`:

  image scaling relative to spatial locations

- `resolution`:

  spatial location units covered per pixel

- `max_intensity`:

  approximate maximum value

- `min_intensity`:

  approximate minimum value

- `max_window`:

  value to set as maximum intensity in color scaling

- `colors`:

  color mappings in hex codes

- `is_int`:

  values are integers

- `file_path`:

  file path to the image if given

- `OS_platform`:

  Operating System to run Giotto analysis on

## Examples

``` r
giottoLargeImage()
```
