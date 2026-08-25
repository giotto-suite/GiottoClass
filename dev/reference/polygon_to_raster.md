# Convert polygon to raster

convert polygon to raster

## Usage

``` r
polygon_to_raster(polygon, field = NULL)
```

## Arguments

- polygon:

  SpatVector polygon to rasterize

- field:

  character. Name of attribute of polygon that should be used when
  rasterizing to assign values. Passing NULL uses the first attribute.

## Value

list

## Examples

``` r
df <- data.frame(x = 1:5, y = c(1, 4, 4, 3, 1))
d <- data.frame(id = 1, name = "polygon_1")
my_polygon <- terra::vect(as.matrix(df), type = "polygons", atts = d)

polygon_to_raster(my_polygon)
```
