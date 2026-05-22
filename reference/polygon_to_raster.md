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
#> $raster
#> class       : SpatRaster
#> size        : 3, 4, 1  (nrow, ncol, nlyr)
#> resolution  : 1, 1  (x, y)
#> extent      : 1, 5, 1, 4  (xmin, xmax, ymin, ymax)
#> coord. ref. : 
#> source(s)   : memory
#> name        : poly_i
#> min value   :      1
#> max value   :      1
#> 
#> $ID_vector
#> 1 
#> 1 
#> 
```
