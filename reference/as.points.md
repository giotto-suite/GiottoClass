# Coerce to SpatVector points

Coversion to a SpatVector of points.

## Usage

``` r
# S4 method for class 'data.frame'
as.points(x, include_values = TRUE, specific_values = NULL)

# S4 method for class 'spatLocsObj'
as.points(x)
```

## Arguments

- x:

  SpatRaster, SpatVector, SpatExtent, or correctly formatted data.frame

- include_values:

  `logical`. Whether to include additional columns other than the
  geometry information as `SpatVector` attributes. Default is TRUE.

- specific_values:

  `character`. Specific subset of columns to include as attributes if
  `include_values = TRUE`.

## Value

SpatVector points

## See also

[`terra::as.points()`](https://rspatial.github.io/terra/reference/as.points.html)

Other As coercion functions:
[`as.data.table.giottoBinPoints()`](https://giotto-suite.github.io/GiottoClass/reference/as.data.table.md),
[`as.matrix`](https://giotto-suite.github.io/GiottoClass/reference/as.matrix.md),
[`as.polygons`](https://giotto-suite.github.io/GiottoClass/reference/as.polygons.md),
[`r_spatial_conversions`](https://giotto-suite.github.io/GiottoClass/reference/r_spatial_conversions.md)

## Examples

``` r
g <- GiottoData::loadSubObjectMini("giottoPoints")

as.points(slot(g, "spatVector"))
#> Warning: [as.points] returning a copy
#> class       : SpatVector
#> geometry    : points
#> dimensions  : 79761, 3  (geometries, attributes)
#> extent      : 6400.037, 6900.032, -5149.983, -4699.979  (xmin, xmax, ymin, ymax)
#> coord. ref. : 
#> names       : feat_ID global_z feat_ID_uniq
#> type        :   <chr>    <int>        <int>
#> values      :    Mlc1        0            1
#>                Gprc5b        0            2
#>                  Gfap        0            3
#>               ...
```
