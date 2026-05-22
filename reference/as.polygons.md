# Coerce to SpatVector polygons

Coversion to a SpatVector of polygons.

## Usage

``` r
# S4 method for class 'data.frame'
as.polygons(
  x,
  include_values = TRUE,
  specific_values = NULL,
  sort_geom = FALSE
)
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

- sort_geom:

  `logical`. Whether to sort key the data.table input by 'geom', 'part',
  and 'hole' columns.

## Value

SpatVector polygons

## See also

[`terra::as.polygons()`](https://rspatial.github.io/terra/reference/as.polygons.html)

Other As coercion functions:
[`as.data.table.giottoBinPoints()`](https://giotto-suite.github.io/GiottoClass/reference/as.data.table.md),
[`as.matrix`](https://giotto-suite.github.io/GiottoClass/reference/as.matrix.md),
[`as.points`](https://giotto-suite.github.io/GiottoClass/reference/as.points.md),
[`r_spatial_conversions`](https://giotto-suite.github.io/GiottoClass/reference/r_spatial_conversions.md)

## Examples

``` r
g <- GiottoData::loadSubObjectMini("giottoPolygon")

as.polygons(slot(g, "spatVector"))
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 462, 4  (geometries, attributes)
#> extent      : 6391.466, 6903.573, -5153.897, -4694.868  (xmin, xmax, ymin, ymax)
#> coord. ref. : 
#> names       :                   poly_ID stack agg_n valid
#> type        :                     <chr> <chr> <int> <int>
#> values      : 100210519278873141813371~    NA     2     1
#>               101161259912191124732236~    NA     2     1
#>               101488859781016188084173~    NA     2     1
#>               ...
```
