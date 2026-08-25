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
[`as.data.table.giottoBinPoints()`](https://giotto-suite.github.io/GiottoClass/dev/reference/as.data.table.md),
[`as.matrix`](https://giotto-suite.github.io/GiottoClass/dev/reference/as.matrix.md),
[`as.points`](https://giotto-suite.github.io/GiottoClass/dev/reference/as.points.md),
[`r_spatial_conversions`](https://giotto-suite.github.io/GiottoClass/dev/reference/r_spatial_conversions.md)

## Examples

``` r
g <- GiottoData::loadSubObjectMini("giottoPolygon")

as.polygons(slot(g, "spatVector"))
```
