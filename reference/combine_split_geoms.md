# Combine or Split Complex Geometries

Geometries can be either single/simple or multi with multiple closed
rings defined as a single record. `combineGeom()` is used to combine
polygons. `splitGeom()` breaks combined geometries down into constituent
parts.  
Avoid using the `SpatVector` methods. They are lower-level and does not
deal with IDs like might be expected by Giotto.

## Usage

``` r
# S4 method for class 'giottoPolygon'
combineGeom(
  x,
  by = NULL,
  dissolve = FALSE,
  fun = "mean",
  ...,
  fmt = "poly_%d",
  previous_id = "source_id"
)

# S4 method for class 'giottoPolygon'
splitGeom(x, fmt = "poly_%d", previous_id = "source_id", ...)

# S4 method for class 'SpatVector'
combineGeom(x, by = NULL, dissolve = FALSE, fun = "mean", ...)

# S4 method for class 'SpatVector'
splitGeom(x, ...)
```

## Arguments

- x:

  geometry class to combine or split.

- by:

  character. Column name of variable used to group the geometries. Will
  be used as the new `poly_ID` column. All geometries will be combined
  if not provided.

- dissolve:

  logical. Should borders between aggregated geometries be dissolved?

- fun:

  function used to aggregate values. Either an actual function, or for
  the following, their name: "mean", "max", "min", "median", "sum",
  "modal", "any", "all", "none", "prod", "which.min", "which.max",
  "table", "sd" (sample standard deviation) and "std" (population
  standard deviation)

- ...:

  additional params to pass to
  [`terra::aggregate()`](https://rspatial.github.io/terra/reference/aggregate.html)
  (and then to `fun`, such as `na.rm=TRUE`) or
  [`terra::disagg()`](https://rspatial.github.io/terra/reference/disaggregate.html)

- fmt:

  character. sprintf formatting to use to generate `poly_ID` column
  values if no attributes are retained after combining.

- previous_id:

  character. If not `NULL`, column name to store original poly_ID values
  under. Note that merged IDs will be `NA`.

## Value

the same class as `x`

## Details

Currently, these are simple wrappers around terra's
`aggregate(dissolve = FALSE)` and `disagg()` with some additional
handling around the `poly_ID` column and a different name to avoid
confusion with spatial feature aggregation.

## Functions

- `combineGeom(SpatVector)`: Avoid using.

- `splitGeom(SpatVector)`: Avoid using.

## Geometry Attributes Handling

- `combineGeom()` attributes are only kept if `by` param is used. For
  conflicts where more than one value is present for a single variable
  after combining, they are resolved using `fun` param. When `by=NULL`,
  all attributes are dropped and a default `poly_ID` column is generated
  based on the `fmt` param.

- `splitGeom()` attributes are duplicated for each part that was part of
  the same multipolygon. The `poly_ID` column will be made unique.

- overlaps and centroids information will be dropped after either
  operation.

## Examples

``` r
dt <- data.table::data.table(
    id = c(
        rep('a', 3), # Triangle (id 'a')
        rep('b', 4), # Square 1 (id 'b')
        rep('c', 4), # Square 2 (id 'c')
        rep('d', 4) # Square 3 (id 'd')
    ),
    x = c(
        0, 1, 0.5,
        2, 5, 5, 2,
        5, 5, 6, 6,
        6, 7, 7, 6
    ),
    y = c(
        0, 0, 1,
        2, 2, 5, 5,
        2, 3, 3, 2,
        5, 5, 6, 6
    )
)
plot_colors <- getRainbowColors(4)
gpoly <- createGiottoPolygon(dt, verbose = FALSE)
plot(gpoly, col = plot_colors)


gpoly$group_id <- sprintf("group_%d", c(1, 2, 2, 3))
gpoly$values <- 1:4
force(gpoly)
#> An object of class giottoPolygon
#> spat_unit : "cell"
#> Spatial Information:
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 4, 3  (geometries, attributes)
#> extent      : 0, 7, 0, 6  (xmin, xmax, ymin, ymax)
#> coord. ref. : 
#> names       : poly_ID group_id values
#> type        :   <chr>    <chr>  <int>
#> values      :       a  group_1      1
#>                     b  group_2      2
#>                     c  group_2      3
#>               ...
#>  centroids   : NULL
#>  overlaps    : NULL

c_all <- combineGeom(gpoly) # combine all
force(c_all)
#> An object of class giottoPolygon
#> spat_unit : "cell"
#> Spatial Information:
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 1, 1  (geometries, attributes)
#> extent      : 0, 7, 0, 6  (xmin, xmax, ymin, ymax)
#> coord. ref. : 
#> names       : poly_ID
#> type        :   <chr>
#> values      :  poly_1
#>  centroids   : NULL
#>  overlaps    : NULL
plot(c_all, col = plot_colors)


c_gid <- combineGeom(gpoly, by = "group_id")
force(c_gid)
#> An object of class giottoPolygon
#> spat_unit : "cell"
#> Spatial Information:
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 3, 4  (geometries, attributes)
#> extent      : 0, 7, 0, 6  (xmin, xmax, ymin, ymax)
#> coord. ref. : 
#> names       : poly_ID mean_values source_id agg_n
#> type        :   <chr>       <num>     <chr> <int>
#> values      : group_1           1         a     1
#>               group_2         2.5        NA     2
#>               group_3           4         d     1
#>  centroids   : NULL
#>  overlaps    : NULL
plot(c_gid, col = plot_colors)

# `dissolve` removes touching boundaries
plot(combineGeom(gpoly, by = "group_id", dissolve = TRUE),
     col = plot_colors)


# split combined geometries
s_cgid <- splitGeom(c_gid)
force(s_cgid)
#> An object of class giottoPolygon
#> spat_unit : "cell"
#> Spatial Information:
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 4, 4  (geometries, attributes)
#> extent      : 0, 7, 0, 6  (xmin, xmax, ymin, ymax)
#> coord. ref. : 
#> names       : poly_ID mean_values source_id agg_n
#> type        :   <chr>       <num>     <chr> <int>
#> values      :  poly_1           1   group_1     1
#>                poly_2         2.5   group_2     2
#>                poly_3         2.5   group_2     2
#>               ...
#>  centroids   : NULL
#>  overlaps    : NULL
plot(s_cgid, col = plot_colors)
```
