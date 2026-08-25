# Get the area of individual polygons

Compute the area covered by polygons

## Usage

``` r
# S4 method for class 'giottoPolygon'
expanse(x, output = c("data.table", "named", "vector"), ...)

# S4 method for class 'giottoPolygon'
area(x, ...)
```

## Arguments

- x:

  `giottoPolygon`

- output:

  one of `"data.table"` (default), `"named"`, or `"vector"`.
  `"data.table"` returns a `data.table` with columns `cell_ID` and
  `area`. `"named"` returns a named numeric vector with `cell_ID` as
  names. `"vector"` returns a plain unnamed numeric vector
  (terra-compatible).

- ...:

  Arguments passed on to
  [`terra::expanse`](https://rspatial.github.io/terra/reference/expanse.html)

  `unit`

  :   character. Output unit of area. One of "m", "km", or "ha"

  `transform`

  :   logical. If `TRUE`, planar CRS are transformed to lon/lat for
      accuracy

  `byValue`

  :   logical. If `TRUE`, the area for each unique cell value is
      returned

  `zones`

  :   NULL or SpatRaster with the same geometry identifying zones in `x`

  `wide`

  :   logical. Should the results be in "wide" rather than "long"
      format?

  `usenames`

  :   logical. If `TRUE` layers are identified by their names instead of
      their numbers

## Value

depends on `output`: a `data.table`, named `numeric`, or `numeric`

## Details

Giotto's `expanse()` method dispatches on terra's generic. When the
underlying data is a `SpatVector`, `transform` defaults to `FALSE` and
CRS warnings are suppressed — both appropriate for biological coordinate
systems. When the underlying data is a non-terra representation (e.g. a
disk-backed store), the call is forwarded to a method for that class.

## Examples

``` r
sl <- GiottoData::loadSubObjectMini("spatLocsObj")
gpoly <- GiottoData::loadSubObjectMini("giottoPolygon")
gpoints <- GiottoData::loadSubObjectMini("giottoPoints")

# area of polygons
head(expanse(gpoly))

# area of the convex hull
expanse(hull(sl))
feature_hulls <- hull(gpoints, by = "feat_ID")
head(expanse(feature_hulls))

# output formats
expanse(gpoly, output = "named")
expanse(gpoly, output = "vector")
```
