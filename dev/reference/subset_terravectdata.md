# Subset terraVectData subobjects

Subset `giottoPolygon` and `giottoPoints` by cell or feature IDs. All
relevant slots (geometries, centroids, overlaps) are subsetted
consistently. Intended as a stable integration point for `subsetGiotto`
and extending packages — the `[` methods remain the low-level workhorse
for logical/numeric indexing.

## Usage

``` r
# S4 method for class 'giottoPolygon'
subset(x, cell_ids = NULL, feat_ids = NULL, feat_type = NULL, ...)

# S4 method for class 'giottoPoints'
subset(x, feat_ids = NULL, negate = FALSE, ...)
```

## Arguments

- x:

  a `giottoPolygon` or `giottoPoints` object

- cell_ids:

  character. Polygon IDs to keep (for `giottoPolygon`)

- feat_ids:

  character. Feature IDs to keep

- feat_type:

  character. Feature type(s) to subset overlaps within. Use `":all:"`
  for all feature types.

- ...:

  not used

## Value

object of same class as `x`, subsetted

## Examples

``` r
gpoly <- GiottoData::loadSubObjectMini("giottoPolygon")
subset(gpoly, cell_ids = spatIDs(gpoly)[1:10])
subset(gpoly, cell_ids = spatIDs(gpoly)[1:10], negate = TRUE)

gpoints <- GiottoData::loadSubObjectMini("giottoPoints")
subset(gpoints, feat_ids = "Adgrl1")
```
