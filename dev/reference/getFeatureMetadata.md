# getFeatureMetadata

Get feature metadata from giotto object

## Usage

``` r
getFeatureMetadata(gobject, spat_unit = NULL, feat_type = NULL, ...)

# S4 method for class 'gAny'
getFeatureMetadata(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  output = c("featMetaObj", "data.table"),
  copy_obj = TRUE,
  set_defaults = TRUE,
  view = NULL,
  space = NULL
)

# S4 method for class 'giottoMulti'
getFeatureMetadata(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  output = c("featMetaObj", "data.table"),
  copy_obj = TRUE,
  set_defaults = TRUE,
  samples = NULL,
  on_missing = c("error", "drop", "fill"),
  view = NULL
)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit (e.g. "cell")

- feat_type:

  feature type (e.g. "rna", "dna", "protein")

- ...:

  additional params to pass

- output:

  return as either 'data.table' or 'featMetaObj'

- copy_obj:

  whether to perform a deepcopy of the data.table information

- set_defaults:

  set default spat_unit and feat_type. Change to FALSE only when
  expression and spat_info are not expected to exist.

- view:

  optional character(1) name of a slotted view

- space:

  optional character(1) name of a slotted space (accepted for API
  symmetry; feature metadata is feat-keyed so view/space are typically
  no-ops here)

- samples:

  (giottoMulti) accepted for API symmetry; feature IDs are not
  sample-namespaced, so this only validates the names

- on_missing:

  (giottoMulti) how assembly treats a keyed child that cannot produce
  the requested content, and mismatched column sets: `"error"`
  (default), `"drop"` (intersect), or `"fill"` (union / NA)

## Value

a data.table or featMetaObj

## See also

fDataDT

Other functions to get data from giotto object:
[`getCellMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getCellMetadata.md),
[`getDimReduction()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getDimReduction.md),
[`getExpression()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getExpression.md),
[`getFeatureInfo()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getFeatureInfo.md),
[`getGiottoImage()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getGiottoImage.md),
[`getMultiomics()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getMultiomics.md),
[`getNearestNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getNearestNetwork.md),
[`getPolygonInfo()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getPolygonInfo.md),
[`getSpatialEnrichment()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getSpatialEnrichment.md),
[`getSpatialGrid()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getSpatialGrid.md),
[`getSpatialLocations()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getSpatialLocations.md),
[`getSpatialNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getSpatialNetwork.md),
[`get_multiomics()`](https://giotto-suite.github.io/GiottoClass/dev/reference/get_multiomics.md)

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")

getFeatureMetadata(g)
```
