# getCellMetadata

Get cell metadata from giotto object

## Usage

``` r
getCellMetadata(gobject, spat_unit = NULL, feat_type = NULL, ...)

# S4 method for class 'gAny'
getCellMetadata(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  output = c("cellMetaObj", "data.table"),
  copy_obj = TRUE,
  set_defaults = TRUE,
  view = NULL,
  space = NULL
)

# S4 method for class 'giottoMulti'
getCellMetadata(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  output = c("cellMetaObj", "data.table"),
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

  return as either 'data.table' or 'cellMetaObj'

- copy_obj:

  whether to deep copy/duplicate when getting the object (default =
  TRUE)

- set_defaults:

  set default spat_unit and feat_type. Change to FALSE only when
  expression and spat_info are not expected to exist.

- view:

  optional character(1) name of a slotted view; when supplied, the
  returned subobject is projected through the view's narrowing recipe
  via
  [`resolveSubobject()`](https://giotto-suite.github.io/GiottoClass/dev/reference/resolveSubobject.md)

- space:

  optional character(1) name of a slotted space; when supplied, applied
  alongside `view` (spaces are no-ops on tabular subobjects but the
  param is accepted for API symmetry)

- samples:

  (giottoMulti) character vector of sample names to slice the joint
  result to; `NULL` (default) returns all participants

- on_missing:

  (giottoMulti) how assembly treats a keyed child that cannot produce
  the requested content, and mismatched column sets: `"error"`
  (default), `"drop"` (intersect), or `"fill"` (union / NA)

## Value

a data.table or cellMetaObj

## See also

pDataDT

Other functions to get data from giotto object:
[`getDimReduction()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getDimReduction.md),
[`getExpression()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getExpression.md),
[`getFeatureInfo()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getFeatureInfo.md),
[`getFeatureMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getFeatureMetadata.md),
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
g <- GiottoData::loadGiottoMini("visium")

getCellMetadata(g)
```
