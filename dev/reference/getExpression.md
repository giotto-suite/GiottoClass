# Get expression values

Function to get expression values from giotto object

## Usage

``` r
getExpression(gobject, spat_unit = NULL, feat_type = NULL, name = NULL, ...)

# S4 method for class 'giotto'
getExpression(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  name = NULL,
  values = NULL,
  output = c("exprObj", "matrix"),
  set_defaults = TRUE
)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit (e.g. "cell")

- feat_type:

  feature type (e.g. "rna", "dna", "protein")

- name:

  name of expression values to extract (e.g. `"raw"`, `"normalized"`,
  `"scaled"`). Canonical form, consistent with the rest of the slot
  accessors.

- ...:

  additional params to pass

- values:

  back-compat alias for `name`. If both are supplied they must agree.

- output:

  what object type to retrieve the expression as. Currently either
  matrix' for the matrix object contained in the exprObj or 'exprObj'
  (default) for the exprObj itself are allowed.

- set_defaults:

  set default spat_unit and feat_type. Change to FALSE only when
  expression and spat_info are not expected to exist.

## Value

exprObj or matrix depending on output param

## See also

Other expression accessor functions:
[`setExpression()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setExpression.md)

Other functions to get data from giotto object:
[`getCellMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getCellMetadata.md),
[`getDimReduction()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getDimReduction.md),
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

getExpression(g)
```
