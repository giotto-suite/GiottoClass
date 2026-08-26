# Set expression data

Function to set expression values for giotto object.

## Usage

``` r
setExpression(
  gobject,
  x,
  spat_unit = NULL,
  feat_type = NULL,
  name = "raw",
  provenance = NULL,
  verbose = TRUE,
  initialize = TRUE,
  ...
)
```

## Arguments

- gobject:

  giotto object

- x:

  exprObj or list of exprObj to set. Passing NULL will remove a
  specified set of expression data from the giotto object

- spat_unit:

  spatial unit (e.g. "cell")

- feat_type:

  feature type (e.g. "rna", "dna", "protein")

- name:

  name for the expression information

- provenance:

  provenance information (optional) information for the giotto object.
  Pass NULL to remove an expression object

- verbose:

  be verbose

- initialize:

  (default = FALSE) whether to initialize the gobject before returning

- ...:

  additional params to pass

## Value

giotto object

## See also

Other expression accessor functions:
[`getExpression()`](https://giotto-suite.github.io/GiottoClass/reference/getExpression.md)

Other functions to set data in giotto object:
[`setCellMetadata()`](https://giotto-suite.github.io/GiottoClass/reference/setCellMetadata.md),
[`setDimReduction()`](https://giotto-suite.github.io/GiottoClass/reference/setDimReduction.md),
[`setFeatureInfo()`](https://giotto-suite.github.io/GiottoClass/reference/setFeatureInfo.md),
[`setFeatureMetadata()`](https://giotto-suite.github.io/GiottoClass/reference/setFeatureMetadata.md),
[`setGiotto,giotto,giottoBinPoints-method`](https://giotto-suite.github.io/GiottoClass/reference/setGiotto.md),
[`setGiottoImage()`](https://giotto-suite.github.io/GiottoClass/reference/setGiottoImage.md),
[`setMultiomics()`](https://giotto-suite.github.io/GiottoClass/reference/setMultiomics.md),
[`setNearestNetwork()`](https://giotto-suite.github.io/GiottoClass/reference/setNearestNetwork.md),
[`setPolygonInfo()`](https://giotto-suite.github.io/GiottoClass/reference/setPolygonInfo.md),
[`setSpatialEnrichment()`](https://giotto-suite.github.io/GiottoClass/reference/setSpatialEnrichment.md),
[`setSpatialGrid()`](https://giotto-suite.github.io/GiottoClass/reference/setSpatialGrid.md),
[`setSpatialLocations()`](https://giotto-suite.github.io/GiottoClass/reference/setSpatialLocations.md),
[`setSpatialNetwork()`](https://giotto-suite.github.io/GiottoClass/reference/setSpatialNetwork.md),
[`set_multiomics()`](https://giotto-suite.github.io/GiottoClass/reference/set_multiomics.md)

## Examples

``` r
g <- createGiottoObject()
#> python already initialized in this session
#>  active environment : '/usr/bin/python3'
#>  python version : 3.12
m <- matrix(rnorm(100), nrow = 10)
colnames(m) <- paste0("cell_", seq_len(10))
rownames(m) <- paste0("feat_", seq_len(10))

g <- setExpression(gobject = g, x = createExprObj(m, name = "raw"))
#> Setting expression [cell][rna] raw
```
