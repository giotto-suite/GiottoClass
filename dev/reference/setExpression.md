# Set expression data

Function to set expression values for `giotto` object.

## Usage

``` r
setExpression(gobject, x, spat_unit = NULL, feat_type = NULL, name = NULL, ...)

# S4 method for class 'giotto'
setExpression(
  gobject,
  x,
  spat_unit = NULL,
  feat_type = NULL,
  name = NULL,
  provenance = NULL,
  write = FALSE,
  verbose = NULL,
  initialize = TRUE,
  ...
)
```

## Arguments

- gobject:

  giotto object

- x:

  `exprObj` or `list` of `exprObj` to set. Passing `NULL` will remove a
  specified set of expression data from the `giotto` object

- spat_unit:

  spatial unit (e.g. "cell")

- feat_type:

  feature type (e.g. "rna", "dna", "protein")

- name:

  `character`. Name for the expression information. `NULL` (default)
  takes the name from `x`, falling back to `"raw"` when `x` is unnamed.

- ...:

  additional params to pass (none implemented)

- provenance:

  provenance information (optional)

- write:

  `logical` (default = `FALSE`). For `gsource` managed projects, whether
  to force a write of `x`.

- verbose:

  verbosity

- initialize:

  (default = FALSE) whether to initialize the gobject before returning

## Value

giotto object

## See also

Other expression accessor functions:
[`getExpression()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getExpression.md)

Other functions to set data in giotto object:
[`setCellMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setCellMetadata.md),
[`setDimReduction()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setDimReduction.md),
[`setFeatureInfo()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setFeatureInfo.md),
[`setFeatureMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setFeatureMetadata.md),
[`setGiotto,giotto,giottoBinPoints-method`](https://giotto-suite.github.io/GiottoClass/dev/reference/setGiotto.md),
[`setGiottoImage()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setGiottoImage.md),
[`setMultiomics()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setMultiomics.md),
[`setNearestNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setNearestNetwork.md),
[`setPolygonInfo()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setPolygonInfo.md),
[`setSpatialEnrichment()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setSpatialEnrichment.md),
[`setSpatialGrid()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setSpatialGrid.md),
[`setSpatialLocations()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setSpatialLocations.md),
[`setSpatialNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setSpatialNetwork.md),
[`set_multiomics()`](https://giotto-suite.github.io/GiottoClass/dev/reference/set_multiomics.md)

## Examples

``` r
g <- createGiottoObject()
m <- matrix(rnorm(100), nrow = 10)
colnames(m) <- paste0("cell_", seq_len(10))
rownames(m) <- paste0("feat_", seq_len(10))

g <- setExpression(gobject = g, x = createExprObj(m, name = "raw"))
```
