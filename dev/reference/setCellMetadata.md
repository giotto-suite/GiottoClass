# Set cell metadata

Function to set cell metadata into giotto object

## Usage

``` r
setCellMetadata(gobject, x, spat_unit = NULL, feat_type = NULL, ...)

# S4 method for class 'giotto'
setCellMetadata(
  gobject,
  x,
  spat_unit = NULL,
  feat_type = NULL,
  provenance = NULL,
  verbose = NULL,
  initialize = TRUE,
  ...
)
```

## Arguments

- gobject:

  giotto object

- x:

  cellMetaObj or list of cellMetaObj to set. Passing NULL will reset a
  specified set of cell metadata in the giotto object.

- spat_unit:

  spatial unit (e.g. "cell")

- feat_type:

  feature type (e.g. "rna", "dna", "protein")

- ...:

  additional params to pass

- provenance:

  provenance information (optional)

- verbose:

  be verbose

- initialize:

  (default = FALSE) whether to initialize the gobject before returning

## Value

giotto object

## See also

Other functions to set data in giotto object:
[`setDimReduction()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setDimReduction.md),
[`setExpression()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setExpression.md),
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
g <- GiottoData::loadGiottoMini("visium")
m1 <- getCellMetadata(g, output = "data.table")
m2 <- data.frame(
    cell_ID = m1$cell_ID,
    new_column = sample(letters, 624, replace = TRUE)
)

setCellMetadata(gobject = g, x = createCellMetaObj(m2))
```
