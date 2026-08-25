# Set feature metadata

Function to set feature metadata into giotto object

## Usage

``` r
setFeatureMetadata(gobject, x, spat_unit = NULL, feat_type = NULL, ...)

# S4 method for class 'giotto'
setFeatureMetadata(
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

  featMetaObj or list of featMetaObj to set. Passing NULL will reset a
  specified set of feature metadata in the giotto object.

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
[`setCellMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setCellMetadata.md),
[`setDimReduction()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setDimReduction.md),
[`setExpression()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setExpression.md),
[`setFeatureInfo()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setFeatureInfo.md),
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
g <- GiottoData::loadGiottoMini("vizgen")
m1 <- getFeatureMetadata(g, output = "data.table")
m2 <- data.frame(
    feat_ID = m1$feat_ID,
    new_column = paste0("gene_", m1$feat_ID)
)

setFeatureMetadata(gobject = g, x = createFeatMetaObj(m2))
```
