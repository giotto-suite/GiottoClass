# Set feature info

Set giotto polygon spatVector for features

## Usage

``` r
setFeatureInfo(gobject, x, feat_type = NULL, ...)

# S4 method for class 'giotto'
setFeatureInfo(
  gobject,
  x,
  feat_type = NULL,
  verbose = NULL,
  initialize = TRUE,
  ...
)
```

## Arguments

- gobject:

  giotto object

- x:

  giottoPoints object or list of giottoPoints to set. Passing NULL will
  remove the specified giottoPoints object from the giotto object

- feat_type:

  feature type (e.g. "rna", "dna", "protein")

- ...:

  additional params to pass

- verbose:

  be verbose

- initialize:

  (default = FALSE) whether to initialize the gobject before returning

## Value

giotto object

## See also

Other feature info data accessor functions:
[`getFeatureInfo()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getFeatureInfo.md)

Other functions to set data in giotto object:
[`setCellMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setCellMetadata.md),
[`setDimReduction()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setDimReduction.md),
[`setExpression()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setExpression.md),
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
g <- GiottoData::loadGiottoMini("vizgen")
featinfo <- getFeatureInfo(g, return_giottoPoints = TRUE)

setFeatureInfo(gobject = g, x = featinfo)
```
