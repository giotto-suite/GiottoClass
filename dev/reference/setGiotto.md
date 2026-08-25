# Set giotto subobjects into giotto object

Set giotto subobjects into giotto object

## Usage

``` r
# S4 method for class 'giotto,giottoBinPoints'
setGiotto(gobject, x, ...)

# S4 method for class 'giotto,list'
setGiotto(gobject, x, verbose = TRUE, ...)

# S4 method for class 'giotto,cellMetaObj'
setGiotto(gobject, x, ...)

# S4 method for class 'giotto,featMetaObj'
setGiotto(gobject, x, ...)

# S4 method for class 'giotto,exprObj'
setGiotto(gobject, x, ...)

# S4 method for class 'giotto,giottoPoints'
setGiotto(gobject, x, ...)

# S4 method for class 'giotto,giottoPolygon'
setGiotto(gobject, x, ...)

# S4 method for class 'giotto,dimObj'
setGiotto(gobject, x, ...)

# S4 method for class 'giotto,spatLocsObj'
setGiotto(gobject, x, ...)

# S4 method for class 'giotto,spatEnrObj'
setGiotto(gobject, x, ...)

# S4 method for class 'giotto,nnNetObj'
setGiotto(gobject, x, ...)

# S4 method for class 'giotto,spatialNetworkObj'
setGiotto(gobject, x, ...)

# S4 method for class 'giotto,giottoLargeImage'
setGiotto(gobject, x, ...)

# S4 method for class 'giotto,giottoImage'
setGiotto(gobject, x, ...)
```

## Arguments

- gobject:

  giotto object

- x:

  giottoSubobject to set

- ...:

  additional params to pass to specific Giotto setter functions

- verbose:

  be verbose

## Value

giottoSubobject

## See also

Other functions to set data in giotto object:
[`setCellMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setCellMetadata.md),
[`setDimReduction()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setDimReduction.md),
[`setExpression()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setExpression.md),
[`setFeatureInfo()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setFeatureInfo.md),
[`setFeatureMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setFeatureMetadata.md),
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
g_expression <- GiottoData::loadSubObjectMini("exprObj")

setGiotto(gobject = g, x = g_expression)
```
