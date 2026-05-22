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
[`setCellMetadata()`](https://giotto-suite.github.io/GiottoClass/reference/setCellMetadata.md),
[`setDimReduction()`](https://giotto-suite.github.io/GiottoClass/reference/setDimReduction.md),
[`setExpression()`](https://giotto-suite.github.io/GiottoClass/reference/setExpression.md),
[`setFeatureInfo()`](https://giotto-suite.github.io/GiottoClass/reference/setFeatureInfo.md),
[`setFeatureMetadata()`](https://giotto-suite.github.io/GiottoClass/reference/setFeatureMetadata.md),
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
#>  active environment : 'giotto_env'
#>  python version : 3.10
g_expression <- GiottoData::loadSubObjectMini("exprObj")

setGiotto(gobject = g, x = g_expression)
#> Setting expression [aggregate][rna] normalized
#> An object of class giotto 
#> >Active spat_unit:  aggregate 
#> >Active feat_type:  rna 
#> dimensions    : 337, 462 (features, cells)
#> [SUBCELLULAR INFO]
#> [AGGREGATE INFO]
#> expression -----------------------
#>   [aggregate][rna] normalized
#> 
#> 
#> Use objHistory() to see steps and params used
```
