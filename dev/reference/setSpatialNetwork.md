# Set spatial network

Function to set a spatial network

## Usage

``` r
setSpatialNetwork(gobject, x, spat_unit = NULL, name = NULL, ...)

# S4 method for class 'giotto'
setSpatialNetwork(
  gobject,
  x,
  spat_unit = NULL,
  name = NULL,
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

  spatialNetworkObj or list of spatialNetworkObj to set. Passing NULL
  removes a specified set of spatial network information from the
  gobject.

- spat_unit:

  spatial unit (e.g. "cell")

- name:

  name of spatial network

- ...:

  additional params to pass

- provenance:

  provenance name

- verbose:

  be verbose

- initialize:

  (default = FALSE) whether to initialize the gobject before returning

## Value

giotto object

## See also

Other spatial network data accessor functions:
[`getSpatialNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getSpatialNetwork.md)

Other functions to set data in giotto object:
[`setCellMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setCellMetadata.md),
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
[`set_multiomics()`](https://giotto-suite.github.io/GiottoClass/dev/reference/set_multiomics.md)

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")
spatnet <- getSpatialNetwork(g)

setSpatialNetwork(gobject = g, x = spatnet)
```
