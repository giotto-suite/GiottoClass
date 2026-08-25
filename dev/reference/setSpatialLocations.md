# Set spatial locations

Function to set a spatial location slot

## Usage

``` r
setSpatialLocations(gobject, x, spat_unit = NULL, name = NULL, ...)

# S4 method for class 'giotto'
setSpatialLocations(
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

  spatLocsObj or list of spatLocsObj. Passing NULL will remove a
  specified set of spatial locations data.

- spat_unit:

  spatial unit (e.g. "cell")

- name:

  name of spatial locations. NULL (default) takes the name from `x`,
  falling back to "raw" when `x` is unnamed

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

## Details

Spatial information will be set to the nested location described by
their tagged spat_unit and name information. An alternative location can
also be specified through the respective params in this function.

## See also

Other spatial location data accessor functions:
[`getSpatialLocations()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getSpatialLocations.md)

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
[`setSpatialNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setSpatialNetwork.md),
[`set_multiomics()`](https://giotto-suite.github.io/GiottoClass/dev/reference/set_multiomics.md)

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")
x <- getSpatialLocations(g, output = "data.table")
sl <- data.frame(cell_ID = x$cell_ID, sdimx = rnorm(624), sdimy = rnorm(624))

setSpatialLocations(gobject = g, x = createSpatLocsObj(sl, name = "raw"))
```
