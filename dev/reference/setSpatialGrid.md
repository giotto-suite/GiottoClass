# Set spatial grid

Function to set a spatial grid

## Usage

``` r
setSpatialGrid(
  gobject,
  spatial_grid,
  spat_unit = NULL,
  feat_type = NULL,
  name = NULL,
  verbose = NULL,
  set_defaults = TRUE,
  ...
)
```

## Arguments

- gobject:

  giotto object

- spatial_grid:

  spatial grid object

- spat_unit:

  spatial unit (e.g. "cell")

- feat_type:

  feature type (e.g. "rna", "dna", "protein")

- name:

  name of spatial grid

- verbose:

  be verbose

- set_defaults:

  set default spat_unit and feat_type. Change to FALSE only when
  expression and spat_info are not expected to exist.

- ...:

  additional params to pass

## Value

giotto object

## See also

Other spatial grid data accessor functions:
[`getSpatialGrid()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getSpatialGrid.md)

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
[`setSpatialLocations()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setSpatialLocations.md),
[`setSpatialNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setSpatialNetwork.md),
[`set_multiomics()`](https://giotto-suite.github.io/GiottoClass/dev/reference/set_multiomics.md)

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")
g <- createSpatialGrid(g, sdimx_stepsize = 5, sdimy_stepsize = 5)
sg <- getSpatialGrid(g, return_grid_Obj = TRUE)

setSpatialGrid(gobject = g, spatial_grid = sg)
```
