# Get spatial network

Function to get a spatial network

## Usage

``` r
getSpatialNetwork(
  gobject,
  spat_unit = NULL,
  name = NULL,
  output = c("spatialNetworkObj", "networkDT", "networkDT_before_filter", "outputObj"),
  set_defaults = TRUE,
  copy_obj = TRUE,
  verbose = TRUE,
  simplify = TRUE
)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit (e.g. "cell")

- name:

  name of spatial network

- output:

  object type to return as. Options: 'spatialNetworkObj' (default),
  'networkDT' and 'networkDT_before_filter' for data.table outputs.

- set_defaults:

  set default spat_unit and feat_type. Change to FALSE only when
  expression and spat_info are not expected to exist.

- copy_obj:

  whether to copy/duplicate when getting the object (default = TRUE)

- verbose:

  be verbose

- simplify:

  logical. Whether or not to take object out of a list when there is a
  length of 1.

## Value

spatialNetworkObj of data.table

## See also

Other spatial network data accessor functions:
[`setSpatialNetwork()`](https://giotto-suite.github.io/GiottoClass/reference/setSpatialNetwork.md)

Other functions to get data from giotto object:
[`getCellMetadata()`](https://giotto-suite.github.io/GiottoClass/reference/getCellMetadata.md),
[`getDimReduction()`](https://giotto-suite.github.io/GiottoClass/reference/getDimReduction.md),
[`getExpression()`](https://giotto-suite.github.io/GiottoClass/reference/getExpression.md),
[`getFeatureInfo()`](https://giotto-suite.github.io/GiottoClass/reference/getFeatureInfo.md),
[`getFeatureMetadata()`](https://giotto-suite.github.io/GiottoClass/reference/getFeatureMetadata.md),
[`getGiottoImage()`](https://giotto-suite.github.io/GiottoClass/reference/getGiottoImage.md),
[`getMultiomics()`](https://giotto-suite.github.io/GiottoClass/reference/getMultiomics.md),
[`getNearestNetwork()`](https://giotto-suite.github.io/GiottoClass/reference/getNearestNetwork.md),
[`getPolygonInfo()`](https://giotto-suite.github.io/GiottoClass/reference/getPolygonInfo.md),
[`getSpatialEnrichment()`](https://giotto-suite.github.io/GiottoClass/reference/getSpatialEnrichment.md),
[`getSpatialGrid()`](https://giotto-suite.github.io/GiottoClass/reference/getSpatialGrid.md),
[`getSpatialLocations()`](https://giotto-suite.github.io/GiottoClass/reference/getSpatialLocations.md),
[`get_multiomics()`](https://giotto-suite.github.io/GiottoClass/reference/get_multiomics.md)

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : 'giotto_env'
#>  python version : 3.10

getSpatialNetwork(g)
#> An object of class spatialNetworkObj : "Delaunay_network"
#> Contains spatial network generated with: deldir 
#> spat_unit : "cell"
#> provenance: cell 
#>    1770 connections (filtered)
#>    1814 connections (before filter)
#> 
```
