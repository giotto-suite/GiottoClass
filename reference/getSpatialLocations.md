# Get spatial locations

Function to get a spatial location data.table

## Usage

``` r
getSpatialLocations(
  gobject,
  spat_unit = NULL,
  name = NULL,
  output = c("spatLocsObj", "data.table"),
  copy_obj = TRUE,
  verbose = TRUE,
  set_defaults = TRUE,
  simplify = TRUE
)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit (e.g. "cell")

- name:

  name of spatial locations (defaults to first name in spatial_locs
  slot, e.g. "raw")

- output:

  what object type to get the spatial locations as. Default is as a
  'spatLocsObj'. Returning as 'data.table' is also possible.

- copy_obj:

  whether to copy/duplicate when getting the object (default = TRUE)

- verbose:

  be verbose

- set_defaults:

  set default spat_unit and feat_type. Change to FALSE only when
  expression and spat_info are not expected to exist.

- simplify:

  logical. Whether or not to take object out of a list when there is a
  length of 1.

## Value

data.table with coordinates or spatLocsObj depending on `output`

## See also

Other spatial location data accessor functions:
[`setSpatialLocations()`](https://giotto-suite.github.io/GiottoClass/reference/setSpatialLocations.md)

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
[`getSpatialNetwork()`](https://giotto-suite.github.io/GiottoClass/reference/getSpatialNetwork.md),
[`get_multiomics()`](https://giotto-suite.github.io/GiottoClass/reference/get_multiomics.md)

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : '/usr/bin/python3'
#>  python version : 3.12

getSpatialLocations(g)
#> An object of class spatLocsObj : "raw"
#> spat_unit : "z0"
#> provenance: z0 
#> dimensions: 498 3 
#> preview   :
#>       sdimx     sdimy                                 cell_ID
#>       <num>     <num>                                  <char>
#> 1: 6405.067 -4780.499  40951783403982682273285375368232495429
#> 2: 6426.020 -4972.519 240649020551054330404932383065726870513
#> 3: 6428.456 -4799.158 274176126496863898679934791272921588227
#> 
#> ranges:
#>         sdimx     sdimy
#> [1,] 6402.438 -5146.726
#> [2,] 6899.203 -4700.157
#> 
```
