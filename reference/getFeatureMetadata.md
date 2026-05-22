# getFeatureMetadata

Get feature metadata from giotto object

## Usage

``` r
getFeatureMetadata(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  output = c("featMetaObj", "data.table"),
  copy_obj = TRUE,
  set_defaults = TRUE
)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit (e.g. "cell")

- feat_type:

  feature type (e.g. "rna", "dna", "protein")

- output:

  return as either 'data.table' or 'featMetaObj'

- copy_obj:

  whether to perform a deepcopy of the data.table information

- set_defaults:

  set default spat_unit and feat_type. Change to FALSE only when
  expression and spat_info are not expected to exist.

## Value

a data.table or featMetaObj

## See also

fDataDT

Other functions to get data from giotto object:
[`getCellMetadata()`](https://giotto-suite.github.io/GiottoClass/reference/getCellMetadata.md),
[`getDimReduction()`](https://giotto-suite.github.io/GiottoClass/reference/getDimReduction.md),
[`getExpression()`](https://giotto-suite.github.io/GiottoClass/reference/getExpression.md),
[`getFeatureInfo()`](https://giotto-suite.github.io/GiottoClass/reference/getFeatureInfo.md),
[`getGiottoImage()`](https://giotto-suite.github.io/GiottoClass/reference/getGiottoImage.md),
[`getMultiomics()`](https://giotto-suite.github.io/GiottoClass/reference/getMultiomics.md),
[`getNearestNetwork()`](https://giotto-suite.github.io/GiottoClass/reference/getNearestNetwork.md),
[`getPolygonInfo()`](https://giotto-suite.github.io/GiottoClass/reference/getPolygonInfo.md),
[`getSpatialEnrichment()`](https://giotto-suite.github.io/GiottoClass/reference/getSpatialEnrichment.md),
[`getSpatialGrid()`](https://giotto-suite.github.io/GiottoClass/reference/getSpatialGrid.md),
[`getSpatialLocations()`](https://giotto-suite.github.io/GiottoClass/reference/getSpatialLocations.md),
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
#>  active environment : 'giotto_env'
#>  python version : 3.10

getFeatureMetadata(g)
#> An object of class featMetaObj 
#> spat_unit : "z0"
#> feat_type : "rna"
#> provenance: z0 
#> 
#>    feat_ID
#>     <char>
#> 1:    Mlc1
#> 2:  Gprc5b
#> 3:    Gfap
```
