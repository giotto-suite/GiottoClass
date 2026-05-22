# getCellMetadata

Get cell metadata from giotto object

## Usage

``` r
getCellMetadata(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  output = c("cellMetaObj", "data.table"),
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

  return as either 'data.table' or 'cellMetaObj'

- copy_obj:

  whether to deep copy/duplicate when getting the object (default =
  TRUE)

- set_defaults:

  set default spat_unit and feat_type. Change to FALSE only when
  expression and spat_info are not expected to exist.

## Value

a data.table or cellMetaObj

## See also

pDataDT

Other functions to get data from giotto object:
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
[`getSpatialNetwork()`](https://giotto-suite.github.io/GiottoClass/reference/getSpatialNetwork.md),
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

getCellMetadata(g)
#> An object of class cellMetaObj 
#> spat_unit : "cell"
#> feat_type : "rna"
#> provenance: cell 
#> dimensions: 624 7 
#> 
#>               cell_ID in_tissue nr_feats perc_feats total_expr leiden_clus
#>                <char>     <int>    <int>      <num>      <num>       <num>
#> 1: AACTCGATGGCGCAGT-1         1      265   41.79811  1057.9308           2
#> 2: GGCTGGCTAGCTTAAA-1         1      279   44.00631  1064.7493           5
#> 3: GACGCCTGTTGCAGGG-1         1      219   34.54259   964.9294           2
#>    custom_leiden
#>            <num>
#> 1:             4
#> 2:             3
#> 3:             3
```
