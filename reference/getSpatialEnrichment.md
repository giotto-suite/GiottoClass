# Get spatial enrichment

Function to get a spatial enrichment data.table

## Usage

``` r
getSpatialEnrichment(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  name = "DWLS",
  output = c("spatEnrObj", "data.table"),
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

- name:

  name of spatial enrichment results. Default "DWLS"

- output:

  what format in which to get information (e.g. "data.table")

- copy_obj:

  whether to deep copy/duplicate when getting the object (default =
  TRUE)

- set_defaults:

  set default spat_unit and feat_type. Change to FALSE only when
  expression and spat_info are not expected to exist.

## Value

spatEnrObj or data.table with fractions

## See also

Other spatial enrichment data accessor functions:
[`setSpatialEnrichment()`](https://giotto-suite.github.io/GiottoClass/reference/setSpatialEnrichment.md)

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

getSpatialEnrichment(g, spat_unit = "aggregate", name = "cluster_metagene")
#> An object of class spatEnrObj : "cluster_metagene"
#> spat_unit : "aggregate"
#> feat_type : "rna"
#> provenance: z0 z1 
#>    ------------------------
#> 
#> preview:
#>           1         2         3         4     5
#>       <num>     <num>     <num>     <num> <num>
#> 1: 1.014837 0.0000000 0.3160792 0.0000000     0
#> 2: 3.207415 0.9579716 0.6728505 0.2132677     0
#> 3: 3.953661 0.4604975 0.0000000 0.2302488     0
#>                                    cell_ID
#>                                     <char>
#> 1: 240649020551054330404932383065726870513
#> 2: 274176126496863898679934791272921588227
#> 3: 323754550002953984063006506310071917306
#> 
#> ...first 20 remaining colnames:
#> 
#>   
#> 
#> 
```
