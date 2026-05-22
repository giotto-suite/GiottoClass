# Read spatial enrichment

read spatial enrichment results from list

## Usage

``` r
readSpatEnrichData(
  data_list,
  default_spat_unit = NULL,
  default_feat_type = NULL,
  provenance = NULL,
  verbose = TRUE
)
```

## Arguments

- data_list:

  (nested) list of spatial enrichment input data

- default_spat_unit:

  (optional) default spat_unit to use

- default_feat_type:

  (optional) default feat_type to use

- provenance:

  (optional) provenance information

- verbose:

  be verbose

## Value

spatEnrObj

## Examples

``` r
x <- GiottoData::loadSubObjectMini("spatEnrObj")

readSpatEnrichData(x)
#> [[1]]
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
#> 
```
