# Create S4 spatEnrObj

Create S4 spatEnrObj

## Usage

``` r
createSpatEnrObj(
  enrichment_data,
  name = "test",
  spat_unit = "cell",
  feat_type = "rna",
  method = NULL,
  provenance = NULL,
  misc = NULL,
  verbose = TRUE
)
```

## Arguments

- enrichment_data:

  spatial enrichment results, provided a dataframe-like object

- name:

  name of S4 spatEnrObj

- spat_unit:

  spatial unit of aggregated expression (e.g. 'cell')

- feat_type:

  feature type of aggregated expression (e.g. 'rna', 'protein')

- method:

  method used to generate spatial enrichment information

- provenance:

  origin data of aggregated expression information (if applicable)

- misc:

  misc additional information about the spatial enrichment or how it was
  generated

- verbose:

  be verbose

## Value

spatEnrObj

## Examples

``` r
x <- GiottoData::loadSubObjectMini("spatEnrObj")

createSpatEnrObj(
    enrichment_data = slot(x, "enrichDT"),
    name = "cluster_metagene"
)
#> An object of class spatEnrObj : "cluster_metagene"
#> spat_unit : "cell"
#> feat_type : "rna"
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
