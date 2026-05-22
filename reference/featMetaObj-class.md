# S4 featMetaObj

Framework to store feature metadata

## Value

featMetaObj

## Slots

- `metaDT`:

  metadata info

- `col_desc`:

  (optional) character vector describing columns of the metadata

- `spat_unit`:

  spatial unit of aggregated expression (e.g. 'cell')

- `feat_type`:

  feature type of aggregated expression (e.g. 'rna', 'protein')

- `provenance`:

  origin data of aggregated expression information (if applicable)

## Examples

``` r
GiottoData::loadSubObjectMini("featMetaObj")
#> An object of class featMetaObj 
#> spat_unit : "aggregate"
#> feat_type : "rna"
#> provenance: z0 z1 
#> 
#>    feat_ID nr_cells perc_cells total_expr mean_expr mean_expr_det hvg_orig
#>     <char>    <int>      <num>      <num>     <num>         <num>   <char>
#> 1:    Mlc1       80   17.31602   526.5915  1.139808      6.582394       no
#> 2:  Gprc5b      138   29.87013   944.0153  2.043323      6.840690       no
#> 3:    Gfap      151   32.68398  1229.1943  2.660594      8.140359      yes
#>          var    hvf
#>        <num> <char>
#> 1:  1.815718    yes
#> 2:  2.388911    yes
#> 3: 12.899599    yes
```
