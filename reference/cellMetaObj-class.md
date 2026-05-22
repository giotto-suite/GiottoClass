# S4 cellMetaObj

Framework to store cell metadata

## Value

cellMetaObj

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
GiottoData::loadSubObjectMini("cellMetaObj")
#> An object of class cellMetaObj 
#> spat_unit : "aggregate"
#> feat_type : "rna"
#> provenance: z0 z1 
#> dimensions: 462 6 
#> 
#>                                    cell_ID nr_feats perc_feats total_expr
#>                                     <char>    <int>      <num>      <num>
#> 1: 240649020551054330404932383065726870513        5   1.483680   49.40986
#> 2: 274176126496863898679934791272921588227       27   8.011869  191.50684
#> 3: 323754550002953984063006506310071917306       23   6.824926  173.86955
#>    leiden_clus louvain_clus
#>          <num>        <num>
#> 1:           2            0
#> 2:           2            3
#> 3:           4            8
```
