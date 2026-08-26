# calculateMetaTable

calculates the average gene expression for one or more (combined)
annotation columns.

## Usage

``` r
calculateMetaTable(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  expression_values = c("normalized", "scaled", "custom"),
  metadata_cols = NULL,
  selected_feats = NULL,
  selected_genes = NULL
)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit

- feat_type:

  feature type

- expression_values:

  expression values to use

- metadata_cols:

  annotation columns found in `pDataDT(gobject)`

- selected_feats:

  subset of features to use

- selected_genes:

  subset of genes to use (deprecated)

## Value

data.table with average expression values for each gene per (combined)
annotation

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : '/usr/bin/python3'
#>  python version : 3.12

calculateMetaTable(g, metadata_cols = "leiden_clus")
#>       leiden_clus uniq_ID      variable      value
#>             <num>   <num>        <fctr>      <num>
#>    1:           2       2         Gna12 2.95192565
#>    2:           5       5         Gna12 4.41905517
#>    3:           1       1         Gna12 2.51999975
#>    4:           3       3         Gna12 2.60980838
#>    5:           6       6         Gna12 4.18561316
#>   ---                                             
#> 4434:           1       1 2900040C04Rik 0.48747773
#> 4435:           3       3 2900040C04Rik 0.21256443
#> 4436:           6       6 2900040C04Rik 0.47596999
#> 4437:           7       7 2900040C04Rik 0.00000000
#> 4438:           4       4 2900040C04Rik 0.04874282
```
