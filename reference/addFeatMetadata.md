# Add feature metadata

Adds feature metadata to the giotto object

## Usage

``` r
addFeatMetadata(
  gobject,
  feat_type = NULL,
  spat_unit = NULL,
  new_metadata,
  vector_name = NULL,
  by_column = FALSE,
  column_feat_ID = NULL
)
```

## Arguments

- gobject:

  giotto object

- feat_type:

  feature type

- spat_unit:

  spatial unit

- new_metadata:

  new metadata to use)

- vector_name:

  (optional) custom name if you provide a single vector

- by_column:

  merge metadata based on *feat_ID* column in
  [`fDataDT`](https://giotto-suite.github.io/GiottoClass/reference/fDataDT.md)

- column_feat_ID:

  column name of new metadata to use if by_column = TRUE

## Value

giotto object

## Details

You can add additional feature metadata in several manners:

- 1\. Provide a data.table or data.frame with feature annotations in the
  same order as the *feat_ID* column in fDataDT(gobject) This is a bit
  risky and not the most recommended.

- 2\. Provide a data.table or data.frame with feature annotations and
  specify which column contains the feature IDs, these feature IDs need
  to match with the *feat_ID* column in fDataDT(gobject)

- 3\. Provide a vector or factor that is named with the feature IDs they
  correspond to. These names will be matched against the *feat_ID*
  column in fDataDT(gobject).

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

m <- fDataDT(g)
m <- m[, "feat_ID"]
m$new_feat_ID <- paste0("gene_", m$feat_ID)

g <- addFeatMetadata(
    g,
    new_metadata = m,
    by_column = TRUE,
    column_feat_ID = "feat_ID"
)

fDataDT(g)
#>            feat_ID nr_cells perc_cells total_expr mean_expr mean_expr_det
#>             <char>    <int>      <num>      <num>     <num>         <num>
#>   1:         Gna12      502  80.448718  1987.1333 3.1845085      3.958433
#>   2:         Ccnd2      357  57.211538  1254.4730 2.0103734      3.513930
#>   3:        Btbd17      252  40.384615   781.9747 1.2531645      3.103074
#>   4:          Sox9      309  49.519231  1019.7255 1.6341755      3.300083
#>   5:          Sez6      504  80.769231  2177.4740 3.4895416      4.320385
#>  ---                                                                     
#> 630:        Lhfpl3      235  37.660256   732.3595 1.1736530      3.116423
#> 631: C030029H02Rik       72  11.538462   203.0811 0.3254506      2.820572
#> 632:       Gm19935       57   9.134615   172.1079 0.2758139      3.019436
#> 633: 9630013A20Rik       52   8.333333   157.2602 0.2520196      3.024235
#> 634: 2900040C04Rik       61   9.775641   179.7015 0.2879831      2.945926
#>         hvf        new_feat_ID
#>      <char>             <char>
#>   1:     no         gene_Gna12
#>   2:     no         gene_Ccnd2
#>   3:     no        gene_Btbd17
#>   4:     no          gene_Sox9
#>   5:     no          gene_Sez6
#>  ---                          
#> 630:     no        gene_Lhfpl3
#> 631:     no gene_C030029H02Rik
#> 632:     no       gene_Gm19935
#> 633:     no gene_9630013A20Rik
#> 634:     no gene_2900040C04Rik
```
