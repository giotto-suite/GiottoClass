# fDataDT

show feature metadata

## Usage

``` r
fDataDT(gobject, spat_unit = NULL, feat_type = NULL, ...)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit (e.g. "cell")

- feat_type:

  feature type (e.g. "rna", "dna", "protein")

- ...:

  additional params to pass

## Value

data.table with feature metadata

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
#>         hvf
#>      <char>
#>   1:     no
#>   2:     no
#>   3:     no
#>   4:     no
#>   5:     no
#>  ---       
#> 630:     no
#> 631:     no
#> 632:     no
#> 633:     no
#> 634:     no
```
