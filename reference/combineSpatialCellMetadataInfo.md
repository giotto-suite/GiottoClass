# combineSpatialCellMetadataInfo

Combine cell metadata with spatial cell information (e.g. polygon)

## Usage

``` r
combineSpatialCellMetadataInfo(gobject, spat_unit = NULL, feat_type = NULL)
```

## Arguments

- gobject:

  Giotto object

- spat_unit:

  spatial unit

- feat_type:

  feature type(s)

## Value

list of data.table(s)

## Details

The returned data.table has the following columns:  

- sdimx: spatial feature location on the x-axis

- sdimy: spatial feature location on the y-axis

- cell_ID: unique cell ID

- feat: selected feature(s)

- other columns that are part of the cell metadata

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

combineSpatialCellMetadataInfo(g, spat_unit = "aggregate", feat_type = "rna")
#> $rna
#> Key: <cell_ID>
#>                                      cell_ID  stack agg_n valid nr_feats
#>                                       <char> <char> <num> <int>    <int>
#>   1: 100210519278873141813371229408401071444   <NA>     2     1       22
#>   2: 101161259912191124732236989250178928032   <NA>     2     1       36
#>   3: 101488859781016188084173008420811094152   <NA>     2     1       35
#>   4: 101523780333017320796881555775415156847   <NA>     2     1       39
#>   5: 102184699197574201819246996094734116255   <NA>     2     1       44
#>  ---                                                                    
#> 458:   9677424102111816817518421117250891895   <NA>     2     1       30
#> 459:  97068595823890802071024838798130036179   <NA>     2     1       46
#> 460:  97411078642927912684859796714759494710   <NA>     2     1       33
#> 461:   9816437869021910185567097363182418837   <NA>     2     1       31
#> 462:  98561957902191275233320065611022298397   <NA>     2     1       53
#>      perc_feats total_expr leiden_clus louvain_clus   feat
#>           <num>      <num>       <num>        <num> <char>
#>   1:   6.528190   163.6923           4           15    rna
#>   2:  10.682493   228.7269           2           10    rna
#>   3:  10.385757   242.0458           1           22    rna
#>   4:  11.572700   262.3842           1           24    rna
#>   5:  13.056380   291.5877           2            0    rna
#>  ---                                                      
#> 458:   8.902077   211.7179           2            3    rna
#> 459:  13.649852   279.5605           2            5    rna
#> 460:   9.792285   226.7092           4           15    rna
#> 461:   9.198813   220.4129           3           13    rna
#> 462:  15.727003   326.0539           1           26    rna
#> 
```
