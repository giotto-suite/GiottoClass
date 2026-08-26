# combineFeatureOverlapData

combine feature data information

## Usage

``` r
combineFeatureOverlapData(
  gobject,
  feat_type = "rna",
  sel_feats = NULL,
  poly_info = "cell"
)
```

## Arguments

- gobject:

  giotto object

- feat_type:

  feature type

- sel_feats:

  selected features (default: NULL or no selection)

- poly_info:

  polygon information name

## Value

data.table with combined spatial polygon information

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : '/usr/bin/python3'
#>  python version : 3.12

combineFeatureOverlapData(g, poly_info = "aggregate")
#> $rna
#> Key: <feat_ID>
#>        feat_ID nr_cells perc_cells total_expr  mean_expr mean_expr_det hvg_orig
#>         <char>    <int>      <num>      <num>      <num>         <num>   <char>
#>     1:   Abcc9       10   2.164502   71.39463 0.15453382      7.139463       no
#>     2:   Abcc9       10   2.164502   71.39463 0.15453382      7.139463       no
#>     3:   Abcc9       10   2.164502   71.39463 0.15453382      7.139463       no
#>     4:   Abcc9       10   2.164502   71.39463 0.15453382      7.139463       no
#>     5:   Abcc9       10   2.164502   71.39463 0.15453382      7.139463       no
#>    ---                                                                         
#> 32067:  Vmn2r1        7   1.515152   41.22525 0.08923213      5.889321       no
#> 32068:  Vmn2r1        7   1.515152   41.22525 0.08923213      5.889321       no
#> 32069:  Vmn2r1        7   1.515152   41.22525 0.08923213      5.889321       no
#> 32070:  Vmn2r1        7   1.515152   41.22525 0.08923213      5.889321       no
#> 32071:  Vmn2r1        7   1.515152   41.22525 0.08923213      5.889321       no
#>              var    hvf  geom  part        x         y  hole global_z
#>            <num> <char> <int> <num>    <num>     <num> <num>    <num>
#>     1: 3.2630074    yes   327     1 6434.212 -4994.575     0        0
#>     2: 3.2630074    yes   372     1 6435.822 -4993.068     0        0
#>     3: 3.2630074    yes   387     1 6436.190 -4993.599     0        0
#>     4: 3.2630074    yes   389     1 6436.225 -4994.331     0        0
#>     5: 3.2630074    yes  3476     1 6583.026 -5109.118     0        0
#>    ---                                                               
#> 32067: 0.9765725     no 12671     1 6835.198 -4821.361     0        0
#> 32068: 0.9765725     no 13128     1 6850.250 -4791.958     0        0
#> 32069: 0.9765725     no 15407     1 6432.064 -5016.469     0        1
#> 32070: 0.9765725     no 21939     1 6742.455 -4972.980     0        1
#> 32071: 0.9765725     no 31289     1 6876.786 -5032.958     0        1
#>        feat_ID_uniq poly_info   feat
#>               <num>    <char> <char>
#>     1:         1414 aggregate    rna
#>     2:         1534 aggregate    rna
#>     3:         1574 aggregate    rna
#>     4:         1578 aggregate    rna
#>     5:         8860 aggregate    rna
#>    ---                              
#> 32067:        30405 aggregate    rna
#> 32068:        31685 aggregate    rna
#> 32069:        37793 aggregate    rna
#> 32070:        53817 aggregate    rna
#> 32071:        78156 aggregate    rna
#> 
```
