# combineFeatureData

combine feature data information

## Usage

``` r
combineFeatureData(
  gobject,
  feat_type = NULL,
  spat_unit = NULL,
  sel_feats = NULL
)
```

## Arguments

- gobject:

  giotto object

- feat_type:

  feature type

- spat_unit:

  spatial unit

- sel_feats:

  selected features (default: NULL or no selection)

## Value

data.table with combined spatial feature information

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

combineFeatureData(g, spat_unit = "aggregate", feat_type = "rna")
#> $rna
#>        feat_ID nr_cells perc_cells total_expr  mean_expr mean_expr_det hvg_orig
#>         <char>    <int>      <num>      <num>      <num>         <num>   <char>
#>     1:   Abcc9       10   2.164502   71.39463 0.15453382      7.139463       no
#>     2:   Abcc9       10   2.164502   71.39463 0.15453382      7.139463       no
#>     3:   Abcc9       10   2.164502   71.39463 0.15453382      7.139463       no
#>     4:   Abcc9       10   2.164502   71.39463 0.15453382      7.139463       no
#>     5:   Abcc9       10   2.164502   71.39463 0.15453382      7.139463       no
#>    ---                                                                         
#> 79757:  Vmn2r1        7   1.515152   41.22525 0.08923213      5.889321       no
#> 79758:  Vmn2r1        7   1.515152   41.22525 0.08923213      5.889321       no
#> 79759:  Vmn2r1        7   1.515152   41.22525 0.08923213      5.889321       no
#> 79760:  Vmn2r1        7   1.515152   41.22525 0.08923213      5.889321       no
#> 79761:  Vmn2r1        7   1.515152   41.22525 0.08923213      5.889321       no
#>              var    hvf global_z feat_ID_uniq        x         y   feat
#>            <num> <char>    <num>        <num>    <num>     <num> <char>
#>     1: 3.2630074    yes        0           52 6401.822 -5074.172    rna
#>     2: 3.2630074    yes        0         1414 6434.212 -4994.575    rna
#>     3: 3.2630074    yes        0         1534 6435.822 -4993.068    rna
#>     4: 3.2630074    yes        0         1574 6436.190 -4993.599    rna
#>     5: 3.2630074    yes        0         1578 6436.225 -4994.331    rna
#>    ---                                                                 
#> 79757: 0.9765725     no        1        56040 6755.787 -4842.521    rna
#> 79758: 0.9765725     no        1        64767 6788.817 -4766.861    rna
#> 79759: 0.9765725     no        1        64870 6789.242 -4927.838    rna
#> 79760: 0.9765725     no        1        65645 6792.122 -4894.507    rna
#> 79761: 0.9765725     no        1        78156 6876.786 -5032.958    rna
#>        spat_unit
#>           <char>
#>     1: aggregate
#>     2: aggregate
#>     3: aggregate
#>     4: aggregate
#>     5: aggregate
#>    ---          
#> 79757: aggregate
#> 79758: aggregate
#> 79759: aggregate
#> 79760: aggregate
#> 79761: aggregate
#> 
```
