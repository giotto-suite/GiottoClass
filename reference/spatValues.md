# Giotto object spatial values

Retrieve specific values from the `giotto` object for a specific
`spat_unit` and `feat_type`. Values are returned as a data.table with
the features requested and a `cell_ID` column. This function may be
updated in the future to search in additional sets of information. To
see the currently available slot it checks, see details.

## Usage

``` r
spatValues(
  gobject,
  feats,
  spat_unit = NULL,
  feat_type = NULL,
  expression_values = NULL,
  spat_loc_name = NULL,
  spat_enr_name = NULL,
  poly_info = NULL,
  dim_reduction_to_use = NULL,
  dim_reduction_name = NULL,
  svkey = NULL,
  verbose = NULL,
  debug = FALSE
)

svkey(
  feats,
  spat_unit = NULL,
  feat_type = NULL,
  expression_values = NULL,
  spat_loc_name = NULL,
  poly_info = NULL,
  dim_reduction_to_use = NULL,
  dim_reduction_name = NULL,
  verbose = NULL
)
```

## Arguments

- gobject:

  `giotto` object

- feats:

  character vector. One or more features or values to find within the
  giotto object

- spat_unit:

  character. spatial unit to check

- feat_type:

  character. feature type to check

- expression_values:

  character. (optional) Name of expression information to use

- spat_loc_name:

  character. (optional) Name of spatial locations information to use

- spat_enr_name:

  character. (optional) Name of spatial enrichments to use

- poly_info:

  character. (optional) Name of polygons to use

- dim_reduction_to_use:

  character. (optional) Which type of dimension reduction to use

- dim_reduction_name:

  character. (optional) Name of dimension reduction to use

- svkey:

  use a `svkey`. Other params will be ignored. This is just syntactic
  sugar for `svkey@get(gobject)`

- verbose:

  verbosity

- debug:

  logical. (default = FALSE) See details.

## Value

A data.table with a cell_ID column and whichever feats were requested

## Details

**\[search\]**  
spatValues searches through the set of available information within the
`giotto` object for matches to `feats`. The current search order is

1.  cell expression

2.  cell metadata

3.  spatial locations

4.  spatial enrichment

5.  dimension reduction

6.  polygon info

If a specific name for one of the types of information is provided via a
param such as `expression_values`, `spat_enr_name`, etc, then the search
will only be performed on that type of data.  
  
**\[debug\]**  
This function uses Giotto's accessor functions which can usually throw
errors whenever a specific set of data or the features within that set
do not exist. This function muffles those errors, and only sends an
error that the data was not found when all getters fail. By setting
`debug = TRUE`, you can see the errors returned from each failed getter
printed as messages for easier debugging.

## Functions

- `svkey()`: Create a `svkey` defining a `spatValues()` call for
  deferred use. Handy for contexts where the full `giotto` object is not
  available.

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

# expression
spatValues(g, spat_unit = "aggregate", feats = c("Mlc1", "Gfap"))
#> Getting values from [aggregate][rna][raw] expression
#>                                      cell_ID  Mlc1  Gfap
#>                                       <char> <num> <num>
#>   1: 240649020551054330404932383065726870513     0     2
#>   2: 274176126496863898679934791272921588227     0     0
#>   3: 323754550002953984063006506310071917306     0     2
#>   4:  87260224659312905497866017323180367450     0     1
#>   5:  17817477728742691260808256980746537959     0     0
#>  ---                                                    
#> 458:   6380671372744430258754116433861320161     2     2
#> 459:  75286702783716447443887872812098770697     1     2
#> 460:   9677424102111816817518421117250891895     3     1
#> 461:  17685062374745280598492217386845129350     0     3
#> 462:  32422253415776258079819139802733069941     0     4
spatValues(g,
    spat_unit = "aggregate", feats = c("Mlc1", "Gfap"),
    expression_values = "normalized"
)
#> Getting values from [aggregate][rna][normalized] expression
#>                                      cell_ID     Mlc1      Gfap
#>                                       <char>    <num>     <num>
#>   1: 240649020551054330404932383065726870513 0.000000 10.481367
#>   2: 274176126496863898679934791272921588227 0.000000  0.000000
#>   3: 323754550002953984063006506310071917306 0.000000  7.901442
#>   4:  87260224659312905497866017323180367450 0.000000  5.886051
#>   5:  17817477728742691260808256980746537959 0.000000  0.000000
#>  ---                                                           
#> 458:   6380671372744430258754116433861320161 6.701726  6.701726
#> 459:  75286702783716447443887872812098770697 5.644422  6.629928
#> 460:   9677424102111816817518421117250891895 7.878817  6.306062
#> 461:  17685062374745280598492217386845129350 0.000000 11.065993
#> 462:  32422253415776258079819139802733069941 0.000000 10.041155

# spatial enrichment
spatValues(g, spat_unit = "aggregate", feats = c("1", "3"))
#> Getting values from [aggregate][rna][cluster_metagene] spatial enrichment
#>                                      cell_ID         1         3
#>                                       <char>     <num>     <num>
#>   1: 240649020551054330404932383065726870513 1.0148370 0.3160792
#>   2: 274176126496863898679934791272921588227 3.2074150 0.6728505
#>   3: 323754550002953984063006506310071917306 3.9536614 0.0000000
#>   4:  87260224659312905497866017323180367450 4.0936217 0.1962017
#>   5:  17817477728742691260808256980746537959 0.9789911 1.9563931
#>  ---                                                            
#> 458:   6380671372744430258754116433861320161 2.2465293 0.1905173
#> 459:  75286702783716447443887872812098770697 1.3827322 0.0000000
#> 460:   9677424102111816817518421117250891895 3.6843575 0.0000000
#> 461:  17685062374745280598492217386845129350 1.0010248 0.0000000
#> 462:  32422253415776258079819139802733069941 2.3449006 0.0000000

# polygon info
spatValues(g, spat_unit = "aggregate", feats = c("agg_n", "valid"))
#> Getting values from [aggregate] polygon info
#>                                      cell_ID agg_n valid
#>                                       <char> <num> <int>
#>   1: 100210519278873141813371229408401071444     2     1
#>   2: 101161259912191124732236989250178928032     2     1
#>   3: 101488859781016188084173008420811094152     2     1
#>   4: 101523780333017320796881555775415156847     2     1
#>   5: 102184699197574201819246996094734116255     2     1
#>  ---                                                    
#> 458:   9677424102111816817518421117250891895     2     1
#> 459:  97068595823890802071024838798130036179     2     1
#> 460:  97411078642927912684859796714759494710     2     1
#> 461:   9816437869021910185567097363182418837     2     1
#> 462:  98561957902191275233320065611022298397     2     1

# cell meta
spatValues(g, spat_unit = "aggregate", feats = c("nr_feats"))
#> Getting values from [aggregate][rna] cell metadata
#>                                      cell_ID nr_feats
#>                                       <char>    <int>
#>   1: 240649020551054330404932383065726870513        5
#>   2: 274176126496863898679934791272921588227       27
#>   3: 323754550002953984063006506310071917306       23
#>   4:  87260224659312905497866017323180367450       37
#>   5:  17817477728742691260808256980746537959       18
#>  ---                                                 
#> 458:   6380671372744430258754116433861320161       54
#> 459:  75286702783716447443887872812098770697       45
#> 460:   9677424102111816817518421117250891895       30
#> 461:  17685062374745280598492217386845129350        5
#> 462:  32422253415776258079819139802733069941       12
```
