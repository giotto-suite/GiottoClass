# combineMetadata

This function combines the cell metadata with spatial locations and
enrichment results from runSpatialEnrich.

## Usage

``` r
combineMetadata(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  spat_loc_name = "raw",
  spat_enr_names = NULL,
  verbose = TRUE
)
```

## Arguments

- gobject:

  Giotto object

- spat_unit:

  spatial unit

- feat_type:

  feature type

- spat_loc_name:

  name of spatial locations to include

- spat_enr_names:

  names of spatial enrichment results to include

- verbose:

  verbosity

## Value

Extended cell metadata in data.table format.

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

combineMetadata(g)
#> Key: <cell_ID>
#>                 cell_ID in_tissue nr_feats perc_feats total_expr leiden_clus
#>                  <char>     <int>    <int>      <num>      <num>       <num>
#>   1: AAAGGGATGTAGCAAG-1         1      227   35.80442   995.1491           1
#>   2: AAATGGCATGTCTTGT-1         1      283   44.63722  1123.8751           2
#>   3: AAATGGTCAATGTGCC-1         1      248   39.11672  1027.6322           4
#>   4: AAATTAACGGGTAGCT-1         1      222   35.01577   974.3766           4
#>   5: AACAACTGGTAGTTGC-1         1      313   49.36909  1174.4881           6
#>  ---                                                                        
#> 620: TTGTAATCCGTACTCG-1         1      315   49.68454  1184.0178           4
#> 621: TTGTATCACACAGAAT-1         1      240   37.85489   999.6419           2
#> 622: TTGTCGTTCAGTTACC-1         1      220   34.70032   994.6648           1
#> 623: TTGTGGCCCTGACAGT-1         1      204   32.17666   930.5489           5
#> 624: TTGTTCAGTGTGCTAC-1         1      237   37.38170  1034.8893           1
#>      custom_leiden sdimx sdimy
#>              <num> <int> <int>
#>   1:             1  5477 -4125
#>   2:             3  5959 -2808
#>   3:             2  4720 -5202
#>   4:             2  5202 -5322
#>   5:             2  4101 -4604
#>  ---                          
#> 620:             2  4996 -5442
#> 621:             3  6303 -2688
#> 622:             1  5202 -3885
#> 623:             1  5340 -3406
#> 624:             1  5615 -4125
```
