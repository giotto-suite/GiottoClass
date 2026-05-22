# pDataDT

show cell metadata

## Usage

``` r
pDataDT(gobject, spat_unit = NULL, feat_type = NULL, ...)
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

data.table with cell metadata

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : 'giotto_env'
#>  python version : 3.10

pDataDT(g)
#>                 cell_ID in_tissue nr_feats perc_feats total_expr leiden_clus
#>                  <char>     <int>    <int>      <num>      <num>       <num>
#>   1: AACTCGATGGCGCAGT-1         1      265   41.79811  1057.9308           2
#>   2: GGCTGGCTAGCTTAAA-1         1      279   44.00631  1064.7493           5
#>   3: GACGCCTGTTGCAGGG-1         1      219   34.54259   964.9294           2
#>   4: GAGGGCATCGCGTATC-1         1      294   46.37224  1142.7664           2
#>   5: TCAACACATTGGGTAA-1         1      261   41.16719  1063.3517           2
#>  ---                                                                        
#> 620: GGTAGTGCTCGCACCA-1         1      179   28.23344   768.4749           5
#> 621: AAGCTCGTGCCAAGTC-1         1      195   30.75710   756.0675           5
#> 622: TATTCAATTCTAATCC-1         1      247   38.95899   921.8264           5
#> 623: TTCAAAGTCTCTAGCC-1         1      384   60.56782   916.5929           6
#> 624: TTGAATATGGACTTTC-1         1      380   59.93691   912.3051           6
#>      custom_leiden
#>              <num>
#>   1:             4
#>   2:             3
#>   3:             3
#>   4:             3
#>   5:             3
#>  ---              
#> 620:             4
#> 621:             4
#> 622:             4
#> 623:             7
#> 624:             4
```
