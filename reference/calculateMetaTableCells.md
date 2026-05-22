# calculateMetaTableCells

calculates the average metadata values for one or more (combined)
annotation columns.

## Usage

``` r
calculateMetaTableCells(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  value_cols = NULL,
  metadata_cols = NULL,
  spat_enr_names = NULL
)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit

- feat_type:

  feature type

- value_cols:

  metadata or enrichment value columns to use

- metadata_cols:

  annotation columns found in `pDataDT(gobject)`

- spat_enr_names:

  which spatial enrichment results to include

## Value

data.table with average metadata values per (combined) annotation

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

calculateMetaTableCells(g,
    metadata_cols = "cell_ID",
    value_cols = "leiden_clus"
)
#>                 cell_ID    variable value
#>                  <char>      <fctr> <num>
#>   1: AAAGGGATGTAGCAAG-1 leiden_clus     1
#>   2: AAATGGCATGTCTTGT-1 leiden_clus     2
#>   3: AAATGGTCAATGTGCC-1 leiden_clus     4
#>   4: AAATTAACGGGTAGCT-1 leiden_clus     4
#>   5: AACAACTGGTAGTTGC-1 leiden_clus     6
#>  ---                                     
#> 620: TTGTAATCCGTACTCG-1 leiden_clus     4
#> 621: TTGTATCACACAGAAT-1 leiden_clus     2
#> 622: TTGTCGTTCAGTTACC-1 leiden_clus     1
#> 623: TTGTGGCCCTGACAGT-1 leiden_clus     5
#> 624: TTGTTCAGTGTGCTAC-1 leiden_clus     1
```
