# list_spatial_enrichments

return the available spatial enrichment results

## Usage

``` r
list_spatial_enrichments(gobject, spat_unit = NULL, feat_type = NULL)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit (e.g. "cell")

- feat_type:

  feature type (e.g. "rna", "dna", "protein")

## Value

names and locations of available data as data.table

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

list_spatial_enrichments(g)
#>    spat_unit feat_type             name
#>       <char>    <char>           <char>
#> 1: aggregate       rna cluster_metagene
```
