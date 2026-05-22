# list_expression_names

lists the available matrices names for a given spatial unit and feature
type

## Usage

``` r
list_expression_names(gobject, spat_unit = NULL, feat_type = NULL)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit (e.g. "cell")

- feat_type:

  feature type (e.g. "rna", "dna", "protein")

## Value

vector with names of available matrices

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

list_expression_names(g, spat_unit = "cell", feat_type = "rna")
#> [1] "raw"        "normalized" "scaled"    
```
