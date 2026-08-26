# List feat ID names

lists the available feat id names In effect, these names are the
feat_types and feature info in the gobject

## Usage

``` r
list_feat_id_names(gobject)
```

## Arguments

- gobject:

  giotto object

## Value

vector with names of available sets of feat_IDs

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

list_feat_id_names(g)
#> [1] "rna"
```
