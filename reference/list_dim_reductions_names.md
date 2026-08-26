# list_dim_reductions_names

return the available dimension reductions object names

## Usage

``` r
list_dim_reductions_names(
  gobject,
  data_type = "cells",
  spat_unit = NULL,
  feat_type = NULL,
  dim_type = NULL
)
```

## Arguments

- gobject:

  giotto object

- data_type:

  cells or feats dim reduction

- spat_unit:

  spatial unit (e.g. "cell")

- feat_type:

  feature type (e.g. "rna", "dna", "protein")

- dim_type:

  dimensional reduction type (method)

## Value

names of dimension reduction object

## Details

function that can be used to find which names have been used

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

list_dim_reductions_names(g,
    spat_unit = "cell", feat_type = "rna",
    dim_type = "pca"
)
#> [1] "pca"        "custom_pca"
```
