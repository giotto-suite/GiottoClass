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

list_dim_reductions_names(g,
    spat_unit = "cell", feat_type = "rna",
    dim_type = "pca"
)
```
