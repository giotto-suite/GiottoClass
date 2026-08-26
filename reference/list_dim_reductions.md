# list_dim_reductions

return the available dimension reductions

## Usage

``` r
list_dim_reductions(
  gobject,
  data_type = NULL,
  spat_unit = NULL,
  feat_type = NULL,
  dim_type = NULL
)
```

## Arguments

- gobject:

  giotto object

- data_type:

  "cells" or "feats" data used in dim reduction

- spat_unit:

  spatial unit (e.g. "cell")

- feat_type:

  feature type (e.g. "rna", "dna", "protein")

- dim_type:

  dimensional reduction method (e.g. "pca", "umap")

## Value

names and locations of dimension reduction as a data.table

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

list_dim_reductions(g)
#>    data_type spat_unit feat_type dim_type        name
#>       <char>    <char>    <char>   <char>      <char>
#> 1:     cells      cell       rna      pca         pca
#> 2:     cells      cell       rna      pca  custom_pca
#> 3:     cells      cell       rna     umap        umap
#> 4:     cells      cell       rna     umap custom_umap
#> 5:     cells      cell       rna     tsne        tsne
```
