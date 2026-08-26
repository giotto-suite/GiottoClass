# list_nearest_networks_names

return the available nearest neighbor network object names

## Usage

``` r
list_nearest_networks_names(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  nn_type = NULL
)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit (e.g. "cell")

- feat_type:

  feature type (e.g. "rna", "dna", "protein")

- nn_type:

  nearest neighbor method (e.g. "sNN", "kNN")

## Value

names of nearest neighbor network object

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

list_nearest_networks_names(g,
    spat_unit = "cell", feat_type = "rna",
    nn_type = "sNN"
)
#> [1] "sNN.pca"   "custom_NN"
```
