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

list_nearest_networks_names(g,
    spat_unit = "cell", feat_type = "rna",
    nn_type = "sNN"
)
```
