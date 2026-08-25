# list_nearest_networks

return the available nearest neighbor network information

## Usage

``` r
list_nearest_networks(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  nn_type = NULL,
  return_uniques = FALSE
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

- return_uniques:

  return unique nesting names (ignores if final object exists/is correct
  class)

## Value

names and locations of nearest neighbor networks as a data.table

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")

list_nearest_networks(g)
```
