# list_feat_metadata

lists the available feature metadata

## Usage

``` r
list_feat_metadata(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
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

- return_uniques:

  return unique nesting names (ignores if final object exists/is correct
  class)

## Value

names and locations of available feature metadata as data.table

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")

list_feat_metadata(g)
```
