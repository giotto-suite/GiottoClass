# fDataDT

show feature metadata

## Usage

``` r
fDataDT(gobject, spat_unit = NULL, feat_type = NULL, ...)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit (e.g. "cell")

- feat_type:

  feature type (e.g. "rna", "dna", "protein")

- ...:

  additional params to pass

## Value

data.table with feature metadata

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")

fDataDT(g)
```
