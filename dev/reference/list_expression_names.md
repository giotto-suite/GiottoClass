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

list_expression_names(g, spat_unit = "cell", feat_type = "rna")
```
