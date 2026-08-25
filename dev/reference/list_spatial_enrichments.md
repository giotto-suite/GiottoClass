# list_spatial_enrichments

return the available spatial enrichment results

## Usage

``` r
list_spatial_enrichments(gobject, spat_unit = NULL, feat_type = NULL)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit (e.g. "cell")

- feat_type:

  feature type (e.g. "rna", "dna", "protein")

## Value

names and locations of available data as data.table

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")

list_spatial_enrichments(g)
```
