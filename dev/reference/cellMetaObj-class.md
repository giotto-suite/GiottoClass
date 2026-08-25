# S4 cellMetaObj

Framework to store cell metadata

## Value

cellMetaObj

## Slots

- `metaDT`:

  metadata info

- `col_desc`:

  (optional) character vector describing columns of the metadata

- `spat_unit`:

  spatial unit of aggregated expression (e.g. 'cell')

- `feat_type`:

  feature type of aggregated expression (e.g. 'rna', 'protein')

- `provenance`:

  origin data of aggregated expression information (if applicable)

## Examples

``` r
GiottoData::loadSubObjectMini("cellMetaObj")
```
