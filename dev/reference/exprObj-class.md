# S4 exprObj

Framework to store aggregated expression information

## Value

exprObj

## Slots

- `name`:

  name of exprObj

- `exprMat`:

  matrix of expression information

- `spat_unit`:

  spatial unit of expression (e.g. 'cell')

- `feat_type`:

  feature type of expression (e.g. 'rna', 'protein')

- `provenance`:

  origin data of expression information (if applicable)

- `misc`:

  misc

## Examples

``` r
GiottoData::loadSubObjectMini("exprObj")
```
