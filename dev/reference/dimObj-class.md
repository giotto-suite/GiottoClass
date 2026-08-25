# S4 dimObj Class

Framework to store dimension reduction information

## Value

dimObj

## Slots

- `name`:

  name of dimObject

- `feat_type`:

  feature type of data

- `spat_unit`:

  spatial unit of data

- `provenance`:

  origin of aggregated information (if applicable)

- `reduction`:

  whether reduction was performed on 'feats' or 'cells'

- `reduction_method`:

  method used to generate dimension reduction

- `coordinates`:

  embedding coordinates

- `misc`:

  method-specific additional outputs

## Examples

``` r
GiottoData::loadSubObjectMini("dimObj")
```
