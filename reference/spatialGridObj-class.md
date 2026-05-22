# S4 spatialGridObj Class

Framework to store spatial grid

## Value

spatialGridObj

## Details

This is an S4 object that defines a spatial grid. The structure of the
grid is stored as a `data.table` within the `gridDT` slot and is defined
by start and stop spatial locations along the spatial axes. The
`data.table` also includes names for each cell of the grid and names for
each of the spatial axis locations that make up the cell. Grids can be
annotated with both spatial and feature information

## Slots

- `name`:

  name of spatialGridObj

- `method`:

  method used to generate spatial grid

- `parameters`:

  additional method-specific parameters used during spatial grid
  generation

- `gridDT`:

  data.table holding the spatial grid information

- `spat_unit`:

  spatial unit

- `feat_type`:

  feature type

- `provenance`:

  origin of aggregated information (if applicable)

- `misc`:

  misc

## Examples

``` r
g <- GiottoData::loadSubObjectMini("spatialGridObj")
```
