# Deprecated

Deprecated. Please use either
[giottoToSeuratV4](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoToSeuratV4.md)
or
[giottoToSeuratV5](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoToSeuratV5.md)

## Usage

``` r
giottoToSeurat(gobject, spat_unit = NULL, obj_use = NULL, ...)
```

## Arguments

- gobject:

  Giotto object

- spat_unit:

  spatial unit (e.g. 'cell')

- obj_use:

  Giotto object (deprecated, use gobject)

- ...:

  additional params to pass to
  [`getSpatialLocations`](https://giotto-suite.github.io/GiottoClass/dev/reference/getSpatialLocations.md)

## Value

Seurat object
