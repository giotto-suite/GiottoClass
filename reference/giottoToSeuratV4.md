# Convert Giotto to Seurat V4

Converts Giotto object into a Seurat object. This functions extracts
specific sets of data belonging to specified spatial unit. The default
values are 'cell' and 'rna' respectively.

## Usage

``` r
giottoToSeuratV4(gobject, spat_unit = NULL, ...)
```

## Arguments

- gobject:

  Giotto object

- spat_unit:

  spatial unit (e.g. 'cell')

- ...:

  additional params to pass to
  [`getSpatialLocations`](https://giotto-suite.github.io/GiottoClass/reference/getSpatialLocations.md)

## Value

Seurat object
