# Convert Giotto to Seurat V5

Converts Giotto object into a Seurat object. This functions extracts
specific sets of data belonging to specified spatial unit. The default
values are 'cell' and 'rna' respectively.

## Usage

``` r
giottoToSeuratV5(
  gobject,
  spat_unit = NULL,
  tech = c("Visium", "Xenium", "Slide-seq"),
  res_type = c("hires", "lowres", "fullres"),
  ...
)
```

## Arguments

- gobject:

  Giotto object

- spat_unit:

  spatial unit (e.g. 'cell')

- tech:

  technology the dataset is using (e.g. "Visium","Xenium", "Slide-seq")

- res_type:

  type of 10x image output resolution

- ...:

  additional params to pass to
  [`getSpatialLocations`](https://giotto-suite.github.io/GiottoClass/dev/reference/getSpatialLocations.md)

## Value

Seurat object

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")
giottoToSeuratV5(g, tech = "Visium")
```
