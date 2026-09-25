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
  [`getSpatialLocations`](https://giotto-suite.github.io/GiottoClass/reference/getSpatialLocations.md)

## Value

Seurat object

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : '/usr/bin/python3'
#>  python version : 3.12
giottoToSeuratV5(g, tech = "Visium")
#> Error: package 'Seurat' is not yet installed
#> 
#>  To install:
#> install.packages(c("Seurat"))
```
