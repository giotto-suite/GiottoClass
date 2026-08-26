# Remove cell annotation

Removes cell annotation from a Giotto object for a specific feature
modality (default = 'rna')

## Usage

``` r
removeCellAnnotation(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  columns = NULL,
  return_gobject = TRUE
)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit

- feat_type:

  feature type

- columns:

  names of columns to remove

- return_gobject:

  boolean: return giotto object (default = TRUE)

## Value

giotto object

## Details

if `return_gobject = FALSE`, it will return the cell metadata

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

annotation <- c(
    "1" = "cell_type_1",
    "2" = "cell_type_2",
    "3" = "cell_type_3",
    "4" = "cell_type_4",
    "5" = "cell_type_5",
    "6" = "cell_type_6",
    "7" = "cell_type_7",
    "8" = "cell_type_8"
)

g <- annotateGiotto(g,
    annotation_vector = annotation,
    cluster_column = "leiden_clus"
)

g <- removeCellAnnotation(g, columns = "cell_types")
```
