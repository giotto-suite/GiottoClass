# Remove feature annotation

Removes feature annotation from a Giotto object for a specific feature
modality

## Usage

``` r
removeFeatAnnotation(
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

if `return_gobject = FALSE`, it will return the gene metadata

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")

g <- removeFeatAnnotation(g, columns = "hvf")
```
