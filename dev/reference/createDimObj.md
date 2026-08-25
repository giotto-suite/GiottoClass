# Create S4 dimObj

Create an S4 dimObj

## Usage

``` r
createDimObj(
  coordinates,
  name = "test",
  spat_unit = "cell",
  feat_type = "rna",
  method = NULL,
  reduction = "cells",
  provenance = NULL,
  misc = NULL,
  my_rownames = NULL
)
```

## Arguments

- coordinates:

  embedding coordinates

- name:

  name of dimObj

- spat_unit:

  spatial unit of aggregated expression (e.g. 'cell')

- feat_type:

  feature type of aggregated expression (e.g. 'rna', 'protein')

- method:

  method used to generate dimension reduction

- reduction:

  reduction on columns (e.g. cells) or rows (e.g. features)

- provenance:

  origin data of aggregated expression information (if applicable)

- misc:

  misc

- my_rownames:

  (optional) if needed, set coordinates rowname values here

## Value

dimObj

## Examples

``` r
x <- matrix(c(2.81, 5.59, 10.54, 2.25, 5.12, 2.79), nrow = 3)
rownames(x) <- c("cell_1", "cell_2", "cell_3")
colnames(x) <- c("Dim.1", "Dim.2")

createDimObj(coordinates = x, name = "pca", method = "pca")
```
