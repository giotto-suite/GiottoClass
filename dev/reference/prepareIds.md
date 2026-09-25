# prepareIds

Coordinator-side protocol method: promote an R-memory cell_ID character
vector into the form the coordinator's preferred backend uses for
filtering. For
[dataTableCoordinator](https://giotto-suite.github.io/GiottoClass/dev/reference/dataTableCoordinator-class.md)
this is the identity transform; downstream coordinators (e.g. duckDB /
sedona from GiottoDisk) register methods that perform ephemeral table
registration or similar.

## Usage

``` r
prepareIds(coordinator, ids, ...)

# S4 method for class 'dataTableCoordinator'
prepareIds(coordinator, ids, ...)
```

## Arguments

- coordinator:

  a
  [viewCoordinator](https://giotto-suite.github.io/GiottoClass/dev/reference/viewCoordinator-class.md)-inheriting
  object

- ids:

  character vector of cell_IDs

- ...:

  reserved

## Value

the prepared IDs in the coordinator's preferred form
