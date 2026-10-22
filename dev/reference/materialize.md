# materialize a giottoView into a new gobject

Deprecated in 0.7.3. Superseded by
[`resolveRecipe()`](https://giotto-suite.github.io/GiottoClass/dev/reference/resolveRecipe.md).

## Usage

``` r
materialize(gobject, view = NULL, ...)

# S4 method for class 'gAny'
materialize(gobject, view, space = NULL, coordinator = NULL, slots = NULL, ...)
```

## Arguments

- gobject:

  a `giotto` object

- view:

  either `NULL` or a `character(1)` slot key

- ...:

  reserved

- space:

  `character(1)` optional — name of a slotted `giottoSpace`

- coordinator:

  a
  [viewCoordinator](https://giotto-suite.github.io/GiottoClass/dev/reference/viewCoordinator-class.md)-inheriting
  object

- slots:

  optional `character` vector of slot names to narrow

## Value

a new `giotto` object reflecting the resolved view
