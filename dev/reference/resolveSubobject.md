# resolveSubobject

Deprecated in 0.7.3. Superseded by
[`resolveRecipe()`](https://giotto-suite.github.io/GiottoClass/dev/reference/resolveRecipe.md),
which is one generic for both the container and the leaf, and which
hands a leaf its evaluated context instead of the parent gobject.

## Usage

``` r
resolveSubobject(subobj, gobject, view, space, coordinator, ...)
```

## Arguments

- subobj:

  a giotto subobject (e.g. `cellMetaObj`, `spatLocsObj`, ...)

- gobject:

  the parent `giotto`

- view:

  a
  [giottoView](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoView.md)
  or `NULL`

- space:

  a
  [giottoSpace](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoSpace.md)
  or `NULL`

- coordinator:

  a
  [viewCoordinator](https://giotto-suite.github.io/GiottoClass/dev/reference/viewCoordinator-class.md)-inheriting
  object

- ...:

  reserved for backend-specific args

## Value

the projected subobject (same class as `subobj`)
