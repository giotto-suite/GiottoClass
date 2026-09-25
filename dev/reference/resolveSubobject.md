# resolveSubobject

Apply a
[giottoView](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoView.md)
(and optional
[giottoSpace](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoSpace.md))
recipe to a single subobject, returning the projected subobject.
Dispatch is on `(subobj, coordinator)` so different storage-bridging
coordinators register different methods.

## Usage

``` r
resolveSubobject(subobj, gobject, view, space, coordinator, ...)

# S4 method for class 'cellMetaObj,ANY,ANY,ANY,dataTableCoordinator'
resolveSubobject(subobj, gobject, view, space, coordinator, ...)

# S4 method for class 'exprObj,ANY,ANY,ANY,dataTableCoordinator'
resolveSubobject(subobj, gobject, view, space, coordinator, ...)

# S4 method for class 'dimObj,ANY,ANY,ANY,dataTableCoordinator'
resolveSubobject(subobj, gobject, view, space, coordinator, ...)

# S4 method for class 'spatEnrObj,ANY,ANY,ANY,dataTableCoordinator'
resolveSubobject(subobj, gobject, view, space, coordinator, ...)

# S4 method for class 'featMetaObj,ANY,ANY,ANY,dataTableCoordinator'
resolveSubobject(subobj, gobject, view, space, coordinator, ...)

# S4 method for class 'spatLocsObj,ANY,ANY,ANY,dataTableCoordinator'
resolveSubobject(subobj, gobject, view, space, coordinator, ...)

# S4 method for class 'giottoPolygon,ANY,ANY,ANY,dataTableCoordinator'
resolveSubobject(subobj, gobject, view, space, coordinator, ...)

# S4 method for class 'giottoPoints,ANY,ANY,ANY,dataTableCoordinator'
resolveSubobject(subobj, gobject, view, space, coordinator, ...)

# S4 method for class 'giottoLargeImage,ANY,ANY,ANY,dataTableCoordinator'
resolveSubobject(subobj, gobject, view, space, coordinator, ...)

# S4 method for class 'giottoAffineImage,ANY,ANY,ANY,dataTableCoordinator'
resolveSubobject(subobj, gobject, view, space, coordinator, ...)

# S4 method for class 'giottoImage,ANY,ANY,ANY,dataTableCoordinator'
resolveSubobject(subobj, gobject, view, space, coordinator, ...)
```

## Arguments

- subobj:

  a giotto subobject (e.g. `cellMetaObj`, `spatLocsObj`, ...)

- gobject:

  the parent `giotto` (needed for cross-slot lookups via
  [`spatValues()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatValues.md))

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
  object brokering IDs and joins between storage backings

- ...:

  reserved for backend-specific args

## Value

the projected subobject (same class as `subobj`)
