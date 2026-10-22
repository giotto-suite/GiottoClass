# resolveKeep

Evaluate a view into the surviving cell set of one
[`resolveRecipe()`](https://giotto-suite.github.io/GiottoClass/dev/reference/resolveRecipe.md)
op: the cells of `spat_unit` that pass every filter, crop and sample
step. Dispatches on the coordinator, which decides how the set is
computed and which forms it carries.

The set is returned as a `viewKeep`: an environment holding one form of
the set per coordinator that reads it, built with
`structure(list2env(list(vector = ids), parent = emptyenv()), class = "viewKeep")`.
`dataTableCoordinator` reads `vector`, a character vector of cell_IDs. A
coordinator that extends another adds its own form alongside, under its
own name. It is an environment so that a form can be installed with
[`delayedAssign()`](https://rdrr.io/r/base/delayedAssign.html) and
computed only if a leaf reads it — a backed coordinator whose own form
is a lazy query collects `vector` only when an in-memory subobject asks
for it. A view that narrows nothing returns `NULL` rather than an empty
`viewKeep`: `NULL` means "every cell survives", while a `viewKeep`
holding zero IDs means none do.

A method must return every form that the coordinators its class extends
read, as well as its own. A coordinator that computes its own form can
derive the inherited ones from it; one that has no faster path can call
the inherited method with `callNextMethod()` and add its form to the
result.

## Usage

``` r
# S3 method for class 'viewKeep'
print(x, ...)

resolveKeep(coordinator, gobject, view, ...)

# S4 method for class 'dataTableCoordinator'
resolveKeep(
  coordinator,
  gobject,
  view,
  spat_unit = NULL,
  feat_type = NULL,
  ...
)
```

## Arguments

- x:

  a `viewKeep`

- ...:

  reserved for backend-specific args

- coordinator:

  a
  [viewCoordinator](https://giotto-suite.github.io/GiottoClass/dev/reference/viewCoordinator-class.md)-inheriting
  object

- gobject:

  the `giotto` / `giottoMulti` the view is evaluated against

- view:

  a
  [giottoView](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoView.md),
  or `NULL`

- spat_unit:

  the spat_unit whose ID vocabulary the set is in. `NULL` uses the
  active one.

- feat_type:

  the feat_type whose metadata a filter's columns are read from. `NULL`
  uses the active one.

## Value

a `viewKeep`, or `NULL` when the view narrows nothing
