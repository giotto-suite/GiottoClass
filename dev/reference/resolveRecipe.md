# Resolve a view / space recipe

Apply a
[giottoView](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoView.md)
(and optional
[giottoSpace](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoSpace.md))
recipe and return the projected object.

At a **container** (`giotto`, `giottoMulti`) this evaluates the recipe
for one `spat_unit` / `feat_type` scope and returns a gobject whose
in-scope subobjects have been projected through it. The input gobject is
not mutated.

This is plumbing for code that consumes one scope of data, such as a
plot function, not a way to derive a new object to work on. Only the
requested scope is guaranteed: subobjects outside it are left untouched,
so they need not agree with the narrowed ones. The returned gobject is a
carrier for the narrowed data — read what you asked for from it and
discard it. To narrow a whole object, use
[`subsetGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/subsetGiotto.md)
or [`subset()`](https://rdrr.io/r/base/subset.html).

At a **leaf** (one subobject) this applies an already-evaluated context:
the surviving cell set, the output frame, the recipe itself.

## Usage

``` r
resolveRecipe(x, coordinator = NULL, ...)

# S4 method for class 'cellMetaObj,dataTableCoordinator'
resolveRecipe(x, coordinator, keep = NULL, space = NULL, view = NULL, ...)

# S4 method for class 'exprObj,dataTableCoordinator'
resolveRecipe(x, coordinator, keep = NULL, space = NULL, view = NULL, ...)

# S4 method for class 'dimObj,dataTableCoordinator'
resolveRecipe(x, coordinator, keep = NULL, space = NULL, view = NULL, ...)

# S4 method for class 'spatEnrObj,dataTableCoordinator'
resolveRecipe(x, coordinator, keep = NULL, space = NULL, view = NULL, ...)

# S4 method for class 'featMetaObj,dataTableCoordinator'
resolveRecipe(x, coordinator, keep = NULL, space = NULL, view = NULL, ...)

# S4 method for class 'spatLocsObj,dataTableCoordinator'
resolveRecipe(x, coordinator, keep = NULL, space = NULL, view = NULL, ...)

# S4 method for class 'giottoPolygon,dataTableCoordinator'
resolveRecipe(x, coordinator, keep = NULL, space = NULL, view = NULL, ...)

# S4 method for class 'giottoPoints,dataTableCoordinator'
resolveRecipe(
  x,
  coordinator,
  keep = NULL,
  space = NULL,
  view = NULL,
  spaces = NULL,
  ...
)

# S4 method for class 'giottoBinPoints,dataTableCoordinator'
resolveRecipe(
  x,
  coordinator,
  keep = NULL,
  space = NULL,
  view = NULL,
  spaces = NULL,
  ...
)

# S4 method for class 'giottoLargeImage,dataTableCoordinator'
resolveRecipe(
  x,
  coordinator,
  keep = NULL,
  space = NULL,
  view = NULL,
  spaces = NULL,
  ...
)

# S4 method for class 'giottoAffineImage,dataTableCoordinator'
resolveRecipe(
  x,
  coordinator,
  keep = NULL,
  space = NULL,
  view = NULL,
  spaces = NULL,
  ...
)

# S4 method for class 'giottoImage,dataTableCoordinator'
resolveRecipe(
  x,
  coordinator,
  keep = NULL,
  space = NULL,
  view = NULL,
  spaces = NULL,
  ...
)

# S4 method for class 'giotto,ANY'
resolveRecipe(
  x,
  coordinator = NULL,
  view = NULL,
  space = NULL,
  slots = NULL,
  spat_unit = NULL,
  feat_type = NULL,
  ...
)

# S4 method for class 'giottoMulti,ANY'
resolveRecipe(
  x,
  coordinator = NULL,
  view = NULL,
  space = NULL,
  slots = NULL,
  spat_unit = NULL,
  feat_type = NULL,
  ...
)
```

## Arguments

- x:

  a `giotto` / `giottoMulti` container, or a giotto subobject leaf

- coordinator:

  a
  [viewCoordinator](https://giotto-suite.github.io/GiottoClass/dev/reference/viewCoordinator-class.md)-inheriting
  object brokering IDs and joins between storage backings. At a
  container, `NULL` (the default) selects one from `x@source` —
  in-memory for non-disk gobjects.

- ...:

  reserved for backend-specific args

- keep:

  leaf only — the op's surviving cell set as a `viewKeep` (see
  [`resolveKeep()`](https://giotto-suite.github.io/GiottoClass/dev/reference/resolveKeep.md)),
  or `NULL` for "no narrowing".

- space:

  at a container, a `character(1)` naming a slotted
  [giottoSpace](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoSpace.md);
  at a leaf, the resolved object. `NULL` means the native frame. This is
  the OUTPUT frame, independent of the frame a crop step names, which
  says only which frame that step's region was drawn in.

- view:

  at a container, a `character(1)` naming a slotted view, or `NULL`. At
  a leaf, the already-looked-up
  [giottoView](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoView.md)
  or `NULL` — a recipe is inert data, so a leaf needs no gobject to read
  one.

- spaces:

  leaf only — the object's `@spaces` list, where a crop step's recorded
  frame is looked up by name. Handed over as data so that a leaf
  clipping geometry needs no gobject.

- slots:

  container only — optional `character` vector of slot names to narrow.
  When `NULL` (default), all slot lists are walked (`cell_metadata`,
  `expression`, `dimension_reduction`, `spatial_enrichment`,
  `feat_metadata`, `spatial_locs`, `spatial_info`, `feat_info`,
  `images`). When supplied, only the listed slots are walked — the rest
  are left untouched on the returned object. Useful for internal helpers
  that consume a subset of slots and want to share one resolver pass
  without paying for irrelevant ones.

- spat_unit:

  container only — the spat_unit to resolve. The surviving cell set is
  computed in this unit's cell_ID vocabulary, and only subobjects of
  this unit are narrowed. `NULL` uses the active one.

- feat_type:

  container only — the feat_type to resolve. Subobjects of other
  feat_types are left untouched, and filter columns are read from this
  feat_type's metadata. `NULL` uses the active one.

## Value

the projected object, same class as `x`

## Details

Dispatch is on `(x, coordinator)`; everything past `x` is passed by
name. A container does not dispatch on the coordinator — it *picks* one
from `x@source` when none is given — so container methods register
against `ANY` and consume `coordinator` as an ordinary argument.

`keep` reaches a leaf as a promise, so a leaf that does not narrow by
cell (points, images) never forces the ID computation.

`coordinator` defaults to `NULL` on the *generic*, not just on the
container methods. S4 propagates the generic's formals symbolically into
the method's `.local` call, so a default written only on the method is
never reached when the argument is omitted.
