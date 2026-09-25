# materialize a giottoView into a new gobject

Resolve a
[giottoView](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoView.md)
(optionally with a slotted
[giottoSpace](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoSpace.md)
frame) against a gobject and return a new gobject containing the
projected subobjects. Use this when downstream work needs to produce
structured outputs (spatial networks, dim reductions) on top of the
projected data — those outputs live in the materialised gobject, never
in the parent.

Read-only contract: the input gobject is not mutated.

## Usage

``` r
materialize(gobject, view, ...)

# S4 method for class 'giotto,character'
materialize(gobject, view, space = NULL, coordinator = NULL, slots = NULL, ...)

# S4 method for class 'giottoMulti,character'
materialize(gobject, view, space = NULL, coordinator = NULL, slots = NULL, ...)

# S4 method for class 'giotto,NULL'
materialize(gobject, view, space = NULL, coordinator = NULL, slots = NULL, ...)

# S4 method for class 'giottoMulti,NULL'
materialize(gobject, view, space = NULL, coordinator = NULL, slots = NULL, ...)
```

## Arguments

- gobject:

  a `giotto` object

- view:

  either a `giottoView` or a `character(1)` slot key

- ...:

  reserved

- space:

  `character(1)` optional — name of a slotted `giottoSpace` naming the
  frame to return the data in. `NULL` means the native frame. This is
  the OUTPUT frame, and it is deliberately independent of the frame a
  crop step names, which says only which frame that step's region
  coordinates were read in. Defaulting one to the other is the
  conflation that made a `space`-bound view silently return transformed
  coordinates from a plain getter.

- coordinator:

  a
  [viewCoordinator](https://giotto-suite.github.io/GiottoClass/dev/reference/viewCoordinator-class.md)-inheriting
  object brokering IDs and joins between storage backings. Defaults to
  the coordinator selected from `gobject@source` (in-memory for non-disk
  gobjects).

- slots:

  optional `character` vector of slot names to narrow. When `NULL`
  (default), all slot lists are walked (`cell_metadata`, `expression`,
  `dimension_reduction`, `spatial_enrichment`, `feat_metadata`,
  `spatial_locs`, `spatial_info`, `feat_info`, `images`). When supplied,
  only the listed slots are walked — the rest are left untouched on the
  returned object. Useful for internal helpers that only consume a
  subset of slots and want to share one resolver pass without paying for
  irrelevant slots.

## Value

a new `giotto` object reflecting the resolved view
