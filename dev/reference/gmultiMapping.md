# gmulti federation mapping accessor

Get or set the `@mapping` slot: declares which child-level spat_units,
feat_types and expression names (`values`) federate up to gmulti-level
handles, with per-child name reconciliation.

Auto-discovered at construction: `spat_unit` / `feat_type` get the
symmetric trivial mapping (handle == child-level name), and `values` is
seeded to the ingest convention `"raw"`. Every entry keys **every**
sample; `NA_character_` is the deliberate-skip sentinel ("this sample
does not contribute to this handle"). A keyed sample whose child cannot
satisfy the entry is a loud error at read time, naming the sample.

The setter has three forms:

- Full replacement:
  `gmultiMapping(mg) <- list(spat_unit = ..., feat_type = ..., values = ...)`

- Whole-axis replacement:
  `gmultiMapping(mg, "spat_unit") <- list(cell = ..., nucleus = ...)`

- Single-entry replacement:
  `gmultiMapping(mg, "values", "raw") <- c(B191 = "raw", B215 = "counts")`

Setting a single-entry vector to `NULL` removes that entry. Assigning
the top-level mapping to `NULL` triggers fresh auto-discovery from the
current child population.

All setter forms validate the resulting mapping and invalidate joint
slot state per affected universe — joint state for unrelated universes
survives untouched. **Expansion is blocked once a universe is
materialized**: joint content was built from a specific participation
set, so widening the declaration afterwards would leave content that
omits a sample the declaration now claims. Opting a sample in is
explicit — drop the joint content for that universe, then re-declare.

## Usage

``` r
gmultiMapping(x, ...)

# S4 method for class 'giottoMulti'
gmultiMapping(x, which = NULL, ...)

gmultiMapping(x, ...) <- value

# S4 method for class 'giottoMulti'
gmultiMapping(x, ...) <- value
```

## Arguments

- x:

  a `giottoMulti`

- ...:

  see `which` and `handle`

- which:

  one of `"spat_unit"`, `"feat_type"` or `"values"` to narrow the return
  / target axis; default `NULL` returns or replaces the full mapping
  list

- value:

  depends on the setter form: full mapping list, axis-shaped list,
  single per-sample-named char vector, or `NULL`

- handle:

  a single gmulti-level handle name (e.g. `"cell"`) under the chosen
  axis; used only by the single-entry setter

## Value

the requested mapping (full list or one axis)
