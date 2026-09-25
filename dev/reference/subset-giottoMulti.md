# Subset a giottoMulti

Narrow the joint analysis view of the multi to a subset of global cell
IDs and/or global feature IDs. Eager: the surviving set is recorded on
`@cell_ID` / `@feat_ID` and every populated joint shared slot is trimmed
in place.

Children (`@objects`) are the spatial axis and are not touched. If you
want narrowed spatial content on a specific child, do that explicitly on
the child.

Subset returns a new `giottoMulti`; R copy-on-modify means the original
is untouched and acts as the "widen back" handle.

## Usage

``` r
# S4 method for class 'giottoMulti'
subset(
  x,
  subset,
  cells = NULL,
  features = NULL,
  negate = FALSE,
  samples = NULL,
  view = NULL,
  ...
)
```

## Arguments

- x:

  a `giottoMulti`

- subset:

  predicate expression, captured unevaluated. Only used with `view = `,
  where it becomes the recorded filter step; the eager path narrows by
  ID vector instead (`cells` / `features`).

- cells:

  `character` vector of global cell IDs to retain. `NULL` = no
  cell-level filter.

- features:

  `character` vector of global feature IDs to retain. `NULL` = no
  feature-level filter.

- negate:

  logical. Invert the predicate. Folded into the recorded predicate,
  matching `subset(<giotto>)`.

- samples:

  `NULL` or `character`. Children (or group names) to keep. Eagerly,
  this is `x[samples]`. With `view = `, it records a sample step on the
  view instead, resolved before the view's other steps.

- view:

  `NULL` or `character(1)`. When supplied, records the predicate as a
  filter step and/or `samples` as a sample step on the named view
  (created if new) instead of narrowing eagerly.

- ...:

  additional scope args forwarded to
  [`spatValues()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatValues.md)
  when a recorded filter step resolves

## Value

a `giottoMulti` with `@cell_ID` / `@feat_ID` narrowed and populated
joint slots trimmed accordingly. `@id_map` (the identity registry) is
left untouched — it records identity, not selection.

## Examples

``` r
if (FALSE) { # \dontrun{
subset(mg, cells = c("a::c1", "a::c2"))
subset(mg, samples = "a")
subset(mg, leiden_clus == 1, view = "cluster1")
subset(mg, samples = c("a", "b"), view = "pair")
} # }
```
