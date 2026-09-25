# Class for coordinate-frame recipes

A `giottoSpace` is a handle over one named coordinate frame: an ordered
list of deferred spatial transforms that consumer functions opt into
with `space = "<name>"`. Every step carries its own `samples` scope, so
one list replays correctly for each sample.

It is virtual. The subclass says whether the samples INTERACT, which is
the only thing a job needs from a space (`adr/0006`):

- [combinedSpace](https://giotto-suite.github.io/GiottoClass/dev/reference/combinedSpace-class.md)
  — the samples are laid out relative to one another in one coordinate
  system, so cross-sample distances mean something and a job over it is
  ONE job. It declares its membership.

- [perSampleSpace](https://giotto-suite.github.io/GiottoClass/dev/reference/perSampleSpace-class.md)
  — each sample sits in its own copy of the frame and they never touch,
  so a job over it is N independent jobs. Steps may still be scoped per
  sample: two sections independently rotated upright are per-sample, not
  combined.

Access it with `[` (class-preserving, so the result stays editable) and
`[[` (extracts the steps that apply). Append to it with the transform
verbs –
[`spin()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spin.md),
[`spatShift()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatShift.md),
[`affine()`](https://giotto-suite.github.io/GiottoClass/dev/reference/affine.md),
[`flip()`](https://giotto-suite.github.io/GiottoClass/dev/reference/flip.md),
[`rescale()`](https://giotto-suite.github.io/GiottoClass/dev/reference/rescale.md),
[`shear()`](https://giotto-suite.github.io/GiottoClass/dev/reference/shear.md),
[`zoom()`](https://giotto-suite.github.io/GiottoClass/dev/reference/zoom.md)
– or compose two of the same kind with `+`. Export the plain form with
[`as.list()`](https://rdrr.io/r/base/list.html).

## Value

a `giottoSpace` object

## Slots

- `name`:

  `character(1)`. The frame's name, or `NA_character_` for a handle not
  yet slotted under one.

- `steps`:

  `list` of steps, in application order. Each step is a tagged plain
  list – `list(type = "transform", op = , args = , samples = )` –
  carrying no closure and no external pointer, so a recipe survives
  [`saveRDS()`](https://rdrr.io/r/base/readRDS.html) and reaches a
  parallel worker. `samples = NULL` broadcasts.

## See also

[`giottoSpace()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoSpace.md)
for the gobject-level accessors;
[giottoSpace-access](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoSpace-access.md)
for
[`names()`](https://giotto-suite.github.io/GiottoClass/dev/reference/names.md),
which reports membership;
[giottoView](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoView-class.md)
for the subset/narrowing recipe

## Examples

``` r
g <- spatShift(giotto(), dx = 10, space = "shifted")
sp <- giottoSpace(g, "shifted")
sp[["shifted"]]
as.list(sp)
```
