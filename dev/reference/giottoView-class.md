# Class for subset / narrowing recipes

A `giottoView` is a deferred, read-only NARROWING of a
[giotto](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto-class.md)
or
[giottoMulti](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoMulti-class.md)
object: a recipe, re-resolved against the object's current state each
time it is consumed, rather than a snapshot.

Access it with `[` (class-preserving, so the result stays editable) and
`[[` (extracts a step). Append to it with the builder verbs
[`subset()`](https://rdrr.io/r/base/subset.html),
[`crop()`](https://giotto-suite.github.io/GiottoClass/dev/reference/crop.md),
and
[`selectSamples()`](https://giotto-suite.github.io/GiottoClass/dev/reference/selectSamples.md),
or compose two with `+`. Export the plain nested form with
[`as.list()`](https://rdrr.io/r/base/list.html).

## Value

a `giottoView` object

## Slots

- `steps`:

  `list` of recorded steps, in order. Each step is a tagged plain list –
  `list(type = "filter" | "crop" | "samples", ...)` – and carries no
  closure and no external pointer, so a recipe survives
  [`saveRDS()`](https://rdrr.io/r/base/readRDS.html) and reaches a
  parallel worker. A `crop` step also carries the `space` its region was
  drawn in.

## See also

[`giottoView()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoView.md)
for the gobject-level accessors;
[giottoSpace](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoSpace-class.md)
for the coordinate-frame recipe

## Examples

``` r
g <- crop(giotto(), c(0, 10, 0, 10), view = "v")
v <- giottoView(g, "v")
v[[1L]]
as.list(v)
```
