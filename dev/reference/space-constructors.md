# Build a coordinate frame directly

Build an empty frame of a given kind, to slot in with
`giottoSpace(g, "name") <- `. Transforms are recorded onto it afterwards
with the usual verbs.

`combinedSpace()` exists because recording onto an unused name creates a
[perSampleSpace](https://giotto-suite.github.io/GiottoClass/dev/reference/perSampleSpace-class.md).
A frame whose samples share one coordinate system has to say so, and
this is where it says it. It also seeds membership up front, which
matters for a member that needs no transform of its own: a sample at the
layout's origin is still in the layout, and nothing would otherwise
record it.

`perSampleSpace()` is rarely needed, since that is the kind recording
gives you — it is here to build one before any step exists, or to state
the kind explicitly.

## Usage

``` r
combinedSpace(samples = character(), name = NA_character_)

perSampleSpace(name = NA_character_)
```

## Arguments

- samples:

  `character`. Member sample names.

- name:

  `character(1)`. Optional; `giottoSpace<-` sets it on slotting.

## Value

a `combinedSpace` or `perSampleSpace`

## Examples

``` r
mg <- giotto()
giottoSpace(mg, "upright") <- perSampleSpace()
giottoSpace(mg, "upright")
```
