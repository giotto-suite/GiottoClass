# Select samples within a gmulti-scoped view

Record a sample-selection step on a named view that will be consumed
against a
[giottoMulti](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoMulti-class.md).
Picks which children participate in resolution. The step is resolved
FIRST, before any other step. Warns at resolution time if the parent is
not a `giottoMulti`.

## Usage

``` r
selectSamples(x, ..., view)

# S4 method for class 'giottoView'
selectSamples(x, ..., view)

# S4 method for class 'gAny'
selectSamples(x, ..., view)
```

## Arguments

- x:

  a `giotto` / `giottoMulti` object, or a
  [giottoView](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoView-class.md)

- ...:

  `character` child names (or a single `character` vector)

- view:

  `character(1)`. Name of the view to record onto; created if it does
  not exist yet. Not used when `x` is already a `giottoView`.

## Value

`x`, with the sample-select step recorded on the named view

## Examples

``` r
g <- giotto()
g <- selectSamples(g, "sample1", "sample2", view = "pair")
giottoViews(g)

# or directly on the recipe
selectSamples(giottoView(g, "pair"), "sample3")
```
