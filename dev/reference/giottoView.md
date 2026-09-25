# Slotted views on a giotto object

List, retrieve, attach, or remove the view recipes held in a
[giotto](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto-class.md)
object's `@view` slot.

- `giottoView(g, "name")` — retrieve a view by name

- `giottoView(g, "name") <- v` — slot in (or replace) a view

- `giottoView(g, "name") <- NULL` — remove a view

- `giottoViews(g)` — list view names

A view is a
[giottoView](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoView-class.md).
There is no standalone constructor: record onto a name with
`subset(g, ..., view = "name")` or `crop(g, ..., view = "name")` and the
view is created on first use. The setter exists to copy a recipe between
objects, to slot one edited through
[giottoView-access](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoView-access.md),
and to remove one. It also accepts the plain nested
[`as.list()`](https://rdrr.io/r/base/list.html) form, so an exported
recipe reads back in.

Views are subset/narrowing recipes; for coordinate-frame recipes see
[giottoSpace](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoSpace.md).

## Usage

``` r
giottoView(gobject, name, ...)

giottoView(gobject, name, ...) <- value

giottoViews(gobject, ...)

# S4 method for class 'gAny,character'
giottoView(gobject, name, ...)

# S4 method for class 'gAny,missing'
giottoView(gobject, name, ...)

# S4 method for class 'gAny,character,ANY'
giottoView(gobject, name, ...) <- value

# S4 method for class 'gAny,character,NULL'
giottoView(gobject, name, ...) <- value

# S4 method for class 'gAny'
giottoViews(gobject, ...)
```

## Arguments

- gobject:

  a `giotto` object

- name:

  `character(1)`. The slot key.

- ...:

  additional arguments, currently unused

- value:

  a `giottoView`, its [`as.list()`](https://rdrr.io/r/base/list.html)
  form, or `NULL` to remove.

## Value

the view, an updated gobject, or a character vector of view names

## Examples

``` r
g <- giotto()
g <- subset(g, samples = c("s1", "s2"), view = "demo")
giottoViews(g)
giottoView(g, "demo")
```
