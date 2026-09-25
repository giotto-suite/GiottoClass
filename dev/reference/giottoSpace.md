# Slotted spaces on a giotto object

List, retrieve, attach, or remove giottoSpace objects slotted into a
[giotto](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto-class.md)
object's `@spaces` slot.

- `giottoSpace(g, "name")` — retrieve a space by name

- `giottoSpace(g, "name") <- s` — slot in (or replace) a space

- `giottoSpace(g, "name") <- NULL` — remove a space

- `giottoSpaces(g)` — list space names

Multiple slotted spaces serve as named alternate coordinate frames of
the same gobject. Consumer functions opt into a frame via the `space =`
parameter.

A space is a
[giottoSpace](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoSpace-class.md).
There is no standalone constructor: record onto a name with a transform
verb, e.g. `spatShift(g, dx = 10, space = "shifted")`, and the space is
created on first use. On a `giottoMulti`, `samples =` scopes the
transform to named children. The setter exists to copy a recipe between
objects, to slot one edited through
[giottoSpace-access](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoSpace-access.md),
and to remove one. It also accepts the plain nested
[`as.list()`](https://rdrr.io/r/base/list.html) form, so an exported
recipe reads back in.

A handle holds exactly one frame, so `giottoSpace(g)` with no `name`
returns a named `list` of them; `giottoSpace(g, "name")` returns the
one.

The native frame — the one the data is already in — has no name.
`space = NULL` is it, everywhere. A sentinel name would be a second
spelling of a value R already has, and a transform cannot be recorded
onto the native frame anyway: the result would not be native.

## Usage

``` r
giottoSpace(gobject, name, ...)

giottoSpace(gobject, name, ...) <- value

giottoSpaces(gobject, ...)

# S4 method for class 'gAny,character'
giottoSpace(gobject, name, ...)

# S4 method for class 'gAny,missing'
giottoSpace(gobject, name, ...)

# S4 method for class 'gAny,character,ANY'
giottoSpace(gobject, name, ...) <- value

# S4 method for class 'gAny,character,NULL'
giottoSpace(gobject, name, ...) <- value

# S4 method for class 'gAny'
giottoSpaces(gobject, ...)
```

## Arguments

- gobject:

  a `giotto` object

- name:

  `character(1)`. The slot key.

- ...:

  additional arguments (none currently used)

- value:

  a `giottoSpace`, its [`as.list()`](https://rdrr.io/r/base/list.html)
  form, or `NULL` to remove.

## Value

the space, an updated gobject, or a character vector of space names

## Examples

``` r
g <- giotto()
g <- spatShift(g, dx = 10, space = "demo")
giottoSpaces(g)
giottoSpace(g, "demo")
```
