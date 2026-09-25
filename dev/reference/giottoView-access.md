# Access a view recipe

`[` selects steps and returns a
[giottoView](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoView-class.md),
so the result is still appendable. `[[` extracts one step as the plain
tagged list it is recorded as; `[[<-` replaces or (with `NULL`) drops
one.

- `v[i]` — a `giottoView` holding steps `i`. Negative indices drop.

- `v[[i]]` — step `i`, a plain `list`

- `v[i, j]` — the value of attribute `j` on step `i`

- `length(v)` / `names(v)` — step count / step types, in recorded order

- `as.list(v)` — the plain nested form

- `v1 + v2` — concatenate steps. Unconditional: each crop step carries
  its own frame, so there is nothing to reconcile.

## Usage

``` r
# S4 method for class 'giottoView,ANY,ANY,ANY'
x[i, j, ..., drop = TRUE]

# S4 method for class 'giottoView,ANY,ANY'
x[[i, j, ...]]

# S4 method for class 'giottoView,ANY,ANY,ANY'
x[[i, j, ...]] <- value

# S4 method for class 'giottoView'
length(x)

# S4 method for class 'giottoView'
names(x)

# S4 method for class 'giottoView'
as.list(x, ...)

# S4 method for class 'giottoView,giottoView'
e1 + e2

# S4 method for class 'giottoView'
show(object)
```

## Arguments

- x:

  a `giottoView`

- i:

  step selector — `numeric` or `logical`

- j:

  `character(1)`. Step attribute to read.

- ...:

  additional arguments, currently unused

- value:

  replacement step, or `NULL` to drop

- e1, e2:

  `giottoView` objects to compose

## Value

a `giottoView` for `[` and `+`; a `list` for `[[` and
[`as.list()`](https://rdrr.io/r/base/list.html); the attribute value for
`v[i, j]`

## Examples

``` r
g <- crop(giotto(), c(0, 10, 0, 10), view = "v")
g <- crop(g, c(0, 5, 0, 5), relation = "within", view = "v")
v <- giottoView(g, "v")

length(v)
names(v)
v[[2L]]
v[2L, "relation"]
v[-1L]
```
