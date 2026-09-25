# Access a coordinate-frame recipe

A handle holds one space, so the index is the SAMPLE — `sp["a"]` and
`sp[["a"]]` both read as "this space, for sample a". `[` narrows and
returns a
[giottoSpace](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoSpace-class.md),
so the result is still a recipe and stays appendable; `[[` extracts the
plain step list.

A numeric index is a step position instead, following `l[["a"]]` vs
`l[[1]]`: character selects by name, numeric by position.

- `sp[i]` — character: narrowed to sample `i`, every surviving step
  rescoped to it rather than stripped, so `sp["a"] + sp["b"]`
  reconstructs the original. Numeric: the space holding steps `i`.

- `sp[[i]]` — character: the ordered step `list` that applies to sample
  `i`. Numeric: step `i`, raw.

- `names(sp)` — the samples this recipe mentions

- `length(sp)` — step count

- `as.list(sp)` — the plain export form

- `sp1 + sp2` — concatenate two spaces' steps. The result is unnamed:
  composition says how to build a recipe, not where it belongs, so name
  it when you place it

## Usage

``` r
# S4 method for class 'giottoSpace,ANY,ANY,ANY'
x[i, j, ..., drop = TRUE]

# S4 method for class 'giottoSpace,ANY,ANY'
x[[i, j, ...]]

# S4 method for class 'giottoSpace'
length(x)

# S4 method for class 'giottoSpace'
names(x)

# S4 method for class 'combinedSpace'
as.list(x, ...)

# S4 method for class 'perSampleSpace'
as.list(x, ...)

# S4 method for class 'combinedSpace,combinedSpace'
e1 + e2

# S4 method for class 'perSampleSpace,perSampleSpace'
e1 + e2

# S4 method for class 'giottoSpace,giottoSpace'
e1 + e2

# S4 method for class 'combinedSpace'
show(object)

# S4 method for class 'perSampleSpace'
show(object)
```

## Arguments

- x:

  a `giottoSpace`

- i:

  `character(1)` sample name, or `numeric` step position

- j:

  not used — spaces are indexed on one axis

- ...:

  additional arguments, currently unused

- e1, e2:

  `giottoSpace` objects to compose

## Value

a `giottoSpace` for `[` and `+`; a `list` for `[[` and
[`as.list()`](https://rdrr.io/r/base/list.html)

## Sample resolution

One rule, both kinds, resolved here and nowhere else: a step with no
scope applies to every sample; a scoped step applies only to the samples
it names.

`NA_character_` means "no sample identity" — a plain
[giotto](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto-class.md),
which is one sample that has no name. If the whole recipe mentions
exactly one sample, that is who the handle is about and it resolves for
it, which is how `sp["a"][[NA_character_]]` reads its steps back without
being told the name twice. Two or more and it does not guess; none, and
the unscoped steps are the whole answer.

`sp[NA_character_]` is an error: narrowing needs a name, and "narrow to
nobody" has no meaning.

## Membership

[`names()`](https://giotto-suite.github.io/GiottoClass/dev/reference/names.md)
says which samples the recipe MENTIONS. Whether that is the whole story
is the class's job, not the accessor's:

- [combinedSpace](https://giotto-suite.github.io/GiottoClass/dev/reference/combinedSpace-class.md)
  — closed. These are the members, and an unscoped step reaches exactly
  them.

- [perSampleSpace](https://giotto-suite.github.io/GiottoClass/dev/reference/perSampleSpace-class.md)
  — open. An unscoped step also reaches samples that appear nowhere in
  the recipe, so coverage is whatever the object holds. A consumer
  sizing a job must read it from the object, which is why nothing asks a
  `perSampleSpace` how many samples there are.

## Examples

``` r
g <- spatShift(giotto(), dx = 10, space = "shifted")
sp <- giottoSpace(g, "shifted")

names(sp)
length(sp)
sp[[NA_character_]]
```
