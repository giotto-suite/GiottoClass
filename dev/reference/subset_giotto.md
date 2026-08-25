# Subset a `giotto` object

Subset a giotto object with `[` or
[`subset()`](https://rdrr.io/r/base/subset.html) generic. The
implementation is different from
[`subsetGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/subsetGiotto.md)
in that all spatial units will always be affected. The feature type to
subset can be specified.

## Usage

``` r
# S4 method for class 'giotto,gIndex,missing,missing'
x[i, j, ..., drop = TRUE]

# S4 method for class 'giotto,missing,gIndex,missing'
x[i, j, ..., drop = TRUE]

# S4 method for class 'giotto,gIndex,gIndex,missing'
x[i, j, ..., drop = TRUE]

# S4 method for class 'giotto,missing,missing,missing'
x[i, j, ..., drop = TRUE]

# S4 method for class 'giotto'
subset(
  x,
  subset,
  feat_ids = NULL,
  cell_ids = NULL,
  spat_unit = NULL,
  feat_type = NULL,
  negate = FALSE,
  quote = TRUE,
  ...
)
```

## Arguments

- x:

  a `giotto` object

- ...:

  additional params to pass to `spatValues` used with the subset param

- drop:

  not used

- subset:

  Logical expression evaluated in expression values

- feat_ids, i:

  character vector. Feature IDs to subset the object for.

- cell_ids, j:

  character vector. Cell/spatial IDs to subset the object for.

- spat_unit:

  character. Controls which spatial unit to pull subsetting information
  from when using `cell_ids`/`j` and `subset` params. However, all
  spatial units will always be affected by the subset.

- feat_type:

  character. Subset affects these feature type(s). Default is `"rna"`

- negate:

  logical. if `TRUE` all IDs that are **not** in the `subset` are
  selected

- quote:

  logical. If `TRUE`, the `subset` param will be quoted with
  [`substitute()`](https://rdrr.io/r/base/substitute.html). Set this to
  `FALSE` when calling from a function, although that may not be
  recommended since NSE output can be unexpected when not used
  interactively.

## Value

giotto object

## Functions

- `x[i`: Subset giotto objects

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")

# `[` examples
g[1:5]
g[, 2:10]
g[1:5, 2:10]
g[c(TRUE, FALSE), ]

# subset() examples
subset(g, nr_feats > 300)
subset(g, nr_feats > 300,
    cell_ids = c("GAATCGCCGGACACGG-1", "GAGGGCATCGCGTATC-1")
)
subset(g, Gfap + Gna12 > 10)
```
