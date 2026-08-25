# Data Reduction

Generic for reducing an object containing measured values to a
lower-dimensional decomposition or embedding (PCA, UMAP, tSNE, ...),
distinct from
[`processData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/processData.md)
(same-shape transform),
[`analyzeData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/analyzeData.md)
(summary stats), and
[`filterData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/filterData.md)
(selection). Specific methods should be defined for this generic to
perform reductions specific to a data class type. No methods are
exported from GiottoClass. The methods, which may differ depending on
the input data, are attached from other packages which focus on
reduction methods and/or alternative data representations with specific
ways to implement those reductions.

## Usage

``` r
reduceData(x, param, ...)
```

## Arguments

- x:

  a data object

- param:

  a
  [reduceParam](https://giotto-suite.github.io/GiottoClass/dev/reference/reduceParam-class.md)
  inheriting object

- ...:

  additional arguments, for use in specific methods

## Value

A decomposition (typically a list of matrices/vectors, e.g.
`list(u, d, v, sdev, eigenvalues)` for PCA)
