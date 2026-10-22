# Data Analysis

Generic for analyzing an object containing measured values, producing
computed outputs or summary statistics about the data rather than
transforming it. Specific methods should be defined for this generic to
perform analyses specific to a data class type. GiottoClass provides the
arithmetic summaries other packages build on
([featStatsParam](https://giotto-suite.github.io/GiottoClass/dev/reference/featStatsParam-class.md),
[cellStatsParam](https://giotto-suite.github.io/GiottoClass/dev/reference/featStatsParam-class.md))
and
[labelProportionsParam](https://giotto-suite.github.io/GiottoClass/dev/reference/labelProportionsParam-class.md).
Methods that embody an analysis choice, or that implement these for
alternative data representations, are attached from other packages.

## Usage

``` r
analyzeData(x, param, ...)

# S4 method for class 'allMatrix,edgeStatsParam'
analyzeData(x, param, ..., network, groups = NULL)

# S4 method for class 'allMatrix,featCorParam'
analyzeData(x, param, ..., network = NULL)

# S4 method for class 'exprObj,analyzeParam'
analyzeData(x, param, ...)

# S4 method for class 'allMatrix,featStatsParam'
analyzeData(x, param, ..., groups = NULL, stats = NULL)

# S4 method for class 'allMatrix,cellStatsParam'
analyzeData(x, param, ...)

# S4 method for class 'igraph,labelProportionsParam'
analyzeData(x, param, ..., labels = NULL)

# S4 method for class 'giottoPolygon,labelProportionsParam'
analyzeData(x, param, ..., labels = NULL, y = NULL)

# S4 method for class 'giotto,labelProportionsParam'
analyzeData(
  x,
  param,
  ...,
  spat_unit = NULL,
  feat_type = NULL,
  output = c("data.table", "matrix", "spatEnrObj", "gobject"),
  verbose = NULL
)
```

## Arguments

- x:

  a data object

- param:

  a
  [analyzeParam](https://giotto-suite.github.io/GiottoClass/dev/reference/analyzeParam-class.md)
  inheriting object

- ...:

  additional arguments, for use in specific methods

- network:

  for a `featCorParam` with `smooth`: the spatial network to smooth over

- groups:

  optional vector of group assignments, one per column of `x`, `NA` to
  exclude. When supplied, the statistics are taken per (feature, group)
  instead of over every cell, and the result gains `group` and `n_cells`
  columns.

- stats:

  optional character vector of accumulators to compute, any of `"sum"`,
  `"sumsq"`, `"nnz"`, `"sum_det"`. Grouped path only; emitted columns
  are whichever the requested accumulators support.

## Value

A `data.table` of computed values or summary statistics
