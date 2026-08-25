# Data Analysis

Generic for analyzing an object containing measured values, producing
computed outputs or summary statistics about the data rather than
transforming it. Specific methods should be defined for this generic to
perform analyses specific to a data class type. No methods are exported
from GiottoClass. The methods, which may differ depending on the input
data, are attached from other packages which focus on analyses and/or
alternative data representations with specific ways to implement those
analyses.

## Usage

``` r
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

## Value

A `data.table` of computed values or summary statistics
