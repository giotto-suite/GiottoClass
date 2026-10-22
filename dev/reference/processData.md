# Data Processing

Generic for processing an object containing measured values. Specific
methods should be defined for this generic to perform pre or post
processing specific to a data class type. No methods are exported from
GiottoClass. The methods, which may differ depending on the input data,
are attached from other packages which focus on analyses and/or
alternative data representations with specific ways to implement those
analyses.

## Usage

``` r
processData(x, param, ...)

# S4 method for class 'allMatrix,spatialSmoothParam'
processData(x, param, ..., network)

# S4 method for class 'giotto,spatialSmoothParam'
processData(
  x,
  param,
  ...,
  spat_unit = NULL,
  feat_type = NULL,
  expression_values = "normalized",
  spatial_network_name = "Delaunay_network",
  feats = NULL
)
```

## Arguments

- x:

  a data object

- param:

  a
  [processParam](https://giotto-suite.github.io/GiottoClass/dev/reference/processParam-class.md)
  inheriting object

- ...:

  additional arguments, for use in specific methods

- network:

  a spatial network: an `igraph` or a data.frame with `from`, `to` (cell
  IDs) and, for `weighted = TRUE`, `weight`

- spat_unit, feat_type:

  spatial unit and feature type

- expression_values:

  expression values to smooth

- spatial_network_name:

  spatial network to smooth over

- feats:

  features to smooth (default all)

## Value

An object of the same class containing the processed values
