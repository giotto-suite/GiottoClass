# Data Clustering

Generic for clustering numeric matrix-like data. `param` accepts param
classes from the bioconductor bluster framework. No methods are exported
from GiottoClass. The methods, which may differ depending on the input
data, are attached from other packages which focus on analyses and/or
alternative data representations with specific ways to implement those
analyses.

## Usage

``` r
clusterData(x, param, ...)
```

## Arguments

- x:

  a data object

- param:

  a bluster::BlusterParam inheriting object

- ...:

  additional arguments, for use in specific methods

## Value

Clustering results. Exact outputs may depend on param settings
