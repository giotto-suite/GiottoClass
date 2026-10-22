# Smooth expression over a spatial network

Parameters for
[`processData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/processData.md)
to replace each cell's values with a weighted average of its own values
and its neighbours' mean: \$\$s_j = (1 - b) x_j + b \frac{1}{d_j}
\sum\_{i \sim j} w\_{ij} x_i\$\$ where \\d_j\\ is the summed weight of
cell \\j\\'s edges. Edges count in both directions whatever the network
stores; a pair listed twice keeps the mean of its weights. Cells without
neighbours keep their values.

## Usage

``` r
spatialSmoothParam(
  smoothing = NULL,
  weighted = FALSE,
  hops = 1L,
  name = "smoothed",
  ...
)
```

## Arguments

- smoothing:

  numeric in `[0, 1]`, the weight \\b\\ of the neighbours. `NULL`
  (default) uses \\1 - 1/k\\ with \\k\\ the median number of neighbours,
  so a cell weighs as much as one neighbour. `0` returns the values
  unchanged.

- weighted:

  logical. Weight neighbours by the network's `weight` column instead of
  equally.

- hops:

  integer. Apply the average this many times, reaching neighbours up to
  `hops` edges away.

- name:

  character. Name of the expression values written by the giotto method.

- ...:

  additional named entries to attach to `@param`.

## Value

A `spatialSmoothParam` object.

## Examples

``` r
spatialSmoothParam(smoothing = 0.5)
```
