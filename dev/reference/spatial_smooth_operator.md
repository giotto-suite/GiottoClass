# Spatial smoothing operator

The cells x cells matrix \\M\\ that
[`spatialSmoothParam()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatialSmoothParam.md)
applies as \\S = X M\\: \\M = (1 - b) I + b A D^{-1}\\, with \\A\\ the
symmetric weight matrix of the edges and \\D\\ its column sums. A cell
without neighbours keeps a self weight of 1. For backends that apply the
smoothing themselves.

## Usage

``` r
spatial_smooth_operator(
  from,
  to,
  weight = 1,
  n,
  smoothing = NULL,
  names = NULL,
  dedupe = TRUE
)
```

## Arguments

- from, to:

  integer cell indices (`1..n`) of the edges' ends; a pair counts once
  whichever way and however often it is listed, at the mean of its
  weights, and self edges are dropped

- weight:

  edge weights, recycled

- n:

  number of cells

- smoothing:

  neighbour weight \\b\\; `NULL` uses \\1 - 1/k\\, \\k\\ the median
  number of neighbours

- names:

  optional cell IDs for the dimnames

- dedupe:

  logical. `FALSE` skips merging pairs listed more than once, for
  callers whose pairs are already one per pair of cells, which saves a
  pass over the edges on large networks

## Value

a sparse `n x n` matrix

## Examples

``` r
spatial_smooth_operator(c(1, 2), c(2, 3), n = 3, smoothing = 0.5)
```
