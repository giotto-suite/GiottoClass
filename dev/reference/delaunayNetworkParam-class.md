# delaunayNetworkParam — Delaunay Network Param

Constructor and class for Delaunay triangulation network parameters.
Delaunay edges are an undirected geometric relation, so the resulting
graph is **undirected** — one edge per pair.

## Usage

``` r
delaunayNetworkParam(
  method = c("geometry", "deldir", "RTriangle"),
  maximum_distance = "auto",
  minimum_k = 0L,
  weight_fun = function(d) 1/d,
  include_weight = TRUE,
  include_distance = TRUE,
  output = c("auto", "data.table", "igraph", "parquet"),
  options = "Pp",
  Y = TRUE,
  j = TRUE,
  S = 0
)
```

## Arguments

- method:

  backend: `"geometry"` (default), `"deldir"`, or `"RTriangle"`

- maximum_distance:

  maximum edge length, or `"auto"`, or `NULL`

- minimum_k:

  minimum neighbours per node when filtering

- weight_fun:

  function mapping distance to weight

- include_weight, include_distance:

  include columns in output

- output:

  one of `"auto"`, `"data.table"`, `"igraph"`, `"parquet"`

- options:

  *geometry only.* passed to
  [`geometry::delaunayn`](https://rdrr.io/pkg/geometry/man/delaunayn.html)

- Y, j, S:

  *RTriangle only.* passed to `RTriangle::triangulate`
