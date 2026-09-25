# Fixed-radius network parameters

Every pair of nodes within `eps` of each other is joined. Unlike kNN,
node degree is not fixed – it follows local density, which is usually
what is meant by "cells that touch" in a tissue with varying cell
density.

Previously the only way to get this was `kNN` with a large `k` and a
`maximum_distance` filter, which searches for neighbours you then throw
away. This searches for the ones you asked for.

The graph is symmetric by construction, so it is **undirected**.

## Usage

``` r
radiusNetworkParam(
  eps,
  minimum_k = 0L,
  weight_fun = function(d) 1/(1 + d),
  include_weight = TRUE,
  include_distance = TRUE,
  output = c("auto", "data.table", "igraph", "parquet")
)
```

## Arguments

- eps:

  numeric. Radius within which nodes are joined. There is no default –
  the right value is set by the data's units and cell spacing. A
  reasonable starting point is a high quantile of the edge lengths of a
  Delaunay network on the same points.

- minimum_k:

  integer. Retain this many nearest neighbours per node even if they
  fall outside `eps`. Guards against isolated nodes in sparse regions,
  which otherwise drop out of the network entirely. Default `0`.

- weight_fun:

  function mapping distance to weight

- include_weight, include_distance:

  logical

- output:

  one of `"auto"`, `"data.table"`, `"igraph"`, `"parquet"`

## Value

a `radiusNetworkParam` object

## Choosing this over a filtered kNN

A kNN network with `maximum_distance` reproduces this graph exactly, but
only once `k` reaches the largest number of neighbours any node has
within `eps`. Below that it silently returns a truncated version, and
nothing in the output says which you got.

That threshold is the reason to prefer this, and it is not a speed
argument. [`dbscan::kNN`](https://rdrr.io/pkg/dbscan/man/kNN.html)
prunes as it searches while `frNN` enumerates the whole ball, so a
correctly sized kNN is usually the faster of the two. Measured at 50,000
points, the crossover sits at `k` of roughly 150-200 and moves little
with density or point count – below it kNN wins by up to ~5x, above it
`frNN` wins and the gap grows quickly.

So a filtered kNN is faster when the neighbourhood is small enough that
a modest `k` covers it. The catch is that the `k` you need is the max
degree within `eps`, which you do not know without computing it – which
is what `frNN` does. Reach for this when the radius is the thing you
mean and you would rather not guess.

## Performance

Backed by [`dbscan::frNN`](https://rdrr.io/pkg/dbscan/man/frNN.html),
which is exact. At 200,000 points with mean degree 6 the search takes ~2
s. `spatstat.geom::closepairs()` is roughly 28x faster and returns flat
index vectors rather than the per-point lists `frNN` has to be flattened
out of, but spatstat.geom is not a Giotto dependency and is 2D-only, so
it is mentioned rather than used.

## Examples

``` r
p <- radiusNetworkParam(eps = 25)
```
