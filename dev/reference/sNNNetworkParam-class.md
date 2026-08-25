# sNNNetworkParam — Shared-Nearest-Neighbour Network Param

Constructor and class for shared-Nearest-Neighbour network parameters.
sNN edges are symmetric by definition (`|N(a) ∩ N(b)| = |N(b) ∩ N(a)|`),
so the resulting graph is **undirected** — one edge per pair.

## Usage

``` r
sNNNetworkParam(
  k = 30L,
  top_shared = 3L,
  minimum_shared = 5L,
  weight_fun = function(d) 1/(1 + d),
  include_weight = TRUE,
  include_distance = TRUE,
  output = c("auto", "data.table", "igraph", "parquet"),
  engine = c("dbscan", "hnsw"),
  ef = 200,
  n_threads_build = 1L
)
```

## Arguments

- k:

  number of neighbours used to compute sharing

- top_shared:

  keep at least this many edges per node

- minimum_shared:

  keep edges with at least this many shared neighbours

- weight_fun:

  function mapping distance to weight

- include_weight, include_distance:

  include columns in output

- output:

  one of `"auto"`, `"data.table"`, `"igraph"`, `"parquet"`

- engine:

  character. kNN search backend. `"dbscan"` (default) is exact and
  single-threaded, and is the better choice on small data where it is
  also faster. `"hnsw"` uses an approximate HNSW index via
  [`hnswKNN()`](https://giotto-suite.github.io/GiottoClass/dev/reference/hnswKNN.md)
  – multithreaded and free of the kd-tree's degradation at PCA
  dimensionality, which is what makes it worth reaching for on large
  datasets. On 158,662 cells at `k = 30` it reproduced 99.995% of the
  exact network's undirected edges in 11.8s against 79.9s. Requires
  RcppHNSW. An `"auto"` setting that picks by dataset size is planned.

- ef:

  integer. `"hnsw"` only, ignored otherwise. Search beam width, default
  200 – the recall/speed dial. Higher values search more of the graph,
  moving the result closer to the exact `"dbscan"` answer at the cost of
  query time.

- n_threads_build:

  integer or `NULL`. `"hnsw"` only, ignored otherwise. Threads for the
  index build, default `1`. A parallel build is not reproducible –
  insertion order varies, so neighbours differ slightly between runs and
  that propagates to clustering even with a fixed seed. `NULL` inherits
  the search thread count and trades reproducibility for speed.

## Slots

- `k`:

  integer. number of nearest neighbours used to compute sharing.

- `top_shared`:

  integer. keep at least this many edges per node.

- `minimum_shared`:

  integer. keep edges with at least this many shared neighbours.

- `weight_fun`:

  function. weight = `weight_fun(distance)`.

- `include_weight,include_distance`:

  logical. include columns.

- `output`:

  character. See
  [`createNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createNetwork.md).
