# Approximate k-nearest neighbors via HNSW

Find the `k` nearest neighbors of every row of `x` using an HNSW index
(Hierarchical Navigable Small World), returning the same structure as
[`dbscan::kNN()`](https://rdrr.io/pkg/dbscan/man/kNN.html) so the two
are interchangeable as the search step of network construction.

HNSW is *approximate*: recall is high but not guaranteed to be 1.0. Use
[`dbscan::kNN()`](https://rdrr.io/pkg/dbscan/man/kNN.html) when
exactness matters, or on small data where the exact search is both
faster and exact.

Repeated calls are reproducible by default. The index build is the only
nondeterministic phase – concurrent insertion makes the graph depend on
thread interleaving – so `n_threads_build` defaults to `1`. Searching is
unaffected and stays parallel.

## Usage

``` r
hnswKNN(
  x,
  k,
  distance = c("euclidean", "cosine", "l2", "ip"),
  M = 16L,
  ef_construction = 200L,
  ef = 200L,
  n_threads = NULL,
  n_threads_build = 1L,
  ...
)
```

## Arguments

- x:

  numeric matrix. Rows are observations (cells), columns are dimensions
  (typically PCA coordinates).

- k:

  integer. Number of neighbors to return per observation, excluding the
  observation itself.

- distance:

  character. Metric, one of `"euclidean"` (default), `"cosine"`, `"l2"`
  (squared euclidean) or `"ip"` (inner product).

- M:

  integer. HNSW graph degree (default 16). Higher improves recall at the
  cost of memory and build time.

- ef_construction:

  integer. Beam width during index construction (default 200). Higher
  improves recall at the cost of build time.

- ef:

  integer. Beam width during search (default 200). This is the
  recall/speed dial: higher `ef` searches more of the graph, bringing
  the result closer to the exact
  [`dbscan::kNN()`](https://rdrr.io/pkg/dbscan/man/kNN.html) answer at
  the cost of query time. Raised to at least `k + 1`. On 158,662 cells
  at `k = 30`, `ef = 50` reproduced 99.225% of the exact network's
  undirected edges and `ef = 200` reproduced 99.995%, for 2.30s against
  2.83s.

- n_threads:

  integer. Threads for the **search**. Defaults to
  [`GiottoUtils::determine_cores()`](https://giotto-suite.github.io/GiottoUtils/reference/determine_cores.html).
  The search is deterministic at any thread count, so this can be left
  parallel.

- n_threads_build:

  integer or `NULL`. Threads for the index **build**, default `1`. A
  multithreaded build is not reproducible: insertion order varies, so
  the graph and hence the neighbours differ slightly between runs, which
  propagates to clustering even with a fixed seed. Building on one
  thread makes repeated calls bit-identical. Set to `NULL` to inherit
  `n_threads` and trade reproducibility for speed while exploring –
  measured at 11.8s against 2.82s on 158,662 cells.

- ...:

  unused, for signature compatibility with
  [`dbscan::kNN()`](https://rdrr.io/pkg/dbscan/man/kNN.html).

## Value

object of class `c("kNN", "NN")` with elements `id` (integer matrix,
`nrow(x)` x `k`), `dist` (numeric matrix, same shape), `k`, `sort` and
`metric`. `id` and `dist` carry
[`dbscan::kNN()`](https://rdrr.io/pkg/dbscan/man/kNN.html)'s dimnames
convention: `rownames(x)` on the rows, `"1".."k"` on the columns.

## Examples

``` r
if (FALSE) { # \dontrun{
m <- matrix(rnorm(1000 * 20), nrow = 1000)
nn <- hnswKNN(m, k = 30)
str(nn$id)
} # }
```
