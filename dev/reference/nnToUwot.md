# Convert a kNN object to uwot's precomputed-neighbor format

Reshape a `c("kNN", "NN")` object – from
[`hnswKNN()`](https://giotto-suite.github.io/GiottoClass/dev/reference/hnswKNN.md)
or [`dbscan::kNN()`](https://rdrr.io/pkg/dbscan/man/kNN.html) – into the
`list(idx =, dist =)` that `uwot::umap()` and `uwot::umap2()` accept as
`nn_method`, so a graph built once can be handed to UMAP instead of
letting uwot run a second, independent search.

## Usage

``` r
nnToUwot(nn)
```

## Arguments

- nn:

  object of class `kNN`/`NN`, or a list with `id` and `dist` matrices of
  equal dimensions.

## Value

list with `idx` (integer matrix, `n` x `k + 1`) and `dist` (numeric
matrix, same shape), suitable as uwot's `nn_method`.

## Details

Two differences have to be reconciled, and uwot validates neither, so
getting either wrong corrupts the embedding silently rather than raising
an error:

- uwot names the neighbor matrix `idx`; `kNN` objects name it `id`.

- uwot requires each observation to be its own first neighbor
  (`idx[, 1] == seq_len(n)`, `dist[, 1] == 0`), because it drops column
  1 when fitting the local-connectivity offset. Both
  [`hnswKNN()`](https://giotto-suite.github.io/GiottoClass/dev/reference/hnswKNN.md)
  and [`dbscan::kNN()`](https://rdrr.io/pkg/dbscan/man/kNN.html)
  *remove* self-matches. Passing their output unchanged therefore
  discards every observation's true nearest neighbor and fits `rho`
  against a shifted distance set.

The returned matrices are `k + 1` columns wide: the self column plus the
`k` neighbors. uwot ignores `n_neighbors` when given a graph, so that
width is what sets the neighborhood size.

## Examples

``` r
if (FALSE) { # \dontrun{
m <- matrix(rnorm(1000 * 20), nrow = 1000)
nn <- hnswKNN(m, k = 29)
uwot::umap2(m, nn_method = nnToUwot(nn))
} # }
```
