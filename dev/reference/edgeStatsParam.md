# Edge statistics parameters

Settings for
[`analyzeData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/analyzeData.md)
that compute per-feature sums over a spatial network: power sums of the
values, degree-weighted sums, and the sum over edges of the product of
the values at both ends.

## Usage

``` r
edgeStatsParam(weighted = FALSE, ...)
```

## Arguments

- weighted:

  logical. Use the network's `weight` column (`TRUE`) or count every
  edge as 1 (`FALSE`).

- ...:

  additional named entries to attach to `@param`.

## Value

An
[edgeStatsParam](https://giotto-suite.github.io/GiottoClass/dev/reference/edgeStatsParam-class.md)
object.

## Examples

``` r
m <- Matrix::rsparsematrix(4, 6, 0.6, rand.x = function(n) rpois(n, 3) + 1)
dimnames(m) <- list(paste0("g", 1:4), paste0("c", 1:6))
net <- data.frame(from = paste0("c", 1:5), to = paste0("c", 2:6))
analyzeData(m, edgeStatsParam(), network = net)
```
