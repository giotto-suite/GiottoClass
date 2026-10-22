# Correlation between features

Parameters for
[`analyzeData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/analyzeData.md)
to correlate every pair of features (rows) across cells (columns).

## Usage

``` r
featCorParam(method = c("pearson", "spearman"), smooth = NULL, ...)
```

## Arguments

- method:

  character. `"pearson"` (default) or `"spearman"`.

- smooth:

  optional
  [`spatialSmoothParam()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatialSmoothParam.md):
  correlate the values after smoothing them over the spatial network
  passed to
  [`analyzeData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/analyzeData.md)
  as `network`

- ...:

  additional named entries to attach to `@param`.

## Value

A `featCorParam` object.

## Details

A feature with the same value in every cell has no defined correlation;
its row and column are `NA`.

## Examples

``` r
m <- matrix(rpois(60, 3), nrow = 3,
    dimnames = list(c("a", "b", "c"), NULL))
analyzeData(m, featCorParam())
```
