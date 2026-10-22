# Feature and cell summary statistics parameters

Parameter classes for
[`analyzeData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/analyzeData.md)
that compute summary statistics over matrix-type data.

- `featStatsParam` — per feature: `nr_cells`, `perc_cells`,
  `total_expr`, `mean_expr`, `mean_expr_det`. With `groups =`, the
  statistics are taken per (feature, group) instead.

- `cellStatsParam` — per cell: `nr_feats`, `perc_feats`, `total_expr`.

Both read one setting from `@param`, `detection_threshold` (default
`0`): values above it count as detected. Build them with `new()`, e.g.
`new("featStatsParam", param = list(detection_threshold = 0))`.

## See also

[`analyzeData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/analyzeData.md)
