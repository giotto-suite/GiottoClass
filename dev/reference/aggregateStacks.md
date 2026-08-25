# aggregateStacks

aggregate expression matrices from different z-stacks

## Usage

``` r
aggregateStacks(
  gobject,
  spat_units,
  feat_type,
  values,
  summarize_expression = "sum",
  summarize_locations = "mean",
  for_loop = FALSE,
  for_loop_group_size = 100,
  new_spat_unit = "aggregate",
  verbose = TRUE
)
```

## Arguments

- gobject:

  giotto object

- spat_units:

  spatial units to aggregate

- feat_type:

  feature type

- values:

  values to use

- summarize_expression:

  method to summarize expression information

- summarize_locations:

  method to summarize spatial location information

- for_loop:

  aggregate polygons in for loop (default = FALSE)

- for_loop_group_size:

  size of polygon groups to aggregate in each loop

- new_spat_unit:

  new name for aggregated spatial unit

- verbose:

  verbosity

## Value

giotto object

## Details

Combines both
[`aggregateStacksExpression`](https://giotto-suite.github.io/GiottoClass/dev/reference/aggregateStacksExpression.md)
and
[`aggregateStacksLocations`](https://giotto-suite.github.io/GiottoClass/dev/reference/aggregateStacksLocations.md)

## See also

Other aggregate stacks:
[`aggregateStacksExpression()`](https://giotto-suite.github.io/GiottoClass/dev/reference/aggregateStacksExpression.md),
[`aggregateStacksLocations()`](https://giotto-suite.github.io/GiottoClass/dev/reference/aggregateStacksLocations.md),
[`aggregateStacksPolygonOverlaps()`](https://giotto-suite.github.io/GiottoClass/dev/reference/aggregateStacksPolygonOverlaps.md),
[`aggregateStacksPolygons()`](https://giotto-suite.github.io/GiottoClass/dev/reference/aggregateStacksPolygons.md)

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")

aggregateStacks(g,
    spat_units = c("z0", "z1"), feat_type = "rna",
    values = "raw"
)
```
