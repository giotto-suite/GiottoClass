# aggregateStacksLocations

aggregate expression matrices from different z-stacks

## Usage

``` r
aggregateStacksLocations(
  gobject,
  spat_units,
  values = "raw",
  summarize = "mean",
  new_spat_unit = "aggregate"
)
```

## Arguments

- gobject:

  giotto object

- spat_units:

  spatial units to aggregate

- values:

  values to use

- summarize:

  method to summarize spatial location information

- new_spat_unit:

  new name for aggregated spatial unit

## Value

giotto object

## See also

Other aggregate stacks:
[`aggregateStacks()`](https://giotto-suite.github.io/GiottoClass/dev/reference/aggregateStacks.md),
[`aggregateStacksExpression()`](https://giotto-suite.github.io/GiottoClass/dev/reference/aggregateStacksExpression.md),
[`aggregateStacksPolygonOverlaps()`](https://giotto-suite.github.io/GiottoClass/dev/reference/aggregateStacksPolygonOverlaps.md),
[`aggregateStacksPolygons()`](https://giotto-suite.github.io/GiottoClass/dev/reference/aggregateStacksPolygons.md)

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")

aggregateStacksLocations(g, spat_units = c("z0", "z1"))
```
