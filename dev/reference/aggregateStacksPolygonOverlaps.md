# aggregateStacksPolygonOverlaps

aggregate polygons overlap information from different z-stacks

## Usage

``` r
aggregateStacksPolygonOverlaps(
  gobject,
  spat_units,
  feat_type,
  new_spat_unit = "aggregate"
)
```

## Arguments

- gobject:

  giotto object

- spat_units:

  spatial units to aggregate

- feat_type:

  feature type used for overlap calculations

- new_spat_unit:

  new name for aggregated spatial unit

## Value

giotto object

## See also

Other aggregate stacks:
[`aggregateStacks()`](https://giotto-suite.github.io/GiottoClass/dev/reference/aggregateStacks.md),
[`aggregateStacksExpression()`](https://giotto-suite.github.io/GiottoClass/dev/reference/aggregateStacksExpression.md),
[`aggregateStacksLocations()`](https://giotto-suite.github.io/GiottoClass/dev/reference/aggregateStacksLocations.md),
[`aggregateStacksPolygons()`](https://giotto-suite.github.io/GiottoClass/dev/reference/aggregateStacksPolygons.md)

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")

aggregateStacksPolygonOverlaps(g,
    spat_units = c("z0", "z1"),
    feat_type = "rna"
)
```
