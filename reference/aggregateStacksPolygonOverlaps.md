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
[`aggregateStacks()`](https://giotto-suite.github.io/GiottoClass/reference/aggregateStacks.md),
[`aggregateStacksExpression()`](https://giotto-suite.github.io/GiottoClass/reference/aggregateStacksExpression.md),
[`aggregateStacksLocations()`](https://giotto-suite.github.io/GiottoClass/reference/aggregateStacksLocations.md),
[`aggregateStacksPolygons()`](https://giotto-suite.github.io/GiottoClass/reference/aggregateStacksPolygons.md)

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : 'giotto_env'
#>  python version : 3.10

aggregateStacksPolygonOverlaps(g,
    spat_units = c("z0", "z1"),
    feat_type = "rna"
)
#> Error in rbind2(...): argument "x" is missing, with no default
```
