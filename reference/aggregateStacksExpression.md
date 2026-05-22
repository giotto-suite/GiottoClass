# aggregateStacksExpression

aggregate expression matrices from different z-stacks

## Usage

``` r
aggregateStacksExpression(
  gobject,
  spat_units,
  feat_type,
  values = "raw",
  summarize = "sum",
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

- summarize:

  method to summarize expression information

- new_spat_unit:

  new name for aggregated spatial unit

- verbose:

  verbosity

## Value

giotto object

## See also

Other aggregate stacks:
[`aggregateStacks()`](https://giotto-suite.github.io/GiottoClass/reference/aggregateStacks.md),
[`aggregateStacksLocations()`](https://giotto-suite.github.io/GiottoClass/reference/aggregateStacksLocations.md),
[`aggregateStacksPolygonOverlaps()`](https://giotto-suite.github.io/GiottoClass/reference/aggregateStacksPolygonOverlaps.md),
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

aggregateStacksExpression(g, spat_units = c("z0", "z1"), feat_type = "rna")
#> > raw already exists and will be replaced with new values
#> > Cell metadata for spat_unit " aggregate " and feat_type " rna " already
#>  exists and will be replaced with new metadata.
#> Setting cell metadata [aggregate][rna]
#> > Feat metadata for spat_unit " aggregate " and feat_type " rna " already
#>  exists and will be replaced with new metadata.
#> Setting feature metadata [aggregate][rna]
#> An object of class giotto 
#> >Active spat_unit:  z0 
#> >Active feat_type:  rna 
#> dimensions    : 337, 498 (features, cells)
#> [SUBCELLULAR INFO]
#> polygons      : z0 z1 aggregate 
#> features      : rna 
#> [AGGREGATE INFO]
#> expression -----------------------
#>   [z0][rna] raw
#>   [z1][rna] raw
#>   [aggregate][rna] raw normalized scaled pearson
#> spatial locations ----------------
#>   [z0] raw
#>   [z1] raw
#>   [aggregate] raw
#> spatial networks -----------------
#>   [aggregate] Delaunay_network kNN_network
#> spatial enrichments --------------
#>   [aggregate][rna] cluster_metagene
#> dim reduction --------------------
#>   [aggregate][rna] pca umap tsne
#> nearest neighbor networks --------
#>   [aggregate][rna] sNN.pca
#> attached images ------------------
#> images      : 4 items...
#> 
#> 
#> Use objHistory() to see steps and params used
```
