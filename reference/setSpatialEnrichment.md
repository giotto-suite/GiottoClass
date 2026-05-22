# Set spatial enrichment

Function to set a spatial enrichment slot

## Usage

``` r
setSpatialEnrichment(
  gobject,
  x,
  spat_unit = NULL,
  feat_type = NULL,
  name = "enrichment",
  provenance = NULL,
  verbose = TRUE,
  initialize = TRUE,
  ...
)
```

## Arguments

- gobject:

  giotto object

- x:

  spatEnrObj or list of spatEnrObj to set. Passing NULL will remove a
  specified set of spatial enrichment information from the gobject.

- spat_unit:

  spatial unit (e.g. "cell")

- feat_type:

  feature type (e.g. "rna", "dna", "protein")

- name:

  name of spatial enrichment results. Default "DWLS"

- provenance:

  provenance information (optional)

- verbose:

  be verbose

- initialize:

  (default = FALSE) whether to initialize the gobject before returning

- ...:

  additional params to pass

## Value

giotto object

## See also

Other spatial enrichment data accessor functions:
[`getSpatialEnrichment()`](https://giotto-suite.github.io/GiottoClass/reference/getSpatialEnrichment.md)

Other functions to set data in giotto object:
[`setCellMetadata()`](https://giotto-suite.github.io/GiottoClass/reference/setCellMetadata.md),
[`setDimReduction()`](https://giotto-suite.github.io/GiottoClass/reference/setDimReduction.md),
[`setExpression()`](https://giotto-suite.github.io/GiottoClass/reference/setExpression.md),
[`setFeatureInfo()`](https://giotto-suite.github.io/GiottoClass/reference/setFeatureInfo.md),
[`setFeatureMetadata()`](https://giotto-suite.github.io/GiottoClass/reference/setFeatureMetadata.md),
[`setGiotto,giotto,giottoBinPoints-method`](https://giotto-suite.github.io/GiottoClass/reference/setGiotto.md),
[`setGiottoImage()`](https://giotto-suite.github.io/GiottoClass/reference/setGiottoImage.md),
[`setMultiomics()`](https://giotto-suite.github.io/GiottoClass/reference/setMultiomics.md),
[`setNearestNetwork()`](https://giotto-suite.github.io/GiottoClass/reference/setNearestNetwork.md),
[`setPolygonInfo()`](https://giotto-suite.github.io/GiottoClass/reference/setPolygonInfo.md),
[`setSpatialGrid()`](https://giotto-suite.github.io/GiottoClass/reference/setSpatialGrid.md),
[`setSpatialLocations()`](https://giotto-suite.github.io/GiottoClass/reference/setSpatialLocations.md),
[`setSpatialNetwork()`](https://giotto-suite.github.io/GiottoClass/reference/setSpatialNetwork.md),
[`set_multiomics()`](https://giotto-suite.github.io/GiottoClass/reference/set_multiomics.md)

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
spatenrich <- GiottoData::loadSubObjectMini("spatEnrObj")

g <- setSpatialEnrichment(g, spatenrich)
#> > " cluster_metagene " already exists and will be replaced with new spatial
#>  enrichment results
#> Setting spatial enrichment [aggregate][rna] cluster_metagene
```
