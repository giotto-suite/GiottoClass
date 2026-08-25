# Get spatial enrichment

Function to get a spatial enrichment data.table

## Usage

``` r
getSpatialEnrichment(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  name = NULL,
  ...
)

# S4 method for class 'giotto'
getSpatialEnrichment(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  name = "DWLS",
  output = c("spatEnrObj", "data.table"),
  copy_obj = TRUE,
  set_defaults = TRUE
)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit (e.g. "cell")

- feat_type:

  feature type (e.g. "rna", "dna", "protein")

- name:

  name of spatial enrichment results. Default "DWLS"

- ...:

  additional params to pass

- output:

  what format in which to get information (e.g. "data.table")

- copy_obj:

  whether to deep copy/duplicate when getting the object (default =
  TRUE)

- set_defaults:

  set default spat_unit and feat_type. Change to FALSE only when
  expression and spat_info are not expected to exist.

## Value

spatEnrObj or data.table with fractions

## See also

Other spatial enrichment data accessor functions:
[`setSpatialEnrichment()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setSpatialEnrichment.md)

Other functions to get data from giotto object:
[`getCellMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getCellMetadata.md),
[`getDimReduction()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getDimReduction.md),
[`getExpression()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getExpression.md),
[`getFeatureInfo()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getFeatureInfo.md),
[`getFeatureMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getFeatureMetadata.md),
[`getGiottoImage()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getGiottoImage.md),
[`getMultiomics()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getMultiomics.md),
[`getNearestNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getNearestNetwork.md),
[`getPolygonInfo()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getPolygonInfo.md),
[`getSpatialGrid()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getSpatialGrid.md),
[`getSpatialLocations()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getSpatialLocations.md),
[`getSpatialNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getSpatialNetwork.md),
[`get_multiomics()`](https://giotto-suite.github.io/GiottoClass/dev/reference/get_multiomics.md)

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")

getSpatialEnrichment(g, spat_unit = "aggregate", name = "cluster_metagene")
```
