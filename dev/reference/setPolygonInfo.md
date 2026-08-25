# Set polygon info

Set polygon information into Giotto object

## Usage

``` r
setPolygonInfo(gobject, x, name = NULL, ...)

# S4 method for class 'giotto'
setPolygonInfo(
  gobject,
  x,
  name = NULL,
  centroids_to_spatlocs = FALSE,
  verbose = NULL,
  initialize = TRUE,
  ...
)
```

## Arguments

- gobject:

  giotto object

- x:

  single object or named list of objects to set as polygon information
  (see details)

- name:

  (optional, character) name to assign to polygon and spatial unit that
  polygon might define. Only used for single giottoPolygon objects.
  Names are taken from a named list for multiple polygons. NULL
  (default) takes the name from `x`, falling back to "cell" when `x` is
  unnamed

- ...:

  additional params to pass

- centroids_to_spatlocs:

  if centroid information is discovered, whether to additionally set
  them as a set of spatial locations (default = FALSE)

- verbose:

  be verbose

- initialize:

  (default = FALSE) whether to initialize the gobject before returning

## Value

giotto object

## Details

Inputs can be provided as either single objects or named lists of
objects. If the list is not named, then a generic name of the template
'cell_i' will be applied.  
If an input is a character string, then it is assumed that it is a
filepath.  
For required formatting when reading tabular data or objects, see
[`createGiottoPolygonsFromDfr`](https://giotto-suite.github.io/GiottoClass/dev/reference/createGiottoPolygon.md)
details.

## See also

Other polygon info data accessor functions:
[`getPolygonInfo()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getPolygonInfo.md)

Other functions to set data in giotto object:
[`setCellMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setCellMetadata.md),
[`setDimReduction()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setDimReduction.md),
[`setExpression()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setExpression.md),
[`setFeatureInfo()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setFeatureInfo.md),
[`setFeatureMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setFeatureMetadata.md),
[`setGiotto,giotto,giottoBinPoints-method`](https://giotto-suite.github.io/GiottoClass/dev/reference/setGiotto.md),
[`setGiottoImage()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setGiottoImage.md),
[`setMultiomics()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setMultiomics.md),
[`setNearestNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setNearestNetwork.md),
[`setSpatialEnrichment()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setSpatialEnrichment.md),
[`setSpatialGrid()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setSpatialGrid.md),
[`setSpatialLocations()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setSpatialLocations.md),
[`setSpatialNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setSpatialNetwork.md),
[`set_multiomics()`](https://giotto-suite.github.io/GiottoClass/dev/reference/set_multiomics.md)

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")
polyinfo <- getPolygonInfo(g, return_giottoPolygon = TRUE)

setPolygonInfo(gobject = g, x = polyinfo)
```
