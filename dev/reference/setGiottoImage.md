# Set giotto image object

Directly attach a giotto image to giotto object

## Usage

``` r
setGiottoImage(gobject, x, name = NULL, ...)

# S4 method for class 'giotto'
setGiottoImage(
  gobject,
  x,
  name = NULL,
  initialize = FALSE,
  verbose = NULL,
  ...,
  image = deprecated()
)
```

## Arguments

- gobject:

  giotto object

- x:

  giotto image object to be attached without modification to the giotto
  object

- name:

  name of giotto image object

- ...:

  additional params to pass

- initialize:

  (default = FALSE) whether to initialize the gobject before returning

- verbose:

  be verbose

- image:

  deprecated. Use `x`

## Value

giotto object

## Details

***Use with care!*** This function directly attaches giotto image
objects to the gobject without further modifications of spatial
positioning values within the image object that are generally needed in
order for them to plot in the correct location relative to the other
modalities of spatial data.  
For the more general-purpose method of attaching image objects, see
[`addGiottoImage`](https://giotto-suite.github.io/GiottoClass/dev/reference/addGiottoImage.md)

## See also

[`addGiottoImage`](https://giotto-suite.github.io/GiottoClass/dev/reference/addGiottoImage.md)

Other image data accessor functions:
[`getGiottoImage()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getGiottoImage.md)

Other functions to set data in giotto object:
[`setCellMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setCellMetadata.md),
[`setDimReduction()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setDimReduction.md),
[`setExpression()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setExpression.md),
[`setFeatureInfo()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setFeatureInfo.md),
[`setFeatureMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setFeatureMetadata.md),
[`setGiotto,giotto,giottoBinPoints-method`](https://giotto-suite.github.io/GiottoClass/dev/reference/setGiotto.md),
[`setMultiomics()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setMultiomics.md),
[`setNearestNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setNearestNetwork.md),
[`setPolygonInfo()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setPolygonInfo.md),
[`setSpatialEnrichment()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setSpatialEnrichment.md),
[`setSpatialGrid()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setSpatialGrid.md),
[`setSpatialLocations()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setSpatialLocations.md),
[`setSpatialNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setSpatialNetwork.md),
[`set_multiomics()`](https://giotto-suite.github.io/GiottoClass/dev/reference/set_multiomics.md)

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")
gimg <- getGiottoImage(gobject = g)

setGiottoImage(g, NULL, name = objName(gimg))
setGiottoImage(gobject = g, x = gimg)
```
