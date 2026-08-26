# Get polygon info

Get giotto polygon spatVector

## Usage

``` r
getPolygonInfo(
  gobject = NULL,
  polygon_name = NULL,
  polygon_overlap = NULL,
  return_giottoPolygon = FALSE,
  verbose = TRUE,
  simplify = TRUE
)
```

## Arguments

- gobject:

  giotto object

- polygon_name:

  name of polygons. Default is "cell"

- polygon_overlap:

  include polygon overlap information

- return_giottoPolygon:

  (Defaults to FALSE) Return as giottoPolygon S4 object

- verbose:

  be verbose

- simplify:

  logical. Whether or not to take object out of a list when there is a
  length of 1.

## Value

spatVector

## See also

Other polygon info data accessor functions:
[`setPolygonInfo()`](https://giotto-suite.github.io/GiottoClass/reference/setPolygonInfo.md)

Other functions to get data from giotto object:
[`getCellMetadata()`](https://giotto-suite.github.io/GiottoClass/reference/getCellMetadata.md),
[`getDimReduction()`](https://giotto-suite.github.io/GiottoClass/reference/getDimReduction.md),
[`getExpression()`](https://giotto-suite.github.io/GiottoClass/reference/getExpression.md),
[`getFeatureInfo()`](https://giotto-suite.github.io/GiottoClass/reference/getFeatureInfo.md),
[`getFeatureMetadata()`](https://giotto-suite.github.io/GiottoClass/reference/getFeatureMetadata.md),
[`getGiottoImage()`](https://giotto-suite.github.io/GiottoClass/reference/getGiottoImage.md),
[`getMultiomics()`](https://giotto-suite.github.io/GiottoClass/reference/getMultiomics.md),
[`getNearestNetwork()`](https://giotto-suite.github.io/GiottoClass/reference/getNearestNetwork.md),
[`getSpatialEnrichment()`](https://giotto-suite.github.io/GiottoClass/reference/getSpatialEnrichment.md),
[`getSpatialGrid()`](https://giotto-suite.github.io/GiottoClass/reference/getSpatialGrid.md),
[`getSpatialLocations()`](https://giotto-suite.github.io/GiottoClass/reference/getSpatialLocations.md),
[`getSpatialNetwork()`](https://giotto-suite.github.io/GiottoClass/reference/getSpatialNetwork.md),
[`get_multiomics()`](https://giotto-suite.github.io/GiottoClass/reference/get_multiomics.md)

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : '/usr/bin/python3'
#>  python version : 3.12

getPolygonInfo(g)
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 498, 1  (geometries, attributes)
#> extent      : 6399.244, 6903.243, -5152.39, -4694.868  (xmin, xmax, ymin, ymax)
#> source      : z0_spatInfo_spatVector.shp
#> coord. ref. : 
#> names       :                                 poly_ID
#> type        :                                   <chr>
#> values      :  40951783403982682273285375368232495429
#>               240649020551054330404932383065726870513
#>               274176126496863898679934791272921588227
#>               ...
```
