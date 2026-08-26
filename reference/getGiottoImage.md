# Get giotto image object

Get giotto one or more image objects from gobject

## Usage

``` r
getGiottoImage(gobject, image_type = NULL, name = NULL)
```

## Arguments

- gobject:

  giotto object

- image_type:

  deprecated

- name:

  character vector. Names giotto image object(s)
  [`showGiottoImageNames`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoImageNames.md)
  to get

## Value

a giotto image object

## See also

Other image data accessor functions:
[`setGiottoImage()`](https://giotto-suite.github.io/GiottoClass/reference/setGiottoImage.md)

Other functions to get data from giotto object:
[`getCellMetadata()`](https://giotto-suite.github.io/GiottoClass/reference/getCellMetadata.md),
[`getDimReduction()`](https://giotto-suite.github.io/GiottoClass/reference/getDimReduction.md),
[`getExpression()`](https://giotto-suite.github.io/GiottoClass/reference/getExpression.md),
[`getFeatureInfo()`](https://giotto-suite.github.io/GiottoClass/reference/getFeatureInfo.md),
[`getFeatureMetadata()`](https://giotto-suite.github.io/GiottoClass/reference/getFeatureMetadata.md),
[`getMultiomics()`](https://giotto-suite.github.io/GiottoClass/reference/getMultiomics.md),
[`getNearestNetwork()`](https://giotto-suite.github.io/GiottoClass/reference/getNearestNetwork.md),
[`getPolygonInfo()`](https://giotto-suite.github.io/GiottoClass/reference/getPolygonInfo.md),
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

getGiottoImage(gobject = g)
#> An object of class giottoLargeImage : "dapi_z0"
#> Image extent            : 6400.029, 6900.037, -5150.007, -4699.967 (xmin, xmax, ymin, ymax)
#> Original image extent   : 6400.029, 6900.037, -5150.007, -4699.967 (xmin, xmax, ymin, ymax)
#> Scale factor            : 0.108626547903541, 0.10862659908279 (x, y)
#> Resolution              : 9.2058527063567, 9.20584836903386 (x, y)
#> Layers                  : 1 
#> Name                    : mini_dataset_dapi_z0 
#> Estimated max intensity : 255 
#> Estimated min intensity : 0 
#> Values                  : integers
#> File path               : '/home/runner/work/_temp/Library/GiottoData/Mini_datasets/Vizgen/VizgenObject/Images/dapi_z0_spatRaster'
```
