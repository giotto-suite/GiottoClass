# showGiottoSpatialInfo

show the available giotto spatial polygon information

## Usage

``` r
showGiottoSpatialInfo(gobject)
```

## Arguments

- gobject:

  giotto object

## Value

SpatVector

## See also

Other functions to show data in giotto object:
[`showGiottoCellMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoCellMetadata.md),
[`showGiottoDimRed()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoDimRed.md),
[`showGiottoExpression()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoExpression.md),
[`showGiottoFeatInfo()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoFeatInfo.md),
[`showGiottoFeatMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoFeatMetadata.md),
[`showGiottoImageNames()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoImageNames.md),
[`showGiottoNearestNetworks()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoNearestNetworks.md),
[`showGiottoSpatEnrichments()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoSpatEnrichments.md),
[`showGiottoSpatGrids()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoSpatGrids.md),
[`showGiottoSpatLocs()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoSpatLocs.md),
[`showGiottoSpatNetworks()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoSpatNetworks.md)

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")

showGiottoSpatialInfo(g)
```
