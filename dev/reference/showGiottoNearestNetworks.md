# showGiottoNearestNetworks

shows the available nearest neighbor networks

## Usage

``` r
showGiottoNearestNetworks(gobject, nrows = 3)
```

## Arguments

- gobject:

  giotto object

- nrows:

  number of network rows to print

## Value

prints the name and small subset of available nearest neighbor network
info

## See also

Other functions to show data in giotto object:
[`showGiottoCellMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoCellMetadata.md),
[`showGiottoDimRed()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoDimRed.md),
[`showGiottoExpression()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoExpression.md),
[`showGiottoFeatInfo()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoFeatInfo.md),
[`showGiottoFeatMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoFeatMetadata.md),
[`showGiottoImageNames()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoImageNames.md),
[`showGiottoSpatEnrichments()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoSpatEnrichments.md),
[`showGiottoSpatGrids()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoSpatGrids.md),
[`showGiottoSpatLocs()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoSpatLocs.md),
[`showGiottoSpatNetworks()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoSpatNetworks.md),
[`showGiottoSpatialInfo()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoSpatialInfo.md)

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")

showGiottoNearestNetworks(g)
```
