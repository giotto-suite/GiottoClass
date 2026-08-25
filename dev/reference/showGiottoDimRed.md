# showGiottoDimRed

shows the available dimension reductions

## Usage

``` r
showGiottoDimRed(gobject, nrows = 3, ncols = 2)
```

## Arguments

- gobject:

  giotto object

- nrows:

  number of coordinates rows to print

- ncols:

  number of coordinates columns to print

## Value

prints the name and small subset of available dimension reduction
coordinates

## See also

Other functions to show data in giotto object:
[`showGiottoCellMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoCellMetadata.md),
[`showGiottoExpression()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoExpression.md),
[`showGiottoFeatInfo()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoFeatInfo.md),
[`showGiottoFeatMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoFeatMetadata.md),
[`showGiottoImageNames()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoImageNames.md),
[`showGiottoNearestNetworks()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoNearestNetworks.md),
[`showGiottoSpatEnrichments()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoSpatEnrichments.md),
[`showGiottoSpatGrids()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoSpatGrids.md),
[`showGiottoSpatLocs()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoSpatLocs.md),
[`showGiottoSpatNetworks()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoSpatNetworks.md),
[`showGiottoSpatialInfo()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoSpatialInfo.md)

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")

showGiottoDimRed(g)
```
