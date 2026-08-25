# showGiottoSpatEnrichments

shows the available spatial enrichment results

## Usage

``` r
showGiottoSpatEnrichments(gobject, nrows = 4)
```

## Arguments

- gobject:

  giotto object

- nrows:

  number of rows to print for each spatial enrichment data.table

## Value

prints the name and small subset of available data.table

## See also

Other functions to show data in giotto object:
[`showGiottoCellMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoCellMetadata.md),
[`showGiottoDimRed()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoDimRed.md),
[`showGiottoExpression()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoExpression.md),
[`showGiottoFeatInfo()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoFeatInfo.md),
[`showGiottoFeatMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoFeatMetadata.md),
[`showGiottoImageNames()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoImageNames.md),
[`showGiottoNearestNetworks()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoNearestNetworks.md),
[`showGiottoSpatGrids()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoSpatGrids.md),
[`showGiottoSpatLocs()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoSpatLocs.md),
[`showGiottoSpatNetworks()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoSpatNetworks.md),
[`showGiottoSpatialInfo()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoSpatialInfo.md)

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")

showGiottoSpatEnrichments(g)
```
