# showProcessingSteps

shows the sequential processing steps that were performed on a Giotto
object in a summarized format

## Usage

``` r
showProcessingSteps(gobject)
```

## Arguments

- gobject:

  giotto object

## Value

list of processing steps and names

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")

showProcessingSteps(g)
```
