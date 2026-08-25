# Utility function to convert a Giotto object to a SpatialExperiment object.

Utility function to convert a Giotto object to a SpatialExperiment
object.

## Usage

``` r
giottoToSpatialExperiment(gobject, verbose = TRUE, giottoObj = deprecated())
```

## Arguments

- gobject:

  Input Giotto object to convert to a SpatialExperiment object

- verbose:

  A boolean value specifying if progress messages should be displayed or
  not. Default `TRUE`.

- giottoObj:

  deprecated

## Value

A SpatialExperiment object that contains data from the input Giotto
object.

## Examples

``` r
if (FALSE) { # \dontrun{
mini_gobject <- GiottoData::loadGiottoMini("vizgen")
giottoToSpatialExperiment(mini_gobject)
} # }
```
