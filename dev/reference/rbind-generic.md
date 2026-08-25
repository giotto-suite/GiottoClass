# Combine objects by rows (Giotto-related)

row bind two objects

## Usage

``` r
# S4 method for class 'giottoBinPoints,giottoBinPoints'
rbind2(x, y, ...)

# S4 method for class 'cellMetaObj,cellMetaObj'
rbind2(x, y, ...)

# S4 method for class 'featMetaObj,featMetaObj'
rbind2(x, y, ...)

# S4 method for class 'spatLocsObj,spatLocsObj'
rbind2(x, y, ...)

# S4 method for class 'giottoPolygon,giottoPolygon'
rbind2(x, y, add_list_ID = TRUE, ...)

# S4 method for class 'giottoPoints,giottoPoints'
rbind2(x, y, ...)

# S4 method for class 'overlapPointDT,overlapPointDT'
rbind2(x, y, ...)
```

## Arguments

- x:

  item 1 to rbind

- y:

  item 2 to rbind

- ...:

  additional params to pass to methods

- add_list_ID:

  whether to generate a list_ID column when giottoPolygons to append
  have different names

## Value

object with appended rows

## Functions

- `rbind2(x = giottoPolygon, y = giottoPolygon)`: Append giottoPolygon
  objects

- `rbind2(x = giottoPoints, y = giottoPoints)`: Append giottoPoints
  objects

## Examples

``` r
g <- GiottoData::loadSubObjectMini("giottoPolygon")

rbind2(g, g)
```
