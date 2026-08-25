# overlaps-generic

Access list of overlaps information from object

## Usage

``` r
# S4 method for class 'giottoPolygon'
overlaps(x, name = NULL)
```

## Arguments

- x:

  object

- name:

  (optional) name of overlaps information to retrieve

## Value

list of overlaps from object

## Functions

- `overlaps(giottoPolygon)`: Get overlaps information from giottoPolygon

## Examples

``` r
g <- GiottoData::loadSubObjectMini("giottoPolygon")

overlaps(g)
```
