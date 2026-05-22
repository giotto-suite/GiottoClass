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
#> $rna
#> class       : SpatVector
#> geometry    : points
#> dimensions  : 32071, 4  (geometries, attributes)
#> extent      : 6400.441, 6900.032, -5149.827, -4699.984  (xmin, xmax, ymin, ymax)
#> coord. ref. : 
#> names       :                   poly_ID feat_ID feat_ID_uniq stack
#> type        :                     <chr>   <chr>        <int> <int>
#> values      : 323754550002953984063006~  Selplg           87     1
#>               323754550002953984063006~   Fgfr3          122     1
#>               323754550002953984063006~    Gfap          138     1
#>               ...
#> 
```
