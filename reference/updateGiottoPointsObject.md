# Update giotto points object

Update giotto points object

## Usage

``` r
updateGiottoPointsObject(gpoints)
```

## Arguments

- gpoints:

  giotto points object

## Value

GiottoPointsObject

## Examples

``` r
g <- GiottoData::loadSubObjectMini("giottoPoints")

updateGiottoPointsObject(g)
#> An object of class giottoPoints
#> feat_type : "rna"
#> Feature Information:
#> class       : SpatVector
#> geometry    : points
#> dimensions  : 79761, 3  (geometries, attributes)
#> extent      : 6400.037, 6900.032, -5149.983, -4699.979  (xmin, xmax, ymin, ymax)
#> coord. ref. : 
#> names       : feat_ID global_z feat_ID_uniq
#> type        :   <chr>    <int>        <int>
#> values      :    Mlc1        0            1
#>                Gprc5b        0            2
#>                  Gfap        0            3
#>               ...
#> 
```
