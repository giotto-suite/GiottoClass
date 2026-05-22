# Read feature information

Function to read lists of feature information data and output a list of
generated giottoPoints objects

## Usage

``` r
readFeatData(data_list, verbose = TRUE)
```

## Arguments

- data_list:

  (nested) list of input data to read

- verbose:

  be verbose

## Value

list of giottoPoints

## Examples

``` r
x <- GiottoData::loadSubObjectMini("giottoPoints")

readFeatData(list(x))
#> pointslist is a list without names
#> [ rna ] Process point info...
#> $rna
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
#> 
```
