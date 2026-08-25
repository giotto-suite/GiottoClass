# Basic class for additional miscellaneous information

Classes (such as dimObj) that can hold information from multiple types
of methods use the misc slot to hold additional information specific to
each method. Information may be stored within as S3 structures.

## Value

slot for miscellaneous information

## Examples

``` r
g <- GiottoData::loadSubObjectMini("dimObj")

slot(g, "misc")
```
