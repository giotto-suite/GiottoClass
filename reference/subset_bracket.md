# Subset part of an object with `[` or `[[`

Extract values from Giotto classes. Providing empty brackets such as:
`x[]` will usually extract the main contained data representation.

## Usage

``` r
# S4 method for class 'giottoBinPoints,logical,missing,missing'
x[i, j, compact = "auto", ..., drop]

# S4 method for class 'gdtData,gIndex,gIndex,missing'
x[i, j]

# S4 method for class 'gdtData,logical,missing,missing'
x[i, j]

# S4 method for class 'gdtData,character,missing,missing'
x[i, j]

# S4 method for class 'gdtData,missing,numeric,missing'
x[i, j]

# S4 method for class 'gdtData,missing,logical,missing'
x[i, j]

# S4 method for class 'coordDataDT,ANY,ANY,missing'
x[i, j]

# S4 method for class 'coordDataDT,missing,ANY,missing'
x[i, j]

# S4 method for class 'coordDataDT,missing,character,missing'
x[i, j]

# S4 method for class 'coordDataDT,ANY,missing,missing'
x[i, j]

# S4 method for class 'coordDataDT,missing,missing,missing'
x[i, j]

# S4 method for class 'giottoPoints,gIndex,missing,missing'
x[i, j]

# S4 method for class 'metaData,missing,ANY,missing'
x[i, j]

# S4 method for class 'metaData,missing,character,missing'
x[i, j]

# S4 method for class 'metaData,ANY,missing,missing'
x[i, j]

# S4 method for class 'metaData,missing,missing,missing'
x[i, j]

# S4 method for class 'dimObj,ANY,ANY,missing'
x[i, j]

# S4 method for class 'dimObj,missing,missing,missing'
x[i, j]

# S4 method for class 'exprData,missing,ANY,missing'
x[i, j]

# S4 method for class 'exprData,ANY,missing,missing'
x[i, j]

# S4 method for class 'exprData,ANY,ANY,missing'
x[i, j]

# S4 method for class 'exprData,missing,missing,missing'
x[i, j]

# S4 method for class 'spatNetData,missing,missing,missing'
x[i, j]

# S4 method for class 'nnData,missing,missing,missing'
x[i, j]

# S4 method for class 'enrData,missing,missing,missing'
x[i, j]

# S4 method for class 'enrData,ANY,missing,missing'
x[i, j]

# S4 method for class 'enrData,missing,ANY,missing'
x[i, j]

# S4 method for class 'enrData,missing,character,missing'
x[i, j]

# S4 method for class 'spatGridData,missing,missing,missing'
x[i, j]

# S4 method for class 'giottoPoints,missing,missing,missing'
x[i, j]

# S4 method for class 'giottoPoints,gIndex,missing,missing'
x[i, j]

# S4 method for class 'giottoPoints,character,missing,missing'
x[i, j]

# S4 method for class 'giottoPoints,missing,gIndex,missing'
x[i, j]

# S4 method for class 'giottoPolygon,missing,missing,missing'
x[i, j]

# S4 method for class 'giottoPolygon,gIndex,missing,missing'
x[i, j]

# S4 method for class 'giottoPolygon,character,missing,missing'
x[i, j]

# S4 method for class 'giottoPolygon,missing,gIndex,missing'
x[i, j]

# S4 method for class 'terraVectData,gIndex,gIndex,missing'
x[i, j]

# S4 method for class 'giottoLargeImage,missing,missing,missing'
x[i, j]

# S4 method for class 'giottoLargeImage,gIndex,missing'
x[[i, j, ...]]

# S4 method for class 'giottoImage,missing,missing,missing'
x[i, j]

# S4 method for class 'affine2d,missing,missing,missing'
x[i, j]

# S4 method for class 'processParam,missing,missing,missing'
x[i, j]
```

## Arguments

- x:

  Giotto S4 object to subset information from

- i, j:

  indices specifying elements to extract. Indices are numeric or
  character vectors, or empty

- compact:

  `character` or `logical` (default = "auto"). Whether to compact
  object. See
  [giottoBinPoints](https://giotto-suite.github.io/GiottoClass/reference/giottoBinPoints-class.md).
  `"auto"` will perform a compaction when number of spatial points
  referenced in `@counts` is 1/10 of that existing in `@spatial`

- ...:

  additional arguments

## Value

Same as `x` unless brackets are empty in which case, the main internal
representation is returned.

## `` `[` `` methods

Return `coordinates` slot data.table from giotto S4

Select rows (i) and cols (j) from giotto S4 metaDT slot

Return `metaDT` slot data.table from giotto S4

Return `coordinates` slot matrix from giotto S4 dimObj

Select rows (i) and cols (j) from giotto S4 exprMat slot

Return `exprMat` slot Matrix object from giotto S4

Return `spatNetData` slot network data.table object from giotto S4

Return `nnData` slot igraph object from giotto S4

Return `enrData` slot enrichment data.table object from giotto S4

Return `spatGridData` slot data.table object from giotto S4

Return `giottoPoints` spatVector slot

Return `giottoPolygon` spatVector slot

## See also

[replace_bracket](https://giotto-suite.github.io/GiottoClass/reference/replace_bracket.md)
[subset_dollar](https://giotto-suite.github.io/GiottoClass/reference/subset_dollar.md)
[replace_dollar](https://giotto-suite.github.io/GiottoClass/reference/replace_dollar.md)

## Examples

``` r
gpoints <- GiottoData::loadSubObjectMini("giottoPoints")

# extract contained `SpatVector`
gpoints[]
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

# subset by feature
gpoints[c("Mlc1", "Gfap")]
#> An object of class giottoPoints
#> feat_type : "rna"
#> Feature Information:
#> class       : SpatVector
#> geometry    : points
#> dimensions  : 5464, 3  (geometries, attributes)
#> extent      : 6400.037, 6899.441, -5149.827, -4700.029  (xmin, xmax, ymin, ymax)
#> coord. ref. : 
#> names       : feat_ID global_z feat_ID_uniq
#> type        :   <chr>    <int>        <int>
#> values      :    Mlc1        0            1
#>                  Gfap        0            3
#>                  Gfap        0            4
#>               ...
#> 

# subset by feature and colname
gpoints["Mlc1", c("feat_ID", "feat_ID_uniq")]
#> An object of class giottoPoints
#> feat_type : "rna"
#> Feature Information:
#> class       : SpatVector
#> geometry    : points
#> dimensions  : 488, 2  (geometries, attributes)
#> extent      : 6400.037, 6899.308, -5149.827, -4703.377  (xmin, xmax, ymin, ymax)
#> coord. ref. : 
#> names       : feat_ID feat_ID_uniq
#> type        :   <chr>        <int>
#> values      :    Mlc1            1
#>                  Mlc1           55
#>                  Mlc1          127
#>               ...
#> 

# subset by index
gpoints[seq(20)]
#> An object of class giottoPoints
#> feat_type : "rna"
#> Feature Information:
#> class       : SpatVector
#> geometry    : points
#> dimensions  : 20, 3  (geometries, attributes)
#> extent      : 6400.037, 6400.745, -5096.167, -4770.126  (xmin, xmax, ymin, ymax)
#> coord. ref. : 
#> names       : feat_ID global_z feat_ID_uniq
#> type        :   <chr>    <int>        <int>
#> values      :    Mlc1        0            1
#>                Gprc5b        0            2
#>                  Gfap        0            3
#>               ...
#> 
```
