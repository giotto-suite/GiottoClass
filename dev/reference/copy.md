# Copy an entire object

S4 generic for Giotto's S4 subobjects to return with full copies of
certain subobjects that usually return referenced information.

## Usage

``` r
# S4 method for class 'coordDataDT'
copy(x)

# S4 method for class 'giottoPoints'
copy(x)

# S4 method for class 'giottoPolygon'
copy(x)

# S4 method for class 'giottoLargeImage'
copy(x)
```

## Arguments

- x:

  a Giotto S4 class subobject

## Value

giotto subobjects

## See also

[`copy`](https://rdrr.io/pkg/data.table/man/copy.html)
[`deepcopy`](https://rspatial.github.io/terra/reference/deepcopy.html)

## Examples

``` r
g <- GiottoData::loadSubObjectMini("exprObj")

copy(g)
```
