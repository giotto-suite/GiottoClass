# Replace part of an object with `[<-`

Replace values from Giotto Classes. Providing empty brackets such as
`x[] <- value` will usually replace the entire contained data
representation.

## Usage

``` r
# S4 method for class 'coordDataDT,missing,missing,ANY'
x[i, j] <- value

# S4 method for class 'metaData,missing,missing,ANY'
x[i, j] <- value

# S4 method for class 'dimObj,missing,missing,ANY'
x[i, j] <- value

# S4 method for class 'exprData,missing,missing,ANY'
x[i, j] <- value

# S4 method for class 'spatNetData,missing,missing,ANY'
x[i, j] <- value

# S4 method for class 'nnData,missing,missing,ANY'
x[i, j] <- value

# S4 method for class 'enrData,missing,missing,ANY'
x[i, j] <- value

# S4 method for class 'spatGridData,missing,missing,ANY'
x[i, j] <- value

# S4 method for class 'giottoPoints,missing,missing,ANY'
x[i, j] <- value

# S4 method for class 'giottoPolygon,missing,missing,ANY'
x[i, j] <- value

# S4 method for class 'giottoLargeImage,missing,missing,ANY'
x[i, j] <- value

# S4 method for class 'giottoImage,missing,missing,ANY'
x[i, j] <- value

# S4 method for class 'affine2d,missing,missing,ANY'
x[i, j] <- value

# S4 method for class 'processParam,missing,missing,list'
x[i, j] <- value
```

## Arguments

- x:

  Giotto S4 object to replace information in

- i, j:

  indices specifying elements to replace. Indices are numeric or
  character vectors or empty

- value:

  values(s) to set

## Value

same as `x`

## `` `[<-` `` methods

Assign to `coordinates` slot in giotto S4

Assign to `metaDT` slot in giotto S4

Assign to `coordinates` slot in giotto S4 dimObj

Assign to `exprMat` slot in giotto S4

Assign to `network` slot in giotto S4

Assign to `network` slot in giotto S4

Assign to `enrichDT` slot in giotto S4

Assign to `gridDT` slot in giotto S4

Assign to `spatVector` slot in giotto S4

Assign to `spatVector` slot in giottoPolygon

Assign to `affine` slot in affine2d

## See also

[subset_bracket](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
[subset_dollar](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_dollar.md)
[replace_dollar](https://giotto-suite.github.io/GiottoClass/dev/reference/replace_dollar.md)

## Examples

``` r
gpoints <- GiottoData::loadSubObjectMini("giottoPoints")

gpoints[] <- gpoints[]
```
