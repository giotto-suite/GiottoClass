# Subset part of an object with `$`

Subset values from a Giotto Class using `$` operator.

## Usage

``` r
# S4 method for class 'giotto'
x$name

# S4 method for class 'coordDataDT'
x$name

# S4 method for class 'spatEnrObj'
x$name

# S4 method for class 'dimObj'
x$name

# S4 method for class 'metaData'
x$name

# S4 method for class 'terraVectData'
x$name

# S4 method for class 'affine2d'
x$name

# S4 method for class 'processParam'
x$name

# S4 method for class 'analyzeParam'
x$name

# S4 method for class 'filterParam'
x$name

# S4 method for class 'reduceParam'
x$name
```

## Arguments

- x:

  Giotto S4 object to extract columns from

- name:

  A literal character string (possibly backtick quoted). This is
  normally matched to the colnames.

## Value

vector of values from a requested column

## Functions

- `$`: Subset giotto object

## `` `$` `` methods

Select by colname from giotto S4 data.table coordinates slot.

Select by colname from giotto S4 spatEnrObj

Select entries in misc slot from giotto S4 dimObj.

Select by colname from giotto S4 data.table metaDT slot.

Select by colname from giotto S4 spatVector slot.

Select piecewise transform values from `affine2d`

Select param from `processParam` inheriting objects

Select param from `analyzeParam` inheriting objects

Select param from `filterParam` inheriting objects

Select param from `reduceParam` inheriting objects

## See also

[subset_bracket](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
[replace_bracket](https://giotto-suite.github.io/GiottoClass/dev/reference/replace_bracket.md)
[replace_dollar](https://giotto-suite.github.io/GiottoClass/dev/reference/replace_dollar.md)

## Examples

``` r
enr <- GiottoData::loadSubObjectMini("spatEnrObj")

enr$cell_ID
```
