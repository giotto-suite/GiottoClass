# Replace part of an object with `$<-`

Replace values from Giotto Classes using `$<-` operator.

## Usage

``` r
# S4 method for class 'giotto'
x$name <- value

# S4 method for class 'coordDataDT'
x$name <- value

# S4 method for class 'spatEnrObj'
x$name <- value

# S4 method for class 'dimObj'
x$name <- value

# S4 method for class 'metaData'
x$name <- value

# S4 method for class 'terraVectData'
x$name <- value

# S4 method for class 'processParam'
x$name <- value

# S4 method for class 'analyzeParam'
x$name <- value

# S4 method for class 'filterParam'
x$name <- value

# S4 method for class 'reduceParam'
x$name <- value
```

## Arguments

- x:

  Giotto S4 object to replace columns from

- name:

  A literal character string (possibly backtick quoted). This is
  normally matched to the colnames.

- value:

  values(s) to set to a column

## Value

same as `x`

## `` `$<-` `` methods

Set values by colname into giotto S4 data.table coordinates slot. Works
via data.table methods

Set values by colname into giotto S4 spatEnrObj.

Set entries in misc slot from giotto S4 dimObj.

Set values by colname into giotto S4 data.table metaDT slot. Works via
data.table methods

Set values by colname into giotto S4 spatVector slot.

Set values by param name into `processParam` inheriting objects

Set values by param name into `analyzeParam` inheriting objects

Set values by param name into `filterParam` inheriting objects

Set values by param name into `reduceParam` inheriting objects

## See also

[subset_bracket](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_bracket.md)
[replace_bracket](https://giotto-suite.github.io/GiottoClass/dev/reference/replace_bracket.md)
[subset_dollar](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_dollar.md)

## Examples

``` r
gpoints <- GiottoData::loadSubObjectMini("giottoPoints")

gpoints$new_col <- sprintf("feat_%d", seq(nrow(gpoints)))
```
