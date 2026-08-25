# Row and column names

Retrieve or set the row or column names of an object

## Usage

``` r
# S4 method for class 'giotto'
colnames(x)

# S4 method for class 'exprObj'
colnames(x)

# S4 method for class 'cellMetaObj'
colnames(x)

# S4 method for class 'featMetaObj'
colnames(x)

# S4 method for class 'spatEnrObj'
colnames(x)

# S4 method for class 'spatLocsObj'
colnames(x)

# S4 method for class 'dimObj'
colnames(x)

# S4 method for class 'giotto'
rownames(x)

# S4 method for class 'exprObj'
rownames(x)

# S4 method for class 'dimObj'
rownames(x)

# S4 method for class 'metaData'
rownames(x)
```

## Arguments

- x:

  object

## Value

A character vector of row or col names

## Examples

``` r
g <- GiottoData::loadSubObjectMini("exprObj")

colnames(g)
```
