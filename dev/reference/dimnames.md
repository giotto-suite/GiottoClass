# Dimnames of an object

Retrieve or set the dimnames of an object

## Usage

``` r
# S4 method for class 'giotto'
dimnames(x)

# S4 method for class 'exprObj'
dimnames(x)

# S4 method for class 'dimObj'
dimnames(x)

# S4 method for class 'spatLocsObj'
dimnames(x)

# S4 method for class 'metaData'
dimnames(x)

# S4 method for class 'enrData'
dimnames(x)

# S4 method for class 'dimObj'
dimnames(x)
```

## Arguments

- x:

  object

## Value

character

## Examples

``` r
g <- GiottoData::loadSubObjectMini("exprObj")

dimnames(g)
```
