# Dimensions of giotto objects

Find the dimensions of an object

## Usage

``` r
# S4 method for class 'giotto'
nrow(x)

# S4 method for class 'giottoPoints'
nrow(x)

# S4 method for class 'giottoPolygon'
nrow(x)

# S4 method for class 'spatLocsObj'
nrow(x)

# S4 method for class 'exprData'
nrow(x)

# S4 method for class 'metaData'
nrow(x)

# S4 method for class 'spatialNetworkObj'
nrow(x)

# S4 method for class 'enrData'
nrow(x)

# S4 method for class 'dimObj'
nrow(x)

# S4 method for class 'overlapPointDT'
nrow(x)

# S4 method for class 'overlapIntensityDT'
nrow(x)

# S4 method for class 'giotto'
ncol(x)

# S4 method for class 'exprData'
ncol(x)

# S4 method for class 'metaData'
ncol(x)

# S4 method for class 'enrData'
ncol(x)

# S4 method for class 'dimObj'
ncol(x)

# S4 method for class 'overlapPointDT'
ncol(x)

# S4 method for class 'overlapIntensityDT'
ncol(x)

# S4 method for class 'giotto'
dim(x)

# S4 method for class 'spatLocsObj'
dim(x)

# S4 method for class 'exprData'
dim(x)

# S4 method for class 'metaData'
dim(x)

# S4 method for class 'enrData'
dim(x)

# S4 method for class 'giottoLargeImage'
dim(x)

# S4 method for class 'giottoPolygon'
dim(x)

# S4 method for class 'giottoPoints'
dim(x)

# S4 method for class 'overlapPointDT'
dim(x)

# S4 method for class 'overlapIntensityDT'
dim(x)
```

## Arguments

- x:

  object to check dimensions of

## Value

numeric

## Examples

``` r
g <- GiottoData::loadSubObjectMini("giottoPoints")

nrow(g)
```
