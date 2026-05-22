# Transpose

Spatially transpose an object

## Usage

``` r
# S4 method for class 'giotto'
t(x)

# S4 method for class 'spatLocsObj'
t(x)

# S4 method for class 'spatialNetworkObj'
t(x)

# S4 method for class 'giottoPoints'
t(x)

# S4 method for class 'giottoPolygon'
t(x)

# S4 method for class 'giottoLargeImage'
t(x)

# S4 method for class 'giottoAffineImage'
t(x)

# S4 method for class 'affine2d'
t(x)

# S3 method for class 'spatLocsObj'
t(x)

# S3 method for class 'spatialNetworkObj'
t(x)
```

## Arguments

- x:

  object to be transposed

## Value

transposed object

## Examples

``` r
sl <- GiottoData::loadSubObjectMini("spatLocsObj")

plot(t(sl))
```
