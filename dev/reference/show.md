# Show methods for Giotto classes

Show methods for Giotto classes

## Usage

``` r
# S4 method for class 'giotto'
show(object)

# S4 method for class 'packedGiotto'
show(object)

# S4 method for class 'exprObj'
show(object)

# S4 method for class 'cellMetaObj'
show(object)

# S4 method for class 'featMetaObj'
show(object)

# S4 method for class 'dimObj'
show(object)

# S4 method for class 'nnNetObj'
show(object)

# S4 method for class 'spatLocsObj'
show(object)

# S4 method for class 'spatialNetworkObj'
show(object)

# S4 method for class 'spatialGridObj'
show(object)

# S4 method for class 'spatEnrObj'
show(object)

# S4 method for class 'giottoPolygon'
show(object)

# S4 method for class 'packedGiottoPolygon'
show(object)

# S4 method for class 'giottoPoints'
show(object)

# S4 method for class 'packedGiottoPoints'
show(object)

# S4 method for class 'giottoImage'
show(object)

# S4 method for class 'giottoLargeImage'
show(object)

# S4 method for class 'affine2d'
show(object)
```

## Arguments

- object:

  object to show

## Value

giotto slot

## Examples

``` r
sl <- data.frame(seq(10), seq(10), letters[seq(10)]) |>
    createSpatLocsObj(verbose = FALSE)
show(sl)
```
