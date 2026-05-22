# Reconnect a GiottoClass object

Reconnect a GiottoClass object

## Usage

``` r
# S4 method for class 'giottoAffineImage'
reconnect(x, path = NULL, ...)

# S4 method for class 'giottoLargeImage'
reconnect(x, path = NULL, ...)

# S4 method for class 'giottoImage'
reconnect(x, path = NULL, ...)
```

## Arguments

- x:

  object to reconnect

- path:

  character. (optional) New filepath to associate with the object. Only
  needed if the filepath is not normally carried by the object or the
  path has changed.

- ...:

  additional params to pass

## Value

GiottoClass object

## Examples

``` r
temp <- tempfile()
f <- system.file("extdata/toy_intensity.tif", package = "GiottoClass")
a <- createGiottoLargeImage(f)
saveRDS(a, temp)

b <- readRDS(temp) # expected to be null pointer
b <- reconnect(b) # reconnected to source image
```
