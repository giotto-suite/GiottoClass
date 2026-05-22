# Create a text representation of an object

Create a text representation of an object

## Usage

``` r
# S4 method for class 'giottoImage'
as.character(x, ...)

# S4 method for class 'svkey'
as.character(x, ...)

# S4 method for class 'giottoLargeImage'
as.character(x, ...)
```

## Arguments

- x:

  object

- ...:

  additional params to pass (none implemented)

## Value

character

## Examples

``` r
img <- GiottoData::loadSubObjectMini("giottoLargeImage")
as.character(img)
#> [1] "<giottoLargeImage> dapi_z0"
```
