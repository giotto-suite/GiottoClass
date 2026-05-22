# Generate rectangular polygon vertices

Generates vertex coordinates for a rectangle with dimensions given
through `dims` param.

## Usage

``` r
rectVertices(dims)
```

## Arguments

- dims:

  named vector in the style of c(x = `numeric`, y = `numeric`) that
  defines the width (x) and height (y) of the generated rectangle
  polygon.

## Value

a data.table of rectangle vertices

## See also

[generate_grid](https://giotto-suite.github.io/GiottoClass/reference/generate_grid.md)

Other polygon stamping:
[`circleVertices()`](https://giotto-suite.github.io/GiottoClass/reference/circleVertices.md),
[`hexVertices()`](https://giotto-suite.github.io/GiottoClass/reference/hexVertices.md),
[`polyStamp()`](https://giotto-suite.github.io/GiottoClass/reference/polyStamp.md)

## Examples

``` r
rectVertices(c(x = 1, y = 2))
#>        x     y
#>    <num> <num>
#> 1:     0     0
#> 2:     0     2
#> 3:     1     2
#> 4:     1     0
```
