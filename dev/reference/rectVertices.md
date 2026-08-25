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

[generate_grid](https://giotto-suite.github.io/GiottoClass/dev/reference/generate_grid.md)

Other polygon stamping:
[`circleVertices()`](https://giotto-suite.github.io/GiottoClass/dev/reference/circleVertices.md),
[`hexVertices()`](https://giotto-suite.github.io/GiottoClass/dev/reference/hexVertices.md),
[`polyStamp()`](https://giotto-suite.github.io/GiottoClass/dev/reference/polyStamp.md)

## Examples

``` r
rectVertices(c(x = 1, y = 2))
```
