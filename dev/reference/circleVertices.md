# Generate circle polygon vertices

Generates vertex coordinates for a circle around (0,0) with the given
radius. Modified from packcircles.

## Usage

``` r
circleVertices(radius, npoints = 25)
```

## Arguments

- radius:

  radius of circle to be drawn

- npoints:

  number of vertices to generate

## Value

a data.table of circle vertices

## See also

[generate_grid](https://giotto-suite.github.io/GiottoClass/dev/reference/generate_grid.md)

Other polygon stamping:
[`hexVertices()`](https://giotto-suite.github.io/GiottoClass/dev/reference/hexVertices.md),
[`polyStamp()`](https://giotto-suite.github.io/GiottoClass/dev/reference/polyStamp.md),
[`rectVertices()`](https://giotto-suite.github.io/GiottoClass/dev/reference/rectVertices.md)

## Examples

``` r
circleVertices(radius = 10)
```
