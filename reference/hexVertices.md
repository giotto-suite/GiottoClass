# Generate regular hexagon vertices

Generates vertex coordinates for a regular hexagon.

## Usage

``` r
hexVertices(radius, major_axis = c("v", "h"))
```

## Arguments

- radius:

  radius of the hexagon

- major_axis:

  orientation of the major axis 'v' is vertical (default) and 'h' is
  horizontal

## Value

a data.table of regular hexagon vertices

## See also

[generate_grid](https://giotto-suite.github.io/GiottoClass/reference/generate_grid.md)

Other polygon stamping:
[`circleVertices()`](https://giotto-suite.github.io/GiottoClass/reference/circleVertices.md),
[`polyStamp()`](https://giotto-suite.github.io/GiottoClass/reference/polyStamp.md),
[`rectVertices()`](https://giotto-suite.github.io/GiottoClass/reference/rectVertices.md)

## Examples

``` r
hexVertices(radius = 10)
#>            x     y
#>        <num> <num>
#> 1:  0.000000    10
#> 2:  8.660254     5
#> 3:  8.660254    -5
#> 4:  0.000000   -10
#> 5: -8.660254    -5
#> 6: -8.660254     5
```
