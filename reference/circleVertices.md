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

[generate_grid](https://giotto-suite.github.io/GiottoClass/reference/generate_grid.md)

Other polygon stamping:
[`hexVertices()`](https://giotto-suite.github.io/GiottoClass/reference/hexVertices.md),
[`polyStamp()`](https://giotto-suite.github.io/GiottoClass/reference/polyStamp.md),
[`rectVertices()`](https://giotto-suite.github.io/GiottoClass/reference/rectVertices.md)

## Examples

``` r
circleVertices(radius = 10)
#>              x             y
#>          <num>         <num>
#>  1: 10.0000000  0.000000e+00
#>  2:  9.6858316  2.486899e+00
#>  3:  8.7630668  4.817537e+00
#>  4:  7.2896863  6.845471e+00
#>  5:  5.3582679  8.443279e+00
#>  6:  3.0901699  9.510565e+00
#>  7:  0.6279052  9.980267e+00
#>  8: -1.8738131  9.822873e+00
#>  9: -4.2577929  9.048271e+00
#> 10: -6.3742399  7.705132e+00
#> 11: -8.0901699  5.877853e+00
#> 12: -9.2977649  3.681246e+00
#> 13: -9.9211470  1.253332e+00
#> 14: -9.9211470 -1.253332e+00
#> 15: -9.2977649 -3.681246e+00
#> 16: -8.0901699 -5.877853e+00
#> 17: -6.3742399 -7.705132e+00
#> 18: -4.2577929 -9.048271e+00
#> 19: -1.8738131 -9.822873e+00
#> 20:  0.6279052 -9.980267e+00
#> 21:  3.0901699 -9.510565e+00
#> 22:  5.3582679 -8.443279e+00
#> 23:  7.2896863 -6.845471e+00
#> 24:  8.7630668 -4.817537e+00
#> 25:  9.6858316 -2.486899e+00
#> 26: 10.0000000 -2.449294e-15
#>              x             y
#>          <num>         <num>
```
