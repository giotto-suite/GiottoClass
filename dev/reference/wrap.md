# Wrap giotto terra pointer information

Extension of wrap methods from terra for Giotto's terra-based S4
objects. Allows pointer information to be packaged into memory so that
it can be passed over a connection (e.g. nodes on a computer cluster)

This pattern is no longer maintained and may be removed in a future
release. Do not build on it. Use the by-reference path instead:
[`saveGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/saveGiotto.md)
/
[`loadGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/loadGiotto.md)
for persistence, and a `gsource` backend to make data reachable from a
worker process.

## Usage

``` r
# S4 method for class 'giottoPolygon'
wrap(x)

# S4 method for class 'giotto'
wrap(x)

# S4 method for class 'giottoPoints'
wrap(x)

# S4 method for class 'packedGiottoPolygon'
vect(x)

# S4 method for class 'packedGiottoPoints'
vect(x)

# S4 method for class 'packedGiotto'
vect(x)
```

## Arguments

- x:

  giottoPolygon or giottoPoints

## Value

wrapped giottoPolygon or giottoPoints

## Methods (by class)

- `wrap(giottoPolygon)`: Wrap giottoPolygon

- `wrap(giotto)`: Wrap giotto

- `wrap(giottoPoints)`: Wrap giottoPoints

- `vect(packedGiottoPolygon)`: Unwrap giottoPolygon

- `vect(packedGiottoPoints)`: Unwrap giottoPolygon

- `vect(packedGiotto)`: Unwrap giotto

## See also

[`saveGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/saveGiotto.md),
[`loadGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/loadGiotto.md)

## Examples

``` r
g <- GiottoData::loadSubObjectMini("giottoPoints")

wrap(g)
```
