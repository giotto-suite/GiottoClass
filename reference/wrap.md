# Wrap giotto terra pointer information

Extension of wrap methods from terra for Giotto's terra-based S4
objects. Allows pointer information to be packaged into memory so that
it can be passed over a connection (e.g. nodes on a computer cluster)

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

## Examples

``` r
g <- GiottoData::loadSubObjectMini("giottoPoints")

wrap(g)
#> [1] "This is a packedGiottoPoints object. Use 'GiottoClass::vect()' to unpack it"
```
