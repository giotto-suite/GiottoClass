# Giotto object uid

The identifier a `giotto` or
[giottoMulti](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoMulti-class.md)
object is given when it is created. It stays with the object as it is
changed, copied, subset, sliced, saved and loaded, so it says which
object this is, not what it holds: two states of one object share it.
Compare
[`objManifest()`](https://giotto-suite.github.io/GiottoClass/dev/reference/objManifest.md)
contents to tell states apart.

A new object, such as one built by a reader or by
[`joinGiottoObjects()`](https://giotto-suite.github.io/GiottoClass/dev/reference/joinGiottoObjects.md),
gets a new uid. So does one assembled from another's parts, for example
with `setGiotto(giotto(), as.list(g))`. A `giottoMulti` has its own uid;
each of its samples keeps its own.

## Usage

``` r
objUID(x)
```

## Arguments

- x:

  giotto or giottoMulti object

## Value

`character(1)`, or `NULL` for an object that has none

## Examples

``` r
g <- giotto()
objUID(g)
g2 <- g
identical(objUID(g), objUID(g2))
```
