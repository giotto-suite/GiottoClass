# defaultViewCoordinator

Pick the default
[viewCoordinator](https://giotto-suite.github.io/GiottoClass/dev/reference/viewCoordinator-class.md)
for resolving views on a `gobject` whose source is `source`. GiottoClass
provides the `ANY`-signature method returning
[dataTableCoordinator](https://giotto-suite.github.io/GiottoClass/dev/reference/dataTableCoordinator-class.md)
(in-memory reference). Downstream packages register their own
coordinators by adding methods for their source class — e.g. GiottoDisk
registers a `gsource` method returning `parquetCoordinator()`. S4
inheritance picks up subclasses automatically.

## Usage

``` r
defaultViewCoordinator(source, ...)

# S4 method for class 'ANY'
defaultViewCoordinator(source, ...)
```

## Arguments

- source:

  the `@source` slot of the gobject (`NULL` is handled upstream by
  `.default_view_coordinator()`)

- ...:

  reserved

## Value

a `viewCoordinator`-inheriting object
