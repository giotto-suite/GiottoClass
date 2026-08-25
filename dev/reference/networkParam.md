# networkParam — Dispatcher constructor

Returns the appropriate concrete `*NetworkParam` based on `type`.
Equivalent to calling
[`kNNNetworkParam()`](https://giotto-suite.github.io/GiottoClass/dev/reference/kNNNetworkParam-class.md),
[`sNNNetworkParam()`](https://giotto-suite.github.io/GiottoClass/dev/reference/sNNNetworkParam-class.md),
or
[`delaunayNetworkParam()`](https://giotto-suite.github.io/GiottoClass/dev/reference/delaunayNetworkParam-class.md)
directly.

## Usage

``` r
networkParam(type = c("kNN", "sNN", "delaunay"), ...)
```

## Arguments

- type:

  one of `"kNN"`, `"sNN"`, `"delaunay"`

- ...:

  arguments forwarded to the type-specific constructor

## Value

a
[networkParam](https://giotto-suite.github.io/GiottoClass/dev/reference/networkParam-class.md)-inheriting
object

## Examples

``` r
p <- networkParam("kNN", k = 30)
```
