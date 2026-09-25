# Coerce to igraph

Coerce a network subobject to the `igraph` it holds. Since 0.6.0 the
`@network` slot holds the graph directly, so this is an accessor rather
than a construction. When the slot is backed, the contents are handed to
`as.igraph()` again, which dispatches on whatever backend class is there
– GiottoDisk registers the method for its own store types.

## Usage

``` r
# S3 method for class 'spatialNetworkObj'
as.igraph(x, ...)

# S3 method for class 'nnNetObj'
as.igraph(x, ...)
```

## Arguments

- x:

  `spatialNetworkObj` or `nnNetObj`

- ...:

  passed to the method for the slot contents when backed. Ignored when
  the slot already holds an `igraph`.

## Value

igraph

## Examples

``` r
sn <- GiottoData::loadSubObjectMini("spatialNetworkObj")
igraph::as.igraph(sn)

nn <- GiottoData::loadSubObjectMini("nnNetObj")
igraph::as.igraph(nn)
```
