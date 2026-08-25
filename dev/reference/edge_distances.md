# Calculate network edge euclidean distances

Calculate network edge euclidean distances

## Usage

``` r
edge_distances(x, y, x_node_ids = NULL)
```

## Arguments

- x:

  matrix of nodes info with coords. Rows should be samples, Cols should
  be variables

- y:

  network data.table with `from` and `to` cols. Usually integer indices
  matching the rows of x.

- x_node_ids:

  if y is indexed by character in from and to cols, then the node IDs
  that apply to the coords in x must be supplied as a character vector

## Value

numeric

## Examples

``` r
m <- matrix(c(0, 0, 0, 1, 1, 1, 3, 2, 4), byrow = TRUE, nrow = 3)
edges <- data.table::data.table(
    from = c(1, 1),
    to = c(2, 3)
)

edge_distances(m, edges)
```
