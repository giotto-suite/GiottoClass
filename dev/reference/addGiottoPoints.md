# Add subcellular giotto points object to giotto object

Adds Giotto points to an existing Giotto object

## Usage

``` r
addGiottoPoints(gobject, gpoints)

addGiottoPoints3D(gobject, coords, feat_type = "rna")
```

## Arguments

- gobject:

  giotto object

- gpoints:

  list of giotto point objects, see
  [`createGiottoPoints`](https://giotto-suite.github.io/GiottoClass/dev/reference/createGiottoPoints.md)

- coords:

  A [data.frame](https://rdrr.io/r/base/data.frame.html) or `spatVector`
  with at least xyz coordinates and feature ids.

- feat_type:

  a character. The feat_type must previously exist in the Giotto object.
  Default = "rna".

## Value

giotto object

## Examples

``` r
x_points <- GiottoData::loadSubObjectMini("giottoPoints")
x_expression <- matrix(seq_len(337 * 2), nrow = 337)
rownames(x_expression) <- unique(x_points$feat_ID)
g <- createGiottoObject(expression = x_expression)

g <- addGiottoPoints(gobject = g, gpoints = list(x_points))
```
