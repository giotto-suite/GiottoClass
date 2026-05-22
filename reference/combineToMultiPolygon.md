# Combine giottoPolygon geometries

**SUPERCEDED**. Use
[`combineGeom()`](https://giotto-suite.github.io/GiottoClass/reference/combine_split_geoms.md)
instead.  
  
Combine multiple giottoPolygon geometries into a set of multipolygons.
Note that attributes cannot be kept

## Usage

``` r
combineToMultiPolygon(x, groups, name = NULL)
```

## Arguments

- x:

  giottoPolygon

- groups:

  data.frame with columns 'poly_ID' and 'group_ID' that relates which
  polygons should now be combined under which group_ID

- name:

  (optional) name for the giottoPolygon object

## Value

giottoPolygon

## Examples

``` r
gpoly <- GiottoData::loadSubObjectMini("giottoPolygon")
groups <- data.table::data.table(
    poly_ID = gpoly$poly_ID,
    group_ID = sort(rep(LETTERS[seq_len(5)], length.out = nrow(gpoly)))
    # make 5 groups
)

multi_gp <- combineToMultiPolygon(gpoly, groups)

plot(multi_gp["A"])
```
