# Split a Giotto Object

Split a Giotto object based on a cell metadata column into a list of
multiple Giotto objects.

## Usage

``` r
splitGiotto(gobject, by, spat_unit = NULL, feat_type = NULL)
```

## Arguments

- gobject:

  giotto object to split

- by:

  cell metadata column by which to split the object

- spat_unit:

  character. Controls which spatial unit to pull splitting information
  from. However, all spatial units will always be affected by the split.

- feat_type:

  character. Split affects these feature type(s). Default is "rna"

## Value

`list` of `giotto` objects

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")
splitGiotto(g, "leiden_clus")
```
