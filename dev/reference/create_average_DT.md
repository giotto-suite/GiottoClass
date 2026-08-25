# create_average_DT

calculates average gene expression for a cell metadata factor (e.g.
cluster)

## Usage

``` r
create_average_DT(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  meta_data_name,
  expression_values = c("normalized", "scaled", "custom")
)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit

- feat_type:

  feature type

- meta_data_name:

  name of metadata column to use

- expression_values:

  which expression values to use

## Value

data.table with average gene expression values for each factor

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")

create_average_DT(g, meta_data_name = "leiden_clus")
```
