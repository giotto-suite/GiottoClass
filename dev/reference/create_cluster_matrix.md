# create_cluster_matrix

creates aggregated matrix for a given clustering column

## Usage

``` r
create_cluster_matrix(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  expression_values = c("normalized", "scaled", "custom"),
  cluster_column,
  feat_subset = NULL,
  gene_subset = NULL
)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit (e.g. "cell")

- feat_type:

  feature type (e.g. "rna", "dna", "protein")

- expression_values:

  name of expression values to use

- cluster_column:

  name of cluster column to use,

- feat_subset:

  subset of features to use

- gene_subset:

  deprecated do not use.

## Value

matrix

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")

create_cluster_matrix(g, cluster_column = "leiden_clus")
```
