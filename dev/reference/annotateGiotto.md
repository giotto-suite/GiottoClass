# Annotate Giotto object

Map user provided annotations/labels based on another existing metadata
column (usually clustering labels)

## Usage

``` r
annotateGiotto(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  annotation_vector = NULL,
  cluster_column = NULL,
  name = "cell_types",
  replace = TRUE
)
```

## Arguments

- gobject:

  `giotto` object

- spat_unit:

  spatial unit

- feat_type:

  feature type

- annotation_vector:

  named `character` vector. Vector names are labels in the cluster
  column. Labels to assign are the vector values.

- cluster_column:

  `character`. Cell metaadata column to map annotation values based on.

- name:

  new name for annotation column

- replace:

  logical. Only consulted when the target `name` column already exists.
  `TRUE` (default) replaces it outright, so a row whose cluster value is
  `NA` or unmapped becomes `NA`. `FALSE` merges: a row the new mapping
  resolves is overwritten, a row it yields `NA` for keeps its existing
  value, which is what makes iterative annotation refinement possible.

## Value

`giotto` object

## Details

You need to specify which (cluster) column you want to annotate and you
need to provide an annotation vector like this:

- 1\. identify the cell type of each cluster

- 2\. create a vector of these cell types, e.g. cell_types = c('T-cell',
  'B-cell', 'Stromal')

- 3\. provide original cluster names to previous vector, e.g.
  names(cell_types) = c(2, 1, 3)

`NA` values in `cluster_column` are tolerated and carry through to the
new annotation column as `NA` – common on a `giottoMulti`, where joint
`@cell_metadata` keeps the full population while the analysis pool is
narrower. A cluster value with no entry in `annotation_vector` also
becomes `NA`, and both it and any unused `annotation_vector` key are
reported rather than raised.

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")

annotation <- c(
    "1" = "cell_type_1",
    "2" = "cell_type_2",
    "3" = "cell_type_3",
    "4" = "cell_type_4",
    "5" = "cell_type_5",
    "6" = "cell_type_6",
    "7" = "cell_type_7",
    "8" = "cell_type_8"
)

g <- annotateGiotto(g,
    annotation_vector = annotation,
    cluster_column = "leiden_clus"
)
```
