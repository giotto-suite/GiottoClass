# annotateSpatialNetwork

Annotate spatial network with cell metadata information.

## Usage

``` r
annotateSpatialNetwork(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  spatial_network_name = "Delaunay_network",
  cluster_column,
  create_full_network = FALSE
)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit

- feat_type:

  feature type

- spatial_network_name:

  name of spatial network to use

- cluster_column:

  name of column to use for clusters

- create_full_network:

  convert from reduced to full network representation

## Value

annotated network in data.table format

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")

annotateSpatialNetwork(g, cluster_column = "leiden_clus")
```
