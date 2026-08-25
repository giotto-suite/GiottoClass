# Convert a Seurat V5 object to a Giotto object

Convert a Seurat V5 object to a Giotto object

## Usage

``` r
seuratToGiottoV5(
  sobject,
  spatial_assay = "Spatial",
  dim_reduction = c("pca", "umap"),
  subcellular_assay = "SCT",
  sp_network = NULL,
  nn_network = NULL,
  polygon = TRUE,
  verbose = TRUE
)
```

## Arguments

- sobject:

  Input Seurat object to convert to Giotto object

- spatial_assay:

  Specify name of the spatial assay slot in Seurat. Default is
  `"Spatial"`.

- dim_reduction:

  Specify which dimensional reduction computations to fetch from input
  Seurat object. Default is `"c('pca', 'umap')"`.

- subcellular_assay:

  Specify name of the subcellular assay in input

- sp_network:

  sp_network

- nn_network:

  nn_network

- polygon:

  Logical. If `TRUE`, extract segmentation polygons and centroids from
  Seurat image boundaries and convert them to a Giotto polygon object.

- verbose:

  logical. Default to TRUE object. Default is `"Vizgen"`.

## Value

A Giotto object converted from Seurat object with all computations
stored in it.

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")
s <- giottoToSeuratV5(g, tech = "Visium")
seuratToGiottoV5(s, spatial_assay = "rna")
```
