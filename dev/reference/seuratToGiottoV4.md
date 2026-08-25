# Convert a Seurat V4 object to a Giotto object

Convert a Seurat V4 object to a Giotto object

## Usage

``` r
seuratToGiottoV4(
  sobject,
  spatial_assay = "Spatial",
  dim_reduction = c("pca", "umap"),
  subcellular_assay = "Vizgen",
  sp_network = NULL,
  nn_network = NULL,
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

- verbose:

  logical. Default to TRUE object. Default is `"Vizgen"`.

## Value

A Giotto object converted from Seurat object with all computations
stored in it.

## Examples

``` r
m_expression <- Matrix::Matrix(rnorm(100), nrow = 10, sparse = TRUE)
s <- Seurat::CreateSeuratObject(counts = m_expression)

seuratToGiottoV5(s, spatial_assay = "RNA")
```
