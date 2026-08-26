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
#> Warning: Layer ‘data’ is empty
#> Warning: Layer ‘data’ is empty
#> Warning: Layer ‘scale.data’ is empty
#> Warning: Layer ‘scale.data’ is empty
#> Images for RNA assay not found in the data.
#>                         Skipping image processing.
#> python already initialized in this session
#>  active environment : '/usr/bin/python3'
#>  python version : 3.12
#> Warning: [createExprObj] param 'expression_matrix_class' is deprecated
#> > normalized already exists and will be replaced with new values
#> An object of class giotto 
#> >Active spat_unit:  cell 
#> >Active feat_type:  rna 
#> dimensions    : 10, 10 (features, cells)
#> [SUBCELLULAR INFO]
#> [AGGREGATE INFO]
#> expression -----------------------
#>   [cell][rna] raw normalized
#> spatial locations ----------------
#>   [cell] raw
#> 
#> 
#> Use objHistory() to see steps and params used
```
