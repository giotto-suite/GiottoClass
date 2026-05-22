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
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : 'giotto_env'
#>  python version : 3.10
s <- giottoToSeuratV5(g, tech = "Visium")
#> Meta data updated for assay: rna
#> Warning: Keys should be one or more alphanumeric characters followed by an underscore, setting key from custom_pca_ to custompca_
#> Warning: Keys should be one or more alphanumeric characters followed by an underscore, setting key from custom_umap_ to customumap_
seuratToGiottoV5(s, spatial_assay = "rna")
#> python already initialized in this session
#>  active environment : 'giotto_env'
#>  python version : 3.10
#> Warning: [createExprObj] param 'expression_matrix_class' is deprecated
#> Warning: 
#> List item [1]: Not possible to
#>  read dimObj. Returning without modifications
#> Warning: 
#> List item [2]: Not possible to
#>  read dimObj. Returning without modifications
#> > normalized already exists and will be replaced with new values
#> Warning: Item 2 has 624 rows but longest item has 634; recycled with remainder.
#> An object of class giotto 
#> >Active spat_unit:  cell 
#> >Active feat_type:  rna 
#> dimensions    : 634, 624 (features, cells)
#> [SUBCELLULAR INFO]
#> [AGGREGATE INFO]
#> expression -----------------------
#>   [cell][rna] raw normalized
#> spatial locations ----------------
#>   [cell] raw
#> dim reduction --------------------
#>   [cell][rna] pca umap
#> attached images ------------------
#> images      : alignment image 
#> 
#> 
#> Use objHistory() to see steps and params used
```
