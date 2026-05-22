# showProcessingSteps

shows the sequential processing steps that were performed on a Giotto
object in a summarized format

## Usage

``` r
showProcessingSteps(gobject)
```

## Arguments

- gobject:

  giotto object

## Value

list of processing steps and names

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

showProcessingSteps(g)
#> Warning: `showProcessingSteps()` was deprecated in GiottoClass 0.4.0.
#> ℹ Please use `objHistory()` instead.
#> ℹ objHistory with arg `summarized = TRUE` replaces this functionality
#> Processing steps:
#> 0_normalize
#> 1_subset
#> 2_filter
#> name info: tag tag
#> 3_feat_stats
#> 4_cell_stats
#> 5_hvf
#> name info: hvf HVFplot
#> 6_pca
#> 7_umap
#> 8_tsne
#> 9_nn_network
#> 10_cluster
#> name info: leiden_clus sNN.pca
#> 11_delaunay_spatial_network
#> name info: Delaunay_network
#> 12_spatial_network
#> name info: spatial_network
#> 13_create_metafeat
#> name info: cluster_metagene
#> 14_pca
#> name info: custom_pca
#> 15_umap
#> name info: custom_pca custom_umap
#> 16_nn_network
#> name info: custom_pca custom_NN
#> 17_cluster
#> name info: custom_leiden custom_NN
#> 18_spatial_deconvolution
#> name info: DWLS
#> 19_spatial_deconvolution
#> name info: DWLS
#> 20_spatial_deconvolution
#> name info: DWLS
```
