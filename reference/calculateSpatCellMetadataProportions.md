# calculateSpatCellMetadataProportions

calculates a proportion table for a cell metadata column (e.g. cluster
labels) for all the spatial neighbors of a source cell. In other words
it calculates the niche composition for a given annotation for each
cell.

## Usage

``` r
calculateSpatCellMetadataProportions(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  spat_network = NULL,
  metadata_column = NULL,
  name = "proportion",
  return_gobject = TRUE
)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit

- feat_type:

  feature type

- spat_network:

  spatial network

- metadata_column:

  metadata column to use

- name:

  descriptive name for the calculated proportions

- return_gobject:

  return giotto object

## Value

giotto object (default) or enrichment object if return_gobject = FALSE

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : '/usr/bin/python3'
#>  python version : 3.12

calculateSpatCellMetadataProportions(g,
    spat_network = "Delaunay_network", metadata_column = "leiden_clus"
)
#> Warning: `calculateSpatCellMetadataProportions()` was deprecated in GiottoClass 0.4.8.
#> ℹ Please use `calculateLabelProportions()` instead.
#> An object of class giotto 
#> >Active spat_unit:  cell 
#> >Active feat_type:  rna 
#> dimensions    : 634, 624 (features, cells)
#> [SUBCELLULAR INFO]
#> polygons      : cell 
#> [AGGREGATE INFO]
#> expression -----------------------
#>   [cell][rna] raw normalized scaled
#> spatial locations ----------------
#>   [cell] raw
#> spatial networks -----------------
#>   [cell] Delaunay_network spatial_network
#> spatial enrichments --------------
#>   [cell][rna] cluster_metagene DWLS proportion
#> dim reduction --------------------
#>   [cell][rna] pca custom_pca umap custom_umap tsne
#> nearest neighbor networks --------
#>   [cell][rna] sNN.pca custom_NN
#> attached images ------------------
#> images      : alignment image 
#> 
#> 
#> Use objHistory() to see steps and params used
```
