# calculateOverlapRaster

calculate overlap between cellular structures (polygons) and features
(points).

## Usage

``` r
calculateOverlapRaster(
  gobject,
  name_overlap = NULL,
  spatial_info = NULL,
  poly_ID_names = NULL,
  feat_info = NULL,
  feat_subset_column = NULL,
  feat_subset_values = NULL,
  feat_count_column = NULL,
  return_gobject = TRUE,
  verbose = TRUE,
  feat_subset_ids = deprecated(),
  count_info_column = deprecated()
)
```

## Arguments

- gobject:

  giotto object

- name_overlap:

  name for the overlap results (default to feat_info parameter)

- spatial_info:

  character. name polygon information

- poly_ID_names:

  (optional) list of poly_IDs to use

- feat_info:

  character. name of feature information

- feat_subset_column:

  feature info column to subset features with

- feat_subset_values:

  value(s) within feature info `feat_subset_column` to use for
  subsetting

- feat_count_column:

  column with count information (optional)

- return_gobject:

  return giotto object (default: TRUE)

- verbose:

  be verbose

- feat_subset_ids:

  deprecated. Use `feat_subset_values` instead.

- count_info_column:

  deprecated. Use `feat_count_column` instead.

## Value

giotto object or spatVector with overlapping information

## Details

Serial overlapping function.

## See also

[`.calculate_overlap_raster()`](https://giotto-suite.github.io/GiottoClass/reference/dot-calculate_overlap_raster.md)

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : '/usr/bin/python3'
#>  python version : 3.12

calculateOverlapRaster(g)
#> Warning: `calculateOverlapRaster()` was deprecated in GiottoClass 0.4.7.
#> ℹ Please use `aggregateFeatures()` instead.
#> ℹ `calculateOverlap()` is another option if only the overlap step is desired.
#> 1. convert polygon to raster
#> 2. overlap raster and points
#> An object of class giotto 
#> >Active spat_unit:  z0 
#> >Active feat_type:  rna 
#> dimensions    : 337, 498 (features, cells)
#> [SUBCELLULAR INFO]
#> polygons      : z0 z1 aggregate 
#> features      : rna 
#> [AGGREGATE INFO]
#> expression -----------------------
#>   [z0][rna] raw
#>   [z1][rna] raw
#>   [aggregate][rna] raw normalized scaled pearson
#> spatial locations ----------------
#>   [z0] raw
#>   [z1] raw
#>   [aggregate] raw
#> spatial networks -----------------
#>   [aggregate] Delaunay_network kNN_network
#> spatial enrichments --------------
#>   [aggregate][rna] cluster_metagene
#> dim reduction --------------------
#>   [aggregate][rna] pca umap tsne
#> nearest neighbor networks --------
#>   [aggregate][rna] sNN.pca
#> attached images ------------------
#> images      : 4 items...
#> 
#> 
#> Use objHistory() to see steps and params used
```
