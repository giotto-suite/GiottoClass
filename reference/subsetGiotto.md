# subsetGiotto

Subsets Giotto object including previous analyses. For subsetting the
subcellular information only without editing the aggregate information,
use
[subsetGiottoLocsSubcellular](https://giotto-suite.github.io/GiottoClass/reference/subsetGiottoLocsSubcellular.md)

## Usage

``` r
subsetGiotto(
  gobject,
  spat_unit = ":all:",
  feat_type = "rna",
  cell_ids = NULL,
  feat_ids = NULL,
  poly_info = spat_unit,
  all_spat_units = NULL,
  all_feat_types = NULL,
  spat_unit_fsub = ":all:",
  feat_type_ssub = ":all:",
  verbose = FALSE,
  toplevel_params = 2
)
```

## Arguments

- gobject:

  giotto object

- spat_unit, feat_type:

  character vector. (default = ':all:') One or more spatial units or
  feature types to subset. Accepts ':all:' as a token to subset across
  all available.

- cell_ids:

  character. cell IDs to keep

- feat_ids:

  character. feature IDs to keep

- poly_info:

  character. polygon info(s) to subset if present. (defaults to be the
  same as the spat_unit)

- all_spat_units:

  deprecated. use spat_unit_fsub = ':all:'

- all_feat_types:

  deprecated. use feat_type_ssub = ':all:'

- spat_unit_fsub:

  character vector. (default = ':all:') limit feat_id subsets to these
  spat_units

- feat_type_ssub:

  character vector. (default = ':all:') limit cell_id subsets to these
  feat_types

- verbose:

  be verbose

- toplevel_params:

  parameters to extract

## Value

giotto object

## Details

Subsets a Giotto object for a specific spatial unit and feature type

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

subsetGiotto(g, cell_ids = c("AACTCGATGGCGCAGT-1", "GGCTGGCTAGCTTAAA-1"))
#> An object of class giotto 
#> >Active spat_unit:  cell 
#> >Active feat_type:  rna 
#> dimensions    : 634, 2 (features, cells)
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
#>   [cell][rna] cluster_metagene DWLS
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
