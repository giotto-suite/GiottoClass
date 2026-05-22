# Giotto object history

Print and return giotto object history

## Usage

``` r
objHistory(object, summarized = FALSE)
```

## Arguments

- object:

  giotto object

- summarized:

  logical. whether print should be summarized

## Value

list

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

objHistory(g)
#> $`0_normalize`
#>           gobject expression_values      norm_methods library_size_norm 
#>     "mini_visium"             "raw"        "standard"            "TRUE" 
#>       scalefactor          log_norm        log_offset           logbase 
#>            "6000"            "TRUE"               "1"               "2" 
#>       scale_feats       scale_cells       scale_order             theta 
#>            "TRUE"            "TRUE"     "first_feats"             "100" 
#>       update_slot           verbose 
#>          "scaled"               "T" 
#> 
#> $`1_subset`
#>             gobject           spat_unit           feat_type            cell_ids 
#>           "gobject"         "spat_unit"         "feat_type" "selected_cell_ids" 
#>            feat_ids           poly_info      spat_unit_fsub      feat_type_ssub 
#> "selected_feat_ids"         "poly_info"    "spat_unit_fsub"    "feat_type_ssub" 
#>             verbose     toplevel_params 
#>           "verbose"                 "2" 
#> 
#> $`2_filter`
#>                gobject      expression_values   expression_threshold 
#>          "mini_visium"                  "raw"                    "1" 
#>  feat_det_in_min_cells min_det_feats_per_cell         spat_unit_fsub 
#>                    "5"                   "20"                ":all:" 
#>         feat_type_ssub              tag_cells          tag_cell_name 
#>                ":all:"                "FALSE"                  "tag" 
#>              tag_feats         tag_feats_name                verbose 
#>                "FALSE"                  "tag"                    "T" 
#> 
#> $`3_feat_stats`
#>             gobject   expression_values detection_threshold      return_gobject 
#>       "mini_visium"        "normalized"                 "0"              "TRUE" 
#>             verbose 
#>              "TRUE" 
#> 
#> $`4_cell_stats`
#>             gobject   expression_values detection_threshold      return_gobject 
#>       "mini_visium"        "normalized"                 "0"              "TRUE" 
#>             verbose 
#>              "TRUE" 
#> 
#> $`5_hvf`
#>              gobject    expression_values               method 
#>        "mini_visium"         "normalized"         "cov_groups" 
#>    reverse_log_scale              logbase expression_threshold 
#>              "FALSE"                  "2"                  "0" 
#> nr_expression_groups     zscore_threshold              HVFname 
#>                 "20"                "1.5"                "hvf" 
#>    difference_in_cov        var_threshold             set_seed 
#>                "0.1"                "1.5"               "TRUE" 
#>          seed_number           save_param    default_save_name 
#>               "1234"             "list()"            "HVFplot" 
#>       return_gobject              verbose 
#>               "TRUE"               "TRUE" 
#> 
#> $`6_pca`
#>                       gobject             expression_values 
#>                 "mini_visium"                  "normalized" 
#>                     reduction                return_gobject 
#>                       "cells"                        "TRUE" 
#>                        center                    scale_unit 
#>                        "TRUE"                        "TRUE" 
#>                           ncp                        method 
#>                         "100"                       "irlba" 
#>                 method_params                           rev 
#> "BiocParallel::SerialParam()"                       "FALSE" 
#>                      set_seed                   seed_number 
#>                        "TRUE"                        "1234" 
#>                       verbose                           ... 
#>                        "TRUE"                            "" 
#> 
#> $`7_umap`
#>              gobject    expression_values            reduction 
#>        "mini_visium"         "normalized"              "cells" 
#> dim_reduction_to_use    dimensions_to_use       return_gobject 
#>                "pca"               "1:10"               "TRUE" 
#>          n_neighbors         n_components             n_epochs 
#>                 "40"                  "2"                "400" 
#>             min_dist            n_threads               spread 
#>               "0.01"                 "NA"                  "5" 
#>             set_seed          seed_number              verbose 
#>               "TRUE"               "1234"               "TRUE" 
#>      toplevel_params                  ... 
#>                  "2"                   "" 
#> 
#> $`8_tsne`
#>              gobject    expression_values            reduction 
#>        "mini_visium"         "normalized"              "cells" 
#> dim_reduction_to_use    dimensions_to_use       return_gobject 
#>                "pca"               "1:10"               "TRUE" 
#>                 dims           perplexity                theta 
#>                  "2"                 "30"                "0.5" 
#>         do_PCA_first             set_seed          seed_number 
#>              "FALSE"               "TRUE"               "1234" 
#>              verbose                  ... 
#>               "TRUE"                   "" 
#> 
#> $`9_nn_network`
#>              gobject                 type dim_reduction_to_use 
#>        "mini_visium"                "sNN"                "pca" 
#>    dimensions_to_use    expression_values       return_gobject 
#>                "1:5"         "normalized"               "TRUE" 
#>                    k       minimum_shared           top_shared 
#>                 "10"                  "5"                  "3" 
#>              verbose                  ... 
#>               "TRUE"                   "" 
#> 
#> $`10_cluster`
#>                          gobject                             name 
#>                    "mini_visium"                    "leiden_clus" 
#>                nn_network_to_use                     network_name 
#>                            "sNN"                        "sNN.pca" 
#>                       resolution                       weight_col 
#>                            "0.1"                         "weight" 
#>                   partition_type                     n_iterations 
#> "RBConfigurationVertexPartition"                           "1000" 
#>                   return_gobject                         set_seed 
#>                           "TRUE"                           "TRUE" 
#>                      seed_number 
#>                           "1234" 
#> 
#> $`11_delaunay_spatial_network`
#>               dimensions used                        method 
#> "dimensions: sdimx and sdimy"                      "deldir" 
#>    maximum distance threshold       name of spatial network 
#>                        "auto"            "Delaunay_network" 
#> 
#> $`12_spatial_network`
#>               k neighbours            dimensions used 
#>                       "10"                      "all" 
#> maximum distance threshold    name of spatial network 
#>                      "400"          "spatial_network" 
#> 
#> $`13_create_metafeat`
#>            gobject  expression_values      feat_clusters               stat 
#>      "mini_visium"       "normalized"    "cluster_genes"             "mean" 
#>               name     return_gobject 
#> "cluster_metagene"             "TRUE" 
#> 
#> $`14_pca`
#>                       gobject             expression_values 
#>                 "mini_visium"                  "normalized" 
#>                     reduction                          name 
#>                       "cells"                  "custom_pca" 
#>                  feats_to_use                return_gobject 
#>            "my_spatial_genes"                        "TRUE" 
#>                        center                    scale_unit 
#>                        "TRUE"                        "TRUE" 
#>                           ncp                        method 
#>                         "100"                       "irlba" 
#>                 method_params                           rev 
#> "BiocParallel::SerialParam()"                       "FALSE" 
#>                      set_seed                   seed_number 
#>                        "TRUE"                        "1234" 
#>                       verbose                           ... 
#>                        "TRUE"                            "" 
#> 
#> $`15_umap`
#>              gobject    expression_values            reduction 
#>        "mini_visium"         "normalized"              "cells" 
#> dim_reduction_to_use   dim_reduction_name    dimensions_to_use 
#>                "pca"         "custom_pca"               "1:20" 
#>                 name       return_gobject          n_neighbors 
#>        "custom_umap"               "TRUE"                 "40" 
#>         n_components             n_epochs             min_dist 
#>                  "2"                "400"               "0.01" 
#>            n_threads               spread             set_seed 
#>                 "NA"                  "5"               "TRUE" 
#>          seed_number              verbose      toplevel_params 
#>               "1234"               "TRUE"                  "2" 
#>                  ... 
#>                   "" 
#> 
#> $`16_nn_network`
#>              gobject                 type dim_reduction_to_use 
#>        "mini_visium"                "sNN"                "pca" 
#>   dim_reduction_name    dimensions_to_use    expression_values 
#>         "custom_pca"               "1:20"         "normalized" 
#>                 name       return_gobject                    k 
#>          "custom_NN"               "TRUE"                  "5" 
#>       minimum_shared           top_shared              verbose 
#>                  "5"                  "3"               "TRUE" 
#>                  ... 
#>                   "" 
#> 
#> $`17_cluster`
#>                          gobject                             name 
#>                    "mini_visium"                  "custom_leiden" 
#>                nn_network_to_use                     network_name 
#>                            "sNN"                      "custom_NN" 
#>                       resolution                       weight_col 
#>                           "0.15"                         "weight" 
#>                   partition_type                     n_iterations 
#> "RBConfigurationVertexPartition"                           "1000" 
#>                   return_gobject                         set_seed 
#>                           "TRUE"                           "TRUE" 
#>                      seed_number 
#>                           "1234" 
#> 
#> $`18_spatial_deconvolution`
#>              method used       deconvolution name        expression values 
#>                   "DWLS"                   "DWLS"             "normalized" 
#>                  logbase      cluster column used number of cells per spot 
#>                      "2"            "leiden_clus"                     "50" 
#>             used cut off 
#>                      "2" 
#> 
#> $`19_spatial_deconvolution`
#>              method used       deconvolution name        expression values 
#>                   "DWLS"                   "DWLS"             "normalized" 
#>                  logbase      cluster column used number of cells per spot 
#>                      "2"            "leiden_clus"                     "50" 
#>             used cut off 
#>                      "2" 
#> 
#> $`20_spatial_deconvolution`
#>              method used       deconvolution name        expression values 
#>                   "DWLS"                   "DWLS"             "normalized" 
#>                  logbase      cluster column used number of cells per spot 
#>                      "2"            "leiden_clus"                     "50" 
#>             used cut off 
#>                      "2" 
#> 
objHistory(g, summarized = TRUE)
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
#> $`0_normalize`
#>           gobject expression_values      norm_methods library_size_norm 
#>     "mini_visium"             "raw"        "standard"            "TRUE" 
#>       scalefactor          log_norm        log_offset           logbase 
#>            "6000"            "TRUE"               "1"               "2" 
#>       scale_feats       scale_cells       scale_order             theta 
#>            "TRUE"            "TRUE"     "first_feats"             "100" 
#>       update_slot           verbose 
#>          "scaled"               "T" 
#> 
#> $`1_subset`
#>             gobject           spat_unit           feat_type            cell_ids 
#>           "gobject"         "spat_unit"         "feat_type" "selected_cell_ids" 
#>            feat_ids           poly_info      spat_unit_fsub      feat_type_ssub 
#> "selected_feat_ids"         "poly_info"    "spat_unit_fsub"    "feat_type_ssub" 
#>             verbose     toplevel_params 
#>           "verbose"                 "2" 
#> 
#> $`2_filter`
#>                gobject      expression_values   expression_threshold 
#>          "mini_visium"                  "raw"                    "1" 
#>  feat_det_in_min_cells min_det_feats_per_cell         spat_unit_fsub 
#>                    "5"                   "20"                ":all:" 
#>         feat_type_ssub              tag_cells          tag_cell_name 
#>                ":all:"                "FALSE"                  "tag" 
#>              tag_feats         tag_feats_name                verbose 
#>                "FALSE"                  "tag"                    "T" 
#> 
#> $`3_feat_stats`
#>             gobject   expression_values detection_threshold      return_gobject 
#>       "mini_visium"        "normalized"                 "0"              "TRUE" 
#>             verbose 
#>              "TRUE" 
#> 
#> $`4_cell_stats`
#>             gobject   expression_values detection_threshold      return_gobject 
#>       "mini_visium"        "normalized"                 "0"              "TRUE" 
#>             verbose 
#>              "TRUE" 
#> 
#> $`5_hvf`
#>              gobject    expression_values               method 
#>        "mini_visium"         "normalized"         "cov_groups" 
#>    reverse_log_scale              logbase expression_threshold 
#>              "FALSE"                  "2"                  "0" 
#> nr_expression_groups     zscore_threshold              HVFname 
#>                 "20"                "1.5"                "hvf" 
#>    difference_in_cov        var_threshold             set_seed 
#>                "0.1"                "1.5"               "TRUE" 
#>          seed_number           save_param    default_save_name 
#>               "1234"             "list()"            "HVFplot" 
#>       return_gobject              verbose 
#>               "TRUE"               "TRUE" 
#> 
#> $`6_pca`
#>                       gobject             expression_values 
#>                 "mini_visium"                  "normalized" 
#>                     reduction                return_gobject 
#>                       "cells"                        "TRUE" 
#>                        center                    scale_unit 
#>                        "TRUE"                        "TRUE" 
#>                           ncp                        method 
#>                         "100"                       "irlba" 
#>                 method_params                           rev 
#> "BiocParallel::SerialParam()"                       "FALSE" 
#>                      set_seed                   seed_number 
#>                        "TRUE"                        "1234" 
#>                       verbose                           ... 
#>                        "TRUE"                            "" 
#> 
#> $`7_umap`
#>              gobject    expression_values            reduction 
#>        "mini_visium"         "normalized"              "cells" 
#> dim_reduction_to_use    dimensions_to_use       return_gobject 
#>                "pca"               "1:10"               "TRUE" 
#>          n_neighbors         n_components             n_epochs 
#>                 "40"                  "2"                "400" 
#>             min_dist            n_threads               spread 
#>               "0.01"                 "NA"                  "5" 
#>             set_seed          seed_number              verbose 
#>               "TRUE"               "1234"               "TRUE" 
#>      toplevel_params                  ... 
#>                  "2"                   "" 
#> 
#> $`8_tsne`
#>              gobject    expression_values            reduction 
#>        "mini_visium"         "normalized"              "cells" 
#> dim_reduction_to_use    dimensions_to_use       return_gobject 
#>                "pca"               "1:10"               "TRUE" 
#>                 dims           perplexity                theta 
#>                  "2"                 "30"                "0.5" 
#>         do_PCA_first             set_seed          seed_number 
#>              "FALSE"               "TRUE"               "1234" 
#>              verbose                  ... 
#>               "TRUE"                   "" 
#> 
#> $`9_nn_network`
#>              gobject                 type dim_reduction_to_use 
#>        "mini_visium"                "sNN"                "pca" 
#>    dimensions_to_use    expression_values       return_gobject 
#>                "1:5"         "normalized"               "TRUE" 
#>                    k       minimum_shared           top_shared 
#>                 "10"                  "5"                  "3" 
#>              verbose                  ... 
#>               "TRUE"                   "" 
#> 
#> $`10_cluster`
#>                          gobject                             name 
#>                    "mini_visium"                    "leiden_clus" 
#>                nn_network_to_use                     network_name 
#>                            "sNN"                        "sNN.pca" 
#>                       resolution                       weight_col 
#>                            "0.1"                         "weight" 
#>                   partition_type                     n_iterations 
#> "RBConfigurationVertexPartition"                           "1000" 
#>                   return_gobject                         set_seed 
#>                           "TRUE"                           "TRUE" 
#>                      seed_number 
#>                           "1234" 
#> 
#> $`11_delaunay_spatial_network`
#>               dimensions used                        method 
#> "dimensions: sdimx and sdimy"                      "deldir" 
#>    maximum distance threshold       name of spatial network 
#>                        "auto"            "Delaunay_network" 
#> 
#> $`12_spatial_network`
#>               k neighbours            dimensions used 
#>                       "10"                      "all" 
#> maximum distance threshold    name of spatial network 
#>                      "400"          "spatial_network" 
#> 
#> $`13_create_metafeat`
#>            gobject  expression_values      feat_clusters               stat 
#>      "mini_visium"       "normalized"    "cluster_genes"             "mean" 
#>               name     return_gobject 
#> "cluster_metagene"             "TRUE" 
#> 
#> $`14_pca`
#>                       gobject             expression_values 
#>                 "mini_visium"                  "normalized" 
#>                     reduction                          name 
#>                       "cells"                  "custom_pca" 
#>                  feats_to_use                return_gobject 
#>            "my_spatial_genes"                        "TRUE" 
#>                        center                    scale_unit 
#>                        "TRUE"                        "TRUE" 
#>                           ncp                        method 
#>                         "100"                       "irlba" 
#>                 method_params                           rev 
#> "BiocParallel::SerialParam()"                       "FALSE" 
#>                      set_seed                   seed_number 
#>                        "TRUE"                        "1234" 
#>                       verbose                           ... 
#>                        "TRUE"                            "" 
#> 
#> $`15_umap`
#>              gobject    expression_values            reduction 
#>        "mini_visium"         "normalized"              "cells" 
#> dim_reduction_to_use   dim_reduction_name    dimensions_to_use 
#>                "pca"         "custom_pca"               "1:20" 
#>                 name       return_gobject          n_neighbors 
#>        "custom_umap"               "TRUE"                 "40" 
#>         n_components             n_epochs             min_dist 
#>                  "2"                "400"               "0.01" 
#>            n_threads               spread             set_seed 
#>                 "NA"                  "5"               "TRUE" 
#>          seed_number              verbose      toplevel_params 
#>               "1234"               "TRUE"                  "2" 
#>                  ... 
#>                   "" 
#> 
#> $`16_nn_network`
#>              gobject                 type dim_reduction_to_use 
#>        "mini_visium"                "sNN"                "pca" 
#>   dim_reduction_name    dimensions_to_use    expression_values 
#>         "custom_pca"               "1:20"         "normalized" 
#>                 name       return_gobject                    k 
#>          "custom_NN"               "TRUE"                  "5" 
#>       minimum_shared           top_shared              verbose 
#>                  "5"                  "3"               "TRUE" 
#>                  ... 
#>                   "" 
#> 
#> $`17_cluster`
#>                          gobject                             name 
#>                    "mini_visium"                  "custom_leiden" 
#>                nn_network_to_use                     network_name 
#>                            "sNN"                      "custom_NN" 
#>                       resolution                       weight_col 
#>                           "0.15"                         "weight" 
#>                   partition_type                     n_iterations 
#> "RBConfigurationVertexPartition"                           "1000" 
#>                   return_gobject                         set_seed 
#>                           "TRUE"                           "TRUE" 
#>                      seed_number 
#>                           "1234" 
#> 
#> $`18_spatial_deconvolution`
#>              method used       deconvolution name        expression values 
#>                   "DWLS"                   "DWLS"             "normalized" 
#>                  logbase      cluster column used number of cells per spot 
#>                      "2"            "leiden_clus"                     "50" 
#>             used cut off 
#>                      "2" 
#> 
#> $`19_spatial_deconvolution`
#>              method used       deconvolution name        expression values 
#>                   "DWLS"                   "DWLS"             "normalized" 
#>                  logbase      cluster column used number of cells per spot 
#>                      "2"            "leiden_clus"                     "50" 
#>             used cut off 
#>                      "2" 
#> 
#> $`20_spatial_deconvolution`
#>              method used       deconvolution name        expression values 
#>                   "DWLS"                   "DWLS"             "normalized" 
#>                  logbase      cluster column used number of cells per spot 
#>                      "2"            "leiden_clus"                     "50" 
#>             used cut off 
#>                      "2" 
#> 
```
