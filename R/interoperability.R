## sp classes ####


## anndata object ####


# Giotto -> .h5ad through python anndata (reticulate); used by
# giottoToSpatialData(). GiottoConnect::giottoToAnnData() writes .h5ad files
# without python.
.giotto_to_anndata_py <- function(
        gobject = NULL,
        spat_unit = NULL,
        feat_type = NULL,
        python_path = NULL,
        env_name = "giotto_env",
        save_directory = NULL) {
    # Check gobject
    invalid_obj <- !inherits(gobject, "giotto")
    if (is.null(gobject) || invalid_obj) {
        stop(wrap_msg("Please provide a valid Giotto Object for conversion."))
    }

    package_check(
        pkg_name = c("anndata", "scanpy"),
        repository = c("pip:anndata", "pip:scanpy")
    )

    # Python module import
    g2ad_path <- system.file("python", "g2ad.py", package = "GiottoClass")
    reticulate::source_python(g2ad_path)
    if (!is.null(save_directory)) {
        save_directory <- paste0(save_directory, "/")
    }

    # Check directory, make it if it doesn't exist
    if (is.null(save_directory)) {
        save_directory <- paste0(getwd(), "/")
    } else if (!dir.exists(save_directory)) {
        warning(wrap_msg("Provided save directory not found. Creating save
                        directory at location:"))
        cat(save_directory)
        dir.create(save_directory, recursive = TRUE)
        if (dir.exists(save_directory)) {
            cat("Created directory", save_directory)
        } else {
            stop(wrap_msg("Unable to create directory. Please change the
                        provided path and try again."))
        }
    } else {
        wrap_msg(
            "Directory", save_directory,
            "found. The converted Giotto object will be saved here as an
                .h5ad file."
        )
    }

    # set up giotto meta folder
    gmdir <- file.path(save_directory, "giotto_meta")
    if (!dir.exists(gmdir)) dir.create(gmdir)

    # Expresion
    expr_dt <- list_expression(gobject)
    # ID spat_unit and feat_type if not already provided.
    if (is.null(spat_unit) && is.null(feat_type)) {
        spat_unit <- unique(expr_dt$spat_unit)
        feat_type <- unique(expr_dt$feat_type)
    } else if (is.null(spat_unit) && !is.null(feat_type)) {
        spat_unit <- unique(expr_dt$spat_unit)
    } else if (!is.null(spat_unit) && is.null(feat_type)) {
        feat_type <- unique(expr_dt$feat_type)
    }

    for (su in spat_unit) {
        wrap_msg("Spatial unit(s)", su, "will be used in conversion.")
    }
    for (ft in feat_type) {
        wrap_msg("Feature type(s)", ft, "will be used in conversion.")
    }

    # Iterate through spat_unit and feat_type to pull out expression data.
    # By default, the raw expression in the slot of the first spatial unit
    # and first feature type (if multiple are present) will be transferred to
    # the AnnData.anndata.X slot
    # Any other expression data will be inserted into AnnData.anndata.layers
    # By default, layer names are formed by
    # "'spatial_unit'_'feature_type'_'value'"

    adata <- NULL # scope
    su_ft_length <- 0

    for (su in spat_unit) {
        for (ft in names(gobject@expression[[su]])) {
            su_ft_length <- su_ft_length + 1
        }
    }


    adata_list <- lapply(seq_len(su_ft_length), function(i) adata)
    adata_pos <- 1

    for (su in spat_unit) {
        for (ft in names(gobject@expression[[su]])) {
            expr_names <- list_expression_names(
                gobject = gobject,
                spat_unit = su,
                feat_type = ft
            )

            for (en in expr_names) {
                if (en == "raw") {
                    raw_x <- getExpression(
                        gobject = gobject,
                        values = en,
                        spat_unit = su,
                        feat_type = ft,
                        output = "matrix"
                    )

                    adata <- ad_obj(x = raw_x)
                } else {
                    ad_layer_name <- paste0(su, "_", ft, "_", en)

                    x <- getExpression(
                        gobject = gobject,
                        values = en,
                        spat_unit = su,
                        feat_type = ft,
                        output = "matrix"
                    )

                    if ("dgeMatrix" %in% class(x)) x <- methods::as(x, "array")

                    adata <- set_adg_layer_data(
                        adata = adata,
                        lay = x,
                        lay_name = ad_layer_name
                    )
                }
            }
            adata_list[[adata_pos]] <- adata
            adata_pos <- adata_pos + 1
            adata <- NULL
        }
    }
    # Reset indexing variable
    adata_pos <- 1

    # Spatial Locations
    for (su in spat_unit) {
        for (ft_ in names(gobject@expression[[su]])) {
            sl <- getSpatialLocations(
                gobject = gobject,
                output = "data.table",
                spat_unit = su
            )
            n_col_sl <- dim(sl)[2]

            # preallocate data.table params
            sdimx <- sdimy <- sdimz <- NULL

            if (n_col_sl == 3) {
                sl <- sl[, .(sdimx, sdimy)]
            } else {
                sl <- sl[, .(sdimx, sdimy, sdimz)]
            }
            adata <- adata_list[[adata_pos]]
            adata <- set_adg_spat_locs(
                adata = adata,
                spat_locs = sl
            )
            adata_pos <- adata_pos + 1
        }
    }
    # Reset indexing variable
    adata_pos <- 1

    # Spatial Info

    # Cell Metadata
    # Feat Metadata
    for (su in spat_unit) {
        for (ft in names(gobject@expression[[su]])) {
            cmeta <- getCellMetadata(
                gobject = gobject,
                spat_unit = su,
                feat_type = ft,
                output = "data.table",
                set_defaults = FALSE
            )

            fm <- getFeatureMetadata(
                gobject = gobject,
                spat_unit = su,
                feat_type = ft,
                output = "data.table",
                set_defaults = FALSE
            )

            adata_list[[adata_pos]] <- set_adg_metadata(
                adata = adata_list[[adata_pos]],
                cell_meta = cmeta,
                feat_meta = fm
            )

            adata_pos <- adata_pos + 1
        }
    }
    # Reset indexing variable
    adata_pos <- 1

    # Dimension Reductions

    # error hanldling wrapper to get_dimReduction
    try_get_dimReduction <- function(
        gobject,
        spat_unit,
        feat_type,
        reduction,
        reduction_method,
        name,
        output,
        set_defaults) {
        tryCatch(
            {
                dim_red <- getDimReduction(
                    gobject = gobject,
                    spat_unit = spat_unit,
                    feat_type = feat_type,
                    reduction = reduction,
                    reduction_method = reduction_method,
                    name = name,
                    output = output,
                    set_defaults = set_defaults
                )
                return(dim_red)
            },
            error = function(e) {
                return(NULL)
            }
        )
    }

    ## PCA

    # pca on feats not supported by anndata because of dimensionality
    # agreement reqs
    reduction_list <- list_giotto_data(gobject, "dimension_reduction")
    dim_red <- NULL
    if (!is.null(reduction_list) && is.data.frame(reduction_list)) {
        for (i in seq_len(nrow(reduction_list))) {
            ro <- reduction_list[[i, 1]]
            su <- reduction_list[[i, 2]]
            ft <- reduction_list[[i, 3]]
            method <- reduction_list[[i, 4]]
            name <- reduction_list[[i, 5]]

            if (ro != "cells") {
                warning("AnnData does not support storing PCA by features.
                        Skipping PCA data conversion.")
                break
            }

            if (ft != "rna") name <- paste0(ft, ".pca")
            dim_red <- try_getDimReduction(
                gobject = gobject,
                spat_unit = su,
                feat_type = ft,
                reduction = ro,
                reduction_method = method,
                name = name,
                output = "dimObj",
                set_defaults = FALSE
            )

            if (is.null(dim_red)) {
                adata_pos <- adata_pos + 1
                next
            }

            if (method == "pca") {
                pca_coord <- dim_red[]
                pca_loadings <- data.table(dim_red@misc$loadings)
                feats_used <- dimnames(dim_red@misc$loadings)[[1]]
                evs <- dim_red@misc$eigenvalues

                adata_list[[adata_pos]] <- set_adg_pca(
                    adata = adata_list[[adata_pos]],
                    pca_coord = pca_coord,
                    loadings = pca_loadings,
                    eigenv = evs,
                    feats_used = feats_used,
                    pca_name = name
                )
                wrap_msg(sprintf("\n%s converted.\n", name))
                adata_pos <- adata_pos + 1
            }

            if (method == "umap") {
                umap_data <- dim_red[]
                adata_list[[adata_pos]] <- set_adg_umap(
                    adata = adata_list[[adata_pos]],
                    umap_data = umap_data,
                    umap_name = name
                )
                wrap_msg(sprintf("\n%s converted.\n", name))
                adata_pos <- adata_pos + 1
            }

            if (method == "tsne") {
                tsne_data <- dim_red[]
                adata_list[[adata_pos]] <- set_adg_tsne(
                    adata = adata_list[[adata_pos]],
                    tsne_data = tsne_data,
                    tsne_name = name
                )
                wrap_msg(sprintf("\n%s converted.\n", name))
                adata_pos <- adata_pos + 1
            }
            adata_pos <- 1
        }
    }


    # Nearest Neighbor Network

    # error handling wrapper to getNearestNetwork
    try_get_NN <- function(
        gobject,
        spat_unit,
        feat_type,
        nn_network_to_use,
        network_name,
        output,
        set_defaults) {
        tryCatch(
            {
                nearest_net <- getNearestNetwork(
                    gobject = gobject,
                    spat_unit = spat_unit,
                    feat_type = feat_type,
                    nn_type = nn_network_to_use,
                    name = network_name,
                    output = output,
                    set_defaults = set_defaults
                )
                return(nearest_net)
            },
            error = function(e) {
                return(NULL)
            }
        )
    }

    for (su in spat_unit) {
        for (ft in names(gobject@expression[[su]])) {
            nn_network_to_use <- c("sNN", "kNN")
            for (nn_net_tu in nn_network_to_use) {
                network_name <- list_nearest_networks_names(
                    gobject = gobject,
                    spat_unit = su,
                    feat_type = ft,
                    nn_type = nn_net_tu
                )
                if (is.null(network_name)) {
                    next
                }
                for (n_name in network_name) {
                    gob_NN <- try_get_NN(
                        gobject = gobject,
                        spat_unit = su,
                        feat_type = ft,
                        nn_network_to_use = nn_net_tu,
                        network_name = n_name,
                        output = "nnNetObj",
                        set_defaults = FALSE
                    )

                    pidx <- grep("nn_network", names(gobject@parameters))
                    for (p in pidx) {
                        if (gobject@parameters[[p]]["type"] == nn_net_tu) {
                            kval <- gobject@parameters[[p]]["k"]
                            dim_red_used <- gobject@parameters[[p]][
                                "dim_red_to_use"
                            ]
                        }
                    }

                    df_gob_NN <- igraph::as_data_frame(gob_NN[])

                    adata_list[[adata_pos]] <- set_adg_nn(
                        adata = adata_list[[adata_pos]],
                        df_NN = df_gob_NN,
                        net_name = n_name,
                        n_neighbors = kval,
                        dim_red_used = dim_red_used
                    )
                }

                # Save NN keys to .uns['NN_keys']
                network_name <- network_name[!grepl("kNN.", network_name)]
                save_NN_keys(adata = adata_list[[adata_pos]], network_name = network_name)
            }
            adata_pos <- adata_pos + 1
        }
    }

    # Reset indexing variable
    adata_pos <- 1

    try_get_SN <- function(
        gobject,
        spat_unit,
        name,
        output,
        set_defaults,
        verbose) {
        tryCatch(
            {
                spatial_net <- getSpatialNetwork(
                    gobject = gobject,
                    spat_unit = spat_unit,
                    name = name,
                    output = output,
                    set_defaults = set_defaults,
                    verbose = verbose
                )
                return(spatial_net)
            },
            error = function(e) {
                return(NULL)
            }
        )
    }

    for (su in spat_unit) {
        for (ft in names(gobject@expression[[su]])) {
            # Spatial networks do not have a feature type slot.
            # Iterate through anyways to properly assign to anndata objects
            network_name <- list_spatial_networks_names(
                gobject = gobject,
                spat_unit = su
            )
            if (is.null(network_name)) {
                next
            }
            for (sn_name in network_name) {
                gob_SN <- try_get_SN(
                    gobject = gobject,
                    spat_unit = su,
                    name = sn_name,
                    output = "networkDT",
                    set_defaults = FALSE,
                    verbose = TRUE
                )

                pidx <- grep("spatial_network", names(gobject@parameters))
                for (p in pidx) {
                    if (gobject@parameters[[p]]["name of spatial network"] == sn_name) {
                        current_param <- gobject@parameters[[p]]
                        kval <- current_param["k neighbours"] # null in the case of Delaunay
                        maxdist <- current_param["maximum distance threshold"] # "auto" for Delaunay, "400 (string)" for sNN
                        dimused <- current_param["dimensions used"]
                    }
                }

                adata_list[[adata_pos]] <- set_adg_sn(
                    adata = adata_list[[adata_pos]],
                    df_SN = gob_SN,
                    net_name = sn_name,
                    n_neighbors = kval,
                    max_distance = maxdist,
                    dim_used = dimused
                )
            }

            # Save SN keys to .uns['SN_keys']
            if (length(network_name) != 0) {
                save_SN_keys(
                    adata = adata_list[[adata_pos]],
                    network_name = network_name
                )
            }
        }

        adata_pos <- adata_pos + 1
    }

    # Reset indexing variable
    adata_pos <- 1

    # Spatial Enrichment
    spat_enrich_list <- list_giotto_data(
        gobject = gobject,
        slot = "spatial_enrichment"
    )

    if (!is.null(spat_enrich_list) && is.data.frame(spat_enrich_list)) {
        for (i in seq_len(nrow(spat_enrich_list))) {
            se_su <- spat_enrich_list[i]$spat_unit
            se_ft <- spat_enrich_list[i]$feat_type
            se_name <- spat_enrich_list[i]$name

            se <- getSpatialEnrichment(
                gobject = gobject,
                spat_unit = se_su,
                feat_type = se_ft,
                name = se_name,
                output = "data.table"
            )
            adata_list[[adata_pos]] <- set_adg_spat_enrich(
                adata = adata_list[[adata_pos]],
                enrichment = se,
                name = se_name
            )
        }
        save_SE_keys(
            adata = adata_list[[adata_pos]],
            enrichment_name = spat_enrich_list$name
        )
    }

    # Write AnnData object to .h5ad file
    # Verify it exists, and return upon success
    fname_list <- lapply(seq_len(su_ft_length), function(i) NULL)
    wrap_msg("\n")
    for (su in spat_unit) {
        for (ft in names(gobject@expression[[su]])) {
            adata <- adata_list[[adata_pos]]
            path_adata <- write_ad_h5ad(
                adata = adata,
                save_directory = save_directory,
                spat_unit = su,
                feat_type = ft
            )
            if (!is.null(path_adata)) {
                wrap_msg(
                    "Spatial unit", su, "and feature type",
                    ft, "converted to:"
                )
                wrap_msg(path_adata)
                fname_list[[adata_pos]] <- path_adata
                adata_pos <- adata_pos + 1
            } else {
                wrap_msg(
                    "Unable to convert spatial unit feature type pair",
                    su, ft
                )
                stop(wrap_msg("File writing error. Please try again."))
            }
        }
    }

    wrap_msg("\nGiotto object successfully converted to .h5ad file(s)\n")

    return(fname_list)
}


# SpatialExperiment methods ####

# register methods if SpatialExperiment is present
if (requireNamespace("SpatialExperiment", quietly = TRUE)) {
    setMethod(
        SingleCellExperiment::colData,
        signature("giotto"),
        function(x, spat_unit = NULL, feat_type = NULL, ...) {
            pDataDT(
                gobject = x,
                spat_unit = spat_unit,
                feat_type = feat_type, ...
            ) %>%
                S4Vectors::DataFrame()
        }
    )

    setMethod(
        # TODO generate DataFrame of ENSG and gene symbols
        SingleCellExperiment::rowData,
        signature("giotto"),
        function(x, spat_unit = NULL, feat_type = NULL, ...) {
            fDataDT(
                gobject = x,
                spat_unit = spat_unit,
                feat_type = feat_type, ...
            ) %>%
                S4Vectors::DataFrame()
        }
    )
}


# older Giotto versions ####

#' Convert a master Giotto object to suite
#'
#' @param gobject A Giotto object created with master version
#' @param expression_feat available features (e.g. rna, protein, ...)
#'
#' @returns A Giotto object compatible with suite version
#' @export
#'
giottoMasterToSuite <- function(
        gobject,
        expression_feat = "rna") {
    master_object <- gobject

    spatial_locs <- cell_metadata <- feat_metadata <- instructions <- NULL

    if (!is.null(master_object@spatial_locs)) {
        spatial_locs <- master_object@spatial_locs
    }

    if (!is.null(master_object@cell_metadata)) {
        cell_metadata <- master_object@cell_metadata
    }

    if (!is.null(master_object@gene_metadata)) {
        feat_metadata <- master_object@gene_metadata
        colnames(feat_metadata)[1] <- "feat_ID"
    }

    # instructions
    if (!is.null(master_object@instructions)) {
        instructions <- master_object@instructions
    }

    # create Giotto object
    gobject <- createGiottoObject(
        expression = master_object@raw_exprs,
        expression_feat = expression_feat,
        spatial_locs = spatial_locs,
        cell_metadata = cell_metadata,
        feat_metadata = feat_metadata,
        instructions = instructions
    )

    # add normalized expression matrix
    if (!is.null(master_object@norm_expr)) {
        x <- createExprObj(master_object@norm_expr,
            name = "normalized",
            feat_type = expression_feat
        )
        gobject <- setExpression(gobject,
            x = x,
            spat_unit = "cell",
            feat_type = expression_feat,
            name = "normalized"
        )
    }

    # add scaled expression matrix
    if (!is.null(master_object@norm_scaled_expr)) {
        x <- createExprObj(master_object@norm_scaled_expr,
            name = "scaled",
            feat_type = expression_feat
        )
        gobject <- setExpression(gobject,
            x = x,
            spat_unit = "cell",
            feat_type = expression_feat,
            name = "scaled"
        )
    }

    # dimension_reduction
    if (!is.null(master_object@dimension_reduction)) {
        dimension_reduction_master <- master_object@dimension_reduction

        for (i in names(master_object@dimension_reduction$cells)) {
            j <- names(dimension_reduction_master$cells[[i]])
            dimension_reduction <- createDimObj(
                coordinates = dimension_reduction_master$cells[[i]][[
                    j
                ]]$coordinates,
                name = dimension_reduction_master$cells[[i]][[j]]$name,
                feat_type = expression_feat,
                method = dimension_reduction_master$cells[[i]][[
                    j
                ]]$reduction_method,
                misc = dimension_reduction_master$cells[[i]][[j]]$misc
            )
            gobject <- setDimReduction(gobject,
                dimension_reduction,
                spat_unit = "cell",
                feat_type = expression_feat,
                name = i,
                reduction_method = dimension_reduction_master$cells[[i]][[
                    j
                ]]$reduction_method
            )
        }
    }

    # nn_network
    if (!is.null(master_object@nn_network)) {
        nn_network_master <- master_object@nn_network

        for (i in names(nn_network_master)) {
            for (j in names(nn_network_master[[i]])) {
                k <- names(nn_network_master[[i]][[j]])
                nn_network <- createNearestNetObj(
                    name = i,
                    network = nn_network_master[[i]][[j]][[k]],
                    feat_type = expression_feat
                )

                gobject <- setNearestNetwork(gobject,
                    nn_network,
                    spat_unit = "cell",
                    feat_type = expression_feat,
                    nn_type = i,
                    name = j
                )
            }
        }
    }

    # spatial_network
    if (!is.null(master_object@spatial_network)) {
        spatial_network_master <- master_object@spatial_network$spatial_network
        spatial_network <- createSpatNetObj(
            network = spatial_network_master$networkDT,
            name = spatial_network_master$name,
            networkDT_before_filter =
                spatial_network_master$networkDT_before_filter,
            method = spatial_network_master$method,
            parameters = spatial_network_master$parameters,
            outputObj = spatial_network_master$outputObj,
            cellShapeObj = spatial_network_master$cellShapeObj,
            crossSectionObjects = spatial_network_master$crossSectionObjects,
            misc = spatial_network_master$misc
        )
        gobject <- setSpatialNetwork(gobject,
            spatial_network,
            spat_unit = "cell",
            name = spatial_network_master$name
        )
    }

    # spatial_enrichment
    if (!is.null(master_object@spatial_enrichment)) {
        spatial_enrichment_master <- master_object@spatial_enrichment

        for (i in names(spatial_enrichment_master)) {
            spatial_enrichment <- createSpatEnrObj(
                spatial_enrichment_master[[i]],
                name = i,
                feat_type = expression_feat,
                method = i
            )
        }

        gobject <- setSpatialEnrichment(gobject,
            x = spatial_enrichment,
            spat_unit = "cell",
            feat_type = expression_feat,
            name = i,
            verbose = FALSE,
            initialize = FALSE
        )
    }

    # spatial_grid
    if (!is.null(master_object@spatial_grid)) {
        spatial_grid_master <- master_object@spatial_grid

        spatial_grid <- new("spatialGridObj",
            name = spatial_grid_master$spatial_grid$name,
            method = spatial_grid_master$spatial_grid$method,
            parameters = spatial_grid_master$spatial_grid$parameters,
            gridDT = spatial_grid_master$spatial_grid$gridDT,
            feat_type = expression_feat,
            misc = spatial_grid_master$spatial_grid$misc
        )

        gobject <- setSpatialGrid(gobject,
            spatial_grid,
            spat_unit = "cell",
            feat_type = expression_feat,
            name = spatial_grid_master$spatial_grid$name
        )
    }

    return(gobject)
}


##  Giotto to SpatialData####

#' @title Convert Giotto to SpatialData
#' @name giottoToSpatialData
#' @description Converts a Giotto object to SpatialData object
#'
#' @param gobject giotto object to be converted
#' @param spat_unit spatial unit which will be used in conversion
#' @param feat_type feature type which will be used in conversion
#' @param spot_radius radius of the spots
#' @param python_path path to python executable within a conda/miniconda
#' environment
#' @param env_name name of environment containing python_path executable
#' @param save_directory directory in which the SpatialData object will be saved
#'
#' @return SpatialData object saved on disk.
#' @details Function in beta. Converts and saves a Giotto object in SpatialData
#' format on disk.
#' @export

giottoToSpatialData <- function(
        gobject = NULL,
        spat_unit = NULL,
        feat_type = NULL,
        spot_radius = NULL,
        python_path = NULL,
        env_name = NULL,
        save_directory = NULL) {
    # Initialize reticulate
    instrs <- createGiottoInstructions(python_path = python_path)

    if (dir.exists(save_directory)) {
        stop("save_directory must be a non-existing directory")
    }

    # Check spatialdata dependencies
    package_check(
        pkg_name = "spatialdata",
        repository = "pip:spatialdata"
    )

    # Import sd2g, a python module for parsing SpatialData
    g2sd_path <- system.file("python", "g2sd.py", package = "GiottoClass")
    reticulate::source_python(g2sd_path)

    spat_unit <- spat_unit %null% spatUnit(gobject)
    feat_type <- feat_type %null% featType(gobject)

    for (su in spat_unit) {
        wrap_msg("Spatial unit(s)", su, "will be used in conversion.")
    }
    for (ft in feat_type) {
        wrap_msg("Feature type(s)", ft, "will be used in conversion.")
    }

    # Create a temporary folder to hold anndata
    temp <- "temp_conversion_files/"
    # Delete temporary files and folders when done
    on.exit(
        {
            if (dir.exists(temp)) unlink(temp, recursive = TRUE)
        },
        add = TRUE
    )


    # First, convert Giotto object to AnnData using an existing function
    .giotto_to_anndata_py(
        gobject = gobject,
        spat_unit = spat_unit,
        feat_type = feat_type,
        python_path = python_path,
        env_name = env_name,
        save_directory = temp
    )

    # Extract GiottoImage only if an image exists
    images_exist <- NULL
    if (length(slot(gobject, "images")) > 0) {
        images_exist <- TRUE
        gimg_list <- slot(gobject, "images")
        for (i in seq_along(gimg_list)) {
            img_name <- slot(gimg_list[[i]], "name")
            writeGiottoLargeImage(
                giottoLargeImage = gimg_list[[i]],
                gobject = gobject,
                largeImage_name = img_name,
                filename = paste0(temp, img_name, ".png"),
                dataType = NULL,
                max_intensity = NULL,
                overwrite = TRUE,
                verbose = TRUE
            )
        }
    }

    # Extract polygons
    if (!is.null(list_spatial_info(gobject))) {
        dir.create(paste0(temp, "shapes"))
        for (su in spat_unit) {
            gpoly <- getPolygonInfo(gobject, name = su)
            gpoly_sf <- as.sf(gpoly)
            sf::st_write(gpoly_sf, paste0(temp, "shapes/", su, ".geojson"),
                delete_dsn = TRUE
            )
        }
    }

    # Extract points
    if (!is.null(list_feature_info(gobject))) {
        dir.create(paste0(temp, "points"))
        for (ft in feat_type) {
            gpoint <- getFeatureInfo(gobject, feat_type = ft)
            gpoint_dt <- as.data.table(gpoint, geom = "XY")
            fwrite(gpoint_dt, paste0(temp, "points/", ft, ".csv"),
                sep = ",", row.names = FALSE
            )
        }
    }

    # Create SpatialData object
    createSpatialData(temp, save_directory, images_exist)

    # copy over giotto metadata
    dir.create(file.path(save_directory, "giotto_meta"))
    file.copy(
        from = file.path(temp, "giotto_meta"),
        to = save_directory,
        recursive = TRUE
    )

    # Successful Conversion
    cat(
        "Giotto object has been converted and saved to SpatialData object at: ",
        save_directory, "\n"
    )
}
