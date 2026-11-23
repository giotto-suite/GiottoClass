## sp classes ####


## gef object ####
#' @title Convert gef to Giotto
#' @name gefToGiotto
#' @description Converts .gef file (output stereo-seq pipeline) into
#' giotto subcellular object
#'
#' @param gef_file path to .gef file
#' @param bin_size bin size to select from .gef file
#' @param gene_column (optional) character. Which column contains gene names
#' within the geneExp information.
#' @param backend path or `gsource` to use as a managed backend for
#'   on-disk artifacts (passed to [createGiottoObject()]).
#' @param h5_file deprecated. Use `backend` instead.
#' @param verbose be verbose
#'
#' @details Function in beta. Converts .gef object to Giotto object.
#'
#' There are six possible choices for bin_size:
#' bin1, bin10, bin20, bin50, bin100, bin200.
#'
#' See SAW pipeline for additional information about the gef file.
#' @returns giotto object
#' @export

gefToGiotto <- function(gef_file,
    bin_size = "bin100",
    gene_column = NULL,
    verbose = FALSE,
    backend = NULL,
    h5_file = deprecated()) {
    if (is_present(h5_file)) backend <- h5_file
    # data.table vars
    genes <- gene_idx <- x <- y <- sdimx <- sdimy <- cell_ID <- bin_ID <-
        count <- i.bin_ID <- NULL

    # package check
    package_check(pkg_name = "rhdf5", repository = "Bioc")
    if (!file.exists(gef_file)) stop("File path to .gef file does not exist")

    # check if proper bin_size is selected. These are determined in SAW pipeline
    wrap_msg("1. gefToGiotto() begin... \n")
    bin_size_options <- c("bin1", "bin10", "bin20", "bin50", "bin100", "bin200")
    if (!(bin_size %in% bin_size_options)) {
        stop("Please select valid bin size, see ?gefToGiotto for details.")
    }

    # 1. read .gef file at specific bin size
    geneExpData <- rhdf5::h5read(
        file = gef_file,
        name = paste0("geneExp/", bin_size)
    )

    exprDT <- data.table::as.data.table(geneExpData[["expression"]])
    exprDT[, count := lapply(.SD, as.integer), .SDcols = "count"]
    data.table::setorder(exprDT, x, y) # sort by x, y coords (ascending)
    geneDT <- data.table::as.data.table(geneExpData[["gene"]])

    # gene_column selection
    name_to_replace <- gene_column %null% "gene"
    if (name_to_replace %in% colnames(geneDT)) {
        data.table::setnames(geneDT, old = name_to_replace, new = "geneName")
    }

    # process duplicated gene symbol
    if (any(duplicated(geneDT$geneName))) {
        duplicated_genes <- unique(geneDT$geneName[duplicated(geneDT$geneName)])
        cat("Ops!!! Duplicated_genes,processing:sum(count),mean(offset)")
        # merge
        for (gene in duplicated_genes) {
            # indices
            idx <- which(geneDT$geneName == gene)

            #
            cat("Processing gene:", gene, "\n")
            cat("Original count values for", gene, ":", geneDT$count[idx], "\n")

            # update
            geneDT$count[idx[1]] <- sum(geneDT$count[idx]) # 对重复的 count 求和
            geneDT$offset[idx[1]] <- mean(geneDT$offset[idx]) # 对重复的 offset 求平均

            #
            cat("Updated count for", gene, ":", geneDT$count[idx[1]], "\n")
            cat("Updated offset for", gene, ":", geneDT$offset[idx[1]], "\n")

            #
            geneDT <- geneDT[-idx[-1], ]

            #
            cat("Deleted duplicate entries for gene:", gene, "\n\n")
        }
    }

    if (isTRUE(verbose)) wrap_msg("finished reading in .gef", bin_size, "\n")

    # 2. create spatial locations
    if (isTRUE(verbose)) wrap_msg("2. create spatial_locations... \n")
    cell_locations <- unique(exprDT[, c("x", "y")], by = c("x", "y"))
    cell_locations[, bin_ID := as.factor(seq_len(nrow(cell_locations)))]
    cell_locations[, cell_ID := paste0("cell_", bin_ID)]
    data.table::setcolorder(cell_locations, c("x", "y", "cell_ID", "bin_ID"))
    # ensure first non-numerical col is cell_ID
    if (isTRUE(verbose)) wrap_msg(nrow(cell_locations), " bins in total \n")
    if (isTRUE(verbose)) wrap_msg("finished spatial_locations \n")

    # 3. create expression matrix
    if (isTRUE(verbose)) wrap_msg("3. create expression matrix... \n")
    exprDT[, genes := as.character(rep(x = geneDT$geneName, geneDT$count))]
    exprDT[, gene_idx := as.integer(factor(exprDT$genes,
        levels = unique(exprDT$genes)
    ))]

    # merge on x,y and populate based on bin_ID values in cell_locations
    exprDT[cell_locations, cell_ID := i.bin_ID, on = .(x, y)]
    exprDT$cell_ID <- as.integer(exprDT$cell_ID)

    expMatrix <- Matrix::sparseMatrix(
        i = exprDT$gene_idx,
        j = exprDT$cell_ID,
        x = exprDT$count
    )

    colnames(expMatrix) <- cell_locations$cell_ID
    rownames(expMatrix) <- geneDT$geneName
    rm(exprDT)
    if (isTRUE(verbose)) wrap_msg("finished expression matrix")

    # 4. create minimal giotto object
    if (isTRUE(verbose)) wrap_msg("4. create giotto object... \n")
    stereo <- createGiottoObject(
        expression = expMatrix,
        spatial_locs = cell_locations,
        verbose = FALSE,
        backend = backend
    )
    if (isTRUE(verbose)) wrap_msg("finished giotto object... \n")

    wrap_msg("gefToGiotto() finished \n")
    return(stereo)
}


## anndata object ####


#' @title Convert Giotto to anndata
#' @name giottoToAnnData
#' @description Converts a Giotto object to a spatial anndata (e.g. scanpy)
#' .h5ad file
#' @param gobject giotto object to be converted
#' @param spat_unit spatial unit which will be used in conversion.
#' @param feat_type feature type which will be used in conversion.
#' @param python_path path to python executable within a conda/miniconda
#' environment
#' @param env_name name of environment containing python_path executable
#' @param save_directory directory in which the file will be saved.
#' @details Function in beta. Converts a Giotto object into .h5ad file(s).
#'
#' If there are multiple spatial units and/or feature types, but only
#' one spatial unit and/or feature type is specified, then only the
#' specified spatial unit and/or feature type will be used. If NULL,
#' by default, all spatial units will be used in conversion.
#'
#' If multiple spatial units or feature types are specified, multiple
#' AnnData object will be created and returned.
#'
#' This function will create .txt files which will record any `key_added`
#' parameters for networks. They are named after the corresponding spatial unit
#' and feature type pair.
#'
#' The save_directory will be created if it does not already exist.
#' The default save_directory is the working directory.
#' @returns vector containing .h5ad file path(s)
#' @export
giottoToAnnData <- function(
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


## SpatialExperiment object ####

#' Utility function to convert a Giotto object to a SpatialExperiment object.
#'
#' @param gobject Input Giotto object to convert to a SpatialExperiment object
#' @param verbose A boolean value specifying if progress messages should be
#' displayed or not. Default \code{TRUE}.
#' @param giottoObj deprecated
#'
#' @returns A SpatialExperiment object that contains data from the input Giotto
#' object.
#' @examples
#' \dontrun{
#' mini_gobject <- GiottoData::loadGiottoMini("vizgen")
#' giottoToSpatialExperiment(mini_gobject)
#' }
#' @export
giottoToSpatialExperiment <- function(gobject,
    verbose = TRUE,
    giottoObj = deprecated()) {
    # deprecation
    gobject <- deprecate_param(giottoObj, gobject,
        fun = "giottoToSpatialExperiment",
        when = "0.4.6"
    )

    spat_unit <- NULL

    # Load required packages
    # package_check(pkg_name = "SummarizedExperiment", repository = 'Bioc')
    # SP should load this?
    # package_check(pkg_name = "SingleCellExperiment", repository = 'Bioc')
    # SP should load this?
    package_check(pkg_name = "SpatialExperiment", repository = "Bioc")
    package_check(pkg_name = "S4Vectors", repository = "Bioc")

    # SpatialExperiment objects list (one object for each spatial unit)
    speList <- list()

    # Expression Matrices
    giottoExpr <- list_expression(gobject)

    # Iterate over spatial units
    spatialUnits <- unique(giottoExpr$spat_unit) # a function to get spat units?
    for (su in seq(spatialUnits)) {
        if (verbose) {
            message("Processing spatial unit: '", spatialUnits[su], "'")
        }
        # Check if expression matrices exist in input object
        if (!is.null(giottoExpr)) {
            if (verbose) {
                message(
                    "Copying expression matrix: '", giottoExpr[1]$name,
                    "' for spatial unit: '", spatialUnits[su], "'"
                )
            }
            exprMat <- getExpression(
                gobject = gobject,
                spat_unit = spatialUnits[su],
                feat_type = giottoExpr[1]$feat_type,
                values = giottoExpr[1]$name,
                output = "matrix"
            )
            names(rownames(exprMat)) <- NULL
            names(colnames(exprMat)) <- NULL
            exprMat <- list(exprMat)
            names(exprMat)[1] <- giottoExpr[1]$name
            # Creating SPE object with first expression matrix
            spe <- SpatialExperiment::SpatialExperiment(assays = exprMat)
            SummarizedExperiment::assayNames(spe) <- paste0(
                giottoExpr[1]$name, "_",
                giottoExpr[1]$feat_type, "_",
                spatialUnits[su]
            )
            giottoExpr <- giottoExpr[-1, ]
        } else {
            stop("The input Giotto object must contain atleast one expression
                matrix.")
        }

        # Copying remaining expression matrices if they exist
        if (nrow(giottoExpr[spat_unit == spatialUnits[su]]) > 0) {
            for (i in seq(nrow(giottoExpr))) {
                if (verbose) {
                    message(
                        "Copying expression matrix: '",
                        giottoExpr[i]$name, "' for spatial unit: '",
                        spatialUnits[su], "'"
                    )
                }
                # SPE does not have specific slots for different units,
                # instead joining multiple unit names to identify them
                SummarizedExperiment::assay(
                    spe,
                    paste0(
                        giottoExpr[i]$name, "_",
                        giottoExpr[i]$feat_type, "_",
                        spatialUnits[su]
                    ),
                    withDimnames = FALSE
                ) <- getExpression(
                    gobject = gobject,
                    spat_unit = spatialUnits[su],
                    feat_type = giottoExpr[i]$feat_type,
                    values = giottoExpr[i]$name,
                    output = "matrix"
                )
            }
        }

        # Cell Metadata to ColData
        pData <- pDataDT(gobject = gobject, spat_unit = spatialUnits[su])
        if (nrow(pData) > 0) {
            if (verbose) {
                message(
                    "Copying phenotype data for spatial unit: '",
                    spatialUnits[su], "'"
                )
            }
            SummarizedExperiment::colData(spe) <- S4Vectors::DataFrame(
                pData,
                row.names = pData$cell_ID
            )
        } else {
            if (verbose) {
                message("No phenotype data found in input Giotto object")
            }
        }

        # Feature Metadata to RowData
        fData <- fDataDT(gobject = gobject, spat_unit = spatialUnits[su])
        if (nrow(fData) > 0) {
            if (verbose) {
                message(
                    "Copying feature metadata for spatial unit: '",
                    spatialUnits[su], "'"
                )
            }
            SummarizedExperiment::rowData(spe) <- fData
        } else {
            if (verbose) {
                message("No feature metadata found in input Giotto object")
            }
        }

        # Spatial Locations to Spatial Coordinates
        spatialLocs <- getSpatialLocations(
            gobject = gobject,
            spat_unit = spatialUnits[su],
            output = "data.table"
        )
        if (!is.null(spatialLocs)) {
            if (verbose) {
                message(
                    "Copying spatial locations for spatial unit: '",
                    spatialUnits[su], "'"
                )
            }
            if (all(colnames(spatialLocs[, seq_along(2)]) == c("sdimx", "sdimy"))) {
                spatialLocs <- spatialLocs[, c("sdimx", "sdimy"), drop = FALSE]
            } else {
                # Rename the first two columns to sdimx and sdimy
                colnames(spatialLocs)[seq_along(2)] <- c("sdimx", "sdimy")
                spatialLocs <- spatialLocs[, c("sdimx", "sdimy"), drop = FALSE]
            }
            SpatialExperiment::spatialCoords(spe) <- data.matrix(spatialLocs)
        } else {
            if (verbose) {
                message("No spatial locations found in the input Giotto object")
            }
        }

        # DimReductions
        giottoReductions <- list_dim_reductions(
            gobject = gobject,
            spat_unit = spatialUnits[su]
        )
        if (!is.null(giottoReductions)) {
            if (verbose) {
                message(
                    "Copying reduced dimensions for spatial unit: '",
                    spatialUnits[su], "'"
                )
            }
            for (i in seq(nrow(giottoReductions))) {
                SingleCellExperiment::reducedDim(
                    spe,
                    giottoReductions[i]$name
                ) <- getDimReduction(
                    gobject = gobject,
                    reduction = "cells",
                    spat_unit = spatialUnits[su],
                    feat_type = giottoReductions[i]$feat_type,
                    reduction_method = giottoReductions[i]$dim_type,
                    name = giottoReductions[i]$name,
                    output = "data.table"
                )
            }
        } else {
            if (verbose) {
                message("No reduced dimensions found in the input Giotto
                        object")
            }
        }


        # NN Graph
        giottoNearestNetworks <- list_nearest_networks(
            gobject = gobject,
            spat_unit = spatialUnits[su]
        )
        if (!is.null(giottoNearestNetworks)) {
            if (verbose) {
                message(
                    "Copying nearest networks for spatial unit: '",
                    spatialUnits[su], "'"
                )
            }
            for (i in seq(nrow(giottoNearestNetworks))) {
                nn_network <- getNearestNetwork(
                    gobject = gobject,
                    spat_unit = spatialUnits[su],
                    nn_type = giottoNearestNetworks[i]$type,
                    name = giottoNearestNetworks[i]$name,
                    output = "data.table"
                )

                # SPE stores in colpairs, with col indices instead of colnames
                cell1 <- match(nn_network$from, pData$cell_ID)
                cell2 <- match(nn_network$to, pData$cell_ID)

                SingleCellExperiment::colPair(
                    spe,
                    giottoNearestNetworks[i]$name
                ) <- S4Vectors::SelfHits(
                    cell1, cell2,
                    nnode = ncol(spe),
                    nn_network[, seq(-1, -2)] # removing from and to
                )
            }
        } else {
            if (verbose) {
                message("No nearest networks found in the input Giotto object")
            }
        }

        # Spatial Networks
        giottoSpatialNetworks <- list_spatial_networks(
            gobject = gobject,
            spat_unit = spatialUnits[su]
        )
        if (!is.null(giottoSpatialNetworks)) {
            if (verbose) {
                message(
                    "Copying spatial networks for spatial unit: '",
                    spatialUnits[su], "'"
                )
            }
            for (i in seq(nrow(giottoSpatialNetworks))) {
                sp_network <- getSpatialNetwork(
                    gobject = gobject,
                    spat_unit = spatialUnits[su],
                    name = giottoSpatialNetworks[i]$name,
                    output = "networkDT"
                )

                # spe stores in colpairs, with col indices instead of colnames
                cell1 <- match(sp_network$from, pData$cell_ID)
                cell2 <- match(sp_network$to, pData$cell_ID)

                SingleCellExperiment::colPair(
                    spe,
                    giottoSpatialNetworks[i]$name
                ) <- S4Vectors::SelfHits(
                    cell1, cell2,
                    nnode = ncol(spe),
                    sp_network[, seq(-1, -2)] # removing from and to
                )
            }
        } else {
            if (verbose) {
                message("No spatial networks found in the input Giotto object")
            }
        }

        # SpatialImages
        giottoImages <- list_images(gobject = gobject)
        if (!is.null(giottoImages)) {
            for (i in seq(nrow(giottoImages))) {
                img <- getGiottoImage(
                    gobject = gobject,
                    name = giottoImages[i]$name
                )

                if (!is.null(img@file_path)) {
                    if (verbose) message("Copying spatial image: ", img@name)

                    tryCatch(
                        expr = {
                            spe <- SpatialExperiment::addImg(spe,
                                sample_id = spe$sample_id[i],
                                image_id = img@name,
                                imageSource = img@file_path,
                                scaleFactor = mean(img@scale_factor),
                                load = TRUE
                            )
                        },
                        error = function(e) {
                            message(
                                "Error copying spatial image: ",
                                img@name,
                                ". Please check if the image path is
                                    correct and the image exists at that path."
                            )
                        }
                    )
                } else {
                    if (verbose) {
                        message(
                            "\t - Skipping image with NULL file path: ",
                            img@name
                        )
                    }
                }
                S4Vectors::metadata(spe)[[img@name]] <- img
            }
        } else {
            if (verbose) {
                message("No spatial images found in the input Giotto object")
            }
        }

        if (verbose) message("")

        # Add spe for current spatial unit to speList
        speList[[su]] <- spe
    }

    # return list of spe objects
    return(speList)
}


#' Utility function to convert a SpatialExperiment object to a Giotto object
#'
#' @param spe Input SpatialExperiment object to convert to a Giotto object.
#' @param python_path Specify the path to python.
#' @param nn_network Specify the name of the nearest neighbour network(s)
#' in the input SpatialExperiment object. Default \code{NULL} will use
#' all existing networks.
#' @param sp_network Specify the name of the spatial network(s) in the input
#' SpatialExperiment object. Default \code{NULL} will use all existing
#' networks. This can be a vector of multiple network names.
#' @param verbose A boolean value specifying if progress messages should
#' be displayed or not. Default \code{TRUE}.
#' @returns Giotto object
#' @examples
#' \dontrun{
#' library(SpatialExperiment)
#' example(read10xVisium, echo = FALSE)
#' spatialExperimentToGiotto(spe)
#' }
#' @export
spatialExperimentToGiotto <- function(
        spe,
        python_path,
        nn_network = NULL,
        sp_network = NULL,
        verbose = TRUE) {
    # Create giotto instructions and set python path
    instrs <- createGiottoInstructions(python_path = python_path)

    # Create Giotto object with first matrix
    exprMats <- SummarizedExperiment::assays(spe)
    exprMatsNames <- SummarizedExperiment::assayNames(spe)
    firstMatrix <- exprMats[[1]]

    # check unique colnames
    if (length(unique(colnames(firstMatrix))) !=
        length(colnames(firstMatrix))) {
        colnames(firstMatrix) <- make.names(colnames(firstMatrix),
            unique = TRUE
        )
    }

    if (verbose) {
        message("Creating Giotto object with ", exprMatsNames[1], " matrix")
    }
    suppressWarnings(suppressMessages(giottoObj <- createGiottoObject(
        expression = firstMatrix,
        instructions = instrs
    )))
    exprMats[[1]] <- NULL
    exprMatsNames <- exprMatsNames[-1]

    # Copying remaining matrices
    if (length(exprMats) > 0) {
        for (i in seq(exprMats)) {
            if (verbose) {
                message("Copying expression matrix: ", exprMatsNames[i])
            }
            exprObj <- create_expr_obj(
                name = exprMatsNames[i],
                exprMat = exprMats[[i]]
            )
            giottoObj <- setExpression(
                gobject = giottoObj,
                x = exprObj,
                verbose = FALSE,
                initialize = FALSE
            )
        }
    }

    # Phenotype Data
    pData <- SummarizedExperiment::colData(spe)

    # To handle cell_ID duplication issues
    if ("cell_ID" %in% colnames(pData)) {
        pData$`cell_ID` <- NULL
    }
    if (nrow(pData) > 0) {
        if (verbose) message("Copying phenotype data")
        giottoObj <- addCellMetadata(
            gobject = giottoObj,
            new_metadata = as.data.table(pData)
        )
    }

    # Feature Metadata
    fData <- SummarizedExperiment::rowData(spe)
    if (nrow(fData) > 0) {
        if (verbose) message("Copying feature metadata")
        giottoObj <- addFeatMetadata(
            gobject = giottoObj,
            new_metadata = as.data.table(fData)
        )
    }

    # Reduced Dimensions
    redDims <- SingleCellExperiment::reducedDims(spe)
    redDimsNames <- SingleCellExperiment::reducedDimNames(spe)
    if (length(redDims) > 0) {
        for (i in seq(length(redDims))) {
            if (verbose) message("Copying reduced dimensions")
            dimRedObj <- create_dim_obj(
                name = redDimsNames[i],
                coordinates = redDims[[i]],
                reduction_method = redDimsNames[i]
            )
            ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
            giottoObj <- setDimReduction(
                gobject = giottoObj,
                x = dimRedObj,
                verbose = FALSE,
                initialize = FALSE
            )
            ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        }
    }

    # Spatial Locations
    spatialLocs <- SpatialExperiment::spatialCoords(spe)
    if (ncol(spatialLocs) > 0) {
        if (verbose) message("Copying spatial locations")
        spatialLocsDT <- data.table::data.table(
            sdimx = spatialLocs[, 1],
            sdimy = spatialLocs[, 2],
            cell_ID = colnames(spe)
        )
        spatLocsObj <- create_spat_locs_obj(
            name = "spatLocs",
            coordinates = spatialLocsDT
        )
        giottoObj <- setSpatialLocations(
            gobject = giottoObj,
            x = spatLocsObj,
            verbose = FALSE,
            initialize = FALSE
        )
    }

    # Spatial Images
    spatialImages <- SpatialExperiment::imgData(spe)
    if (nrow(spatialImages) > 0) {
        for (i in seq(nrow(spatialImages))) {
            if (verbose) message("Copying spatial images")
            spImg <- SpatialExperiment::getImg(
                spe,
                spatialImages[i, "sample_id"],
                spatialImages[i, "image_id"]
            )
            mObject <- magick::image_read(grDevices::as.raster(spImg))
            giottoImage <- createGiottoImage(
                gobject = giottoObj,
                mg_object = mObject,
                scale_factor = spatialImages[i, "scaleFactor"]
            )
            giottoObj <- addGiottoImage(
                gobject = giottoObj,
                images = list(giottoImage)
            )
        }
    }

    # Networks
    networks <- SingleCellExperiment::colPairs(spe)
    # Spatial Networks
    if (!is.null(sp_network)) {
        if (all(sp_network %in% names(networks))) {
            for (i in seq(sp_network)) {
                if (verbose) message("Copying spatial networks")
                networkDT <- as.data.table(networks[[sp_network[i]]])
                networkDT$to <- colnames(spe)[networkDT$to]
                networkDT$from <- colnames(spe)[networkDT$from]
                spatNetObj <- create_spat_net_obj(
                    network = networkDT
                )
                giottoObj <- setSpatialNetwork(
                    gobject = giottoObj,
                    x = spatNetObj,
                    name = sp_network[i],
                    verbose = FALSE,
                    initialize = FALSE
                )
                networks[[sp_network[i]]] <- NULL
            }
        }
    }

    # Nearest Neighbour Networks
    if (!is.null(nn_network)) {
        if (nn_network %in% names(networks)) {
            for (i in seq(nn_network)) {
                if (verbose) message("Copying nearest neighbour networks")
                nnNetObj <- create_nn_net_obj(
                    name = nn_network[i],
                    network = networks[[nn_network[i]]]
                )
                giottoObj <- setNearestNetwork(
                    gobject = giottoObj,
                    x = nnNetObj,
                    verbose = FALSE,
                    initialize = FALSE
                )
                networks[[nn_network[i]]] <- NULL
            }
        }
    }

    # if not specified, storing remaining as NN
    if (length(networks) > 0) {
        for (i in seq(networks)) {
            if (verbose) message("Copying additional networks")
            nnNetObj <- create_nn_net_obj(
                name = names(networks)[i],
                network = networks[[i]]
            )
            giottoObj <- setNearestNetwork(
                gobject = giottoObj,
                x = nnNetObj,
                verbose = FALSE,
                initialize = FALSE
            )
        }
    }

    return(giottoObj)
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
    giottoToAnnData(
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
