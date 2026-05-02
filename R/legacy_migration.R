

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

        gobject <- set_spatial_enrichment(gobject,
            spatial_enrichment,
            spat_unit = "cell",
            feat_type = expression_feat,
            enrichm_name = i
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
