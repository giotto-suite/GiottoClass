.init_check_severity <- function() {
    x <- getOption("giotto.init_check_severity", "stop")
    if (!x %in% c("stop", "warning")) {
        stop("option 'giotto.init_check_severity' ",
            "must be one of \"stop\" or \"warning\"",
            call. = FALSE
        )
    }
    x
}

.init_check_output_prefix <- function(
    slotname = NULL,
    spat_unit = NULL,
    feat_type = NULL,
    name = NULL) {
    prefix <- "[gobject init-check]"
    if (!is.null(slotname)) {
        prefix <- sprintf("%s[%s]", prefix, color_purple(slotname))
    }
    if (!is.null(spat_unit)) {
        prefix <- sprintf("%s[%s]", prefix, color_blue(spat_unit))
    }
    if (!is.null(feat_type)) {
        prefix <- sprintf("%s[%s]", prefix, color_red(feat_type))
    }
    if (!is.null(name)) {
        prefix <- sprintf("%s[%s]", prefix, color_teal(name))
    }
    paste0(prefix, "\n ")
}

.init_check_log <- function(x, ..., type = "warn",
    slotname = NULL,
    spat_unit = NULL,
    feat_type = NULL,
    name = NULL,
    verbose = NULL) { # only used with message type
    type <- match.arg(type, c("warning", "stop", "message"))
    pre <- .init_check_output_prefix(
        slotname = slotname,
        spat_unit = spat_unit,
        feat_type = feat_type,
        name = name
    )
    txt <- paste(pre, x)
    switch(type,
        "warning" = {
            warning(wrap_txt(txt, ...), call. = FALSE)
        },
        "stop" = {
            stop(wrap_txt(txt, ..., errWidth = TRUE), call. = FALSE)
        },
        "message" = {
            vmsg(.v = verbose, txt, ...)
        }
    )
}

#### slot checks ####

#' @keywords internal
#' @noRd
.check_cell_metadata <- function(gobject,
    verbose = TRUE) {
    # data.table vars
    cell_ID <- spat_unit <- NULL

    # find available cell metadata
    avail_cm <- list_cell_metadata(gobject)
    g_su <- list_cell_id_names(gobject)
    used_su <- unique(avail_cm$spat_unit)


    # [check hierarchical]
    # metadata is only allowed to exist when the associated spat_unit exists in
    # polygon and/or expression data
    missing_su <- !used_su %in% g_su
    if (any(missing_su)) {
        .init_check_log(
            type = "stop",
            slotname = "cell metadata",
            spat_unit = used_su[missing_su],
            "No expression or polygon information discovered.
            Please add expression or polygon info for this spatial unit first."
        )
    }

    for (su_i in used_su) {
        IDs <- spatIDs(gobject, spat_unit = su_i)
        search_ids <- c(head(IDs, 10L), tail(IDs, 10L))

        su_cm <- avail_cm[spat_unit == su_i, ]
        lapply(seq(nrow(su_cm)), function(obj_i) {
            ft_i <- su_cm$feat_type[[obj_i]]

            # console print helper
            .cm_log <- function(x, ..., type = "warning") {
                .init_check_log(
                    x = x,
                    ...,
                    type = type,
                    slotname = "cell metadata",
                    spat_unit = su_i,
                    feat_type = ft_i,
                    verbose = verbose
                )
            }

            # get metadata
            meta <- getCellMetadata(
                gobject = gobject,
                spat_unit = su_i,
                feat_type = ft_i,
                output = "cellMetaObj",
                copy_obj = FALSE,
                set_defaults = FALSE
            )

            # no cell_IDs (attempts repair)
            if (any(meta[][, is.na(cell_ID)])) {
                # denotes missing or need to repair IDs
                # works by reference
                .check_metadata_repair_ids(meta,
                    meta_type = "cell",
                    search_ids = search_ids,
                    gobj_ids = IDs,
                    verbose = verbose,
                    .log_fun = .cm_log
                )
            }

            # duplicated IDs
            if (any(meta[][, duplicated(cell_ID)])) {
                .cm_log(
                    type = .init_check_severity(),
                    "Duplicates found in cell_ID column."
                )
            }

            # length mismatch
            if (nrow(meta[]) > length(IDs)) {
                m_IDs <- meta[][["cell_ID"]]
                filter_bool_cells <- m_IDs %in% IDs

                meta[] <- meta[][filter_bool_cells]
            }

            if (nrow(meta[]) < length(IDs)) {
                ID_dt <- data.table::data.table(cell_ID = IDs)
                meta[] <- merge(ID_dt, meta[], all.x = TRUE)
            }

            if (nrow(meta[]) != length(IDs)) {
                .cm_log(
                    type = .init_check_severity(),
                    sprintf(
                        "Number of entries (%d) != number of gobject IDs (%d)",
                        nrow(meta[]), length(IDs)
                    )
                )
            }

            # cell_ID  contents mismatch
            if (!meta[][, setequal(cell_ID, IDs)]) {
                .cm_log(
                    type = .init_check_severity(),
                    "IDs do not match between metadata and cell_ID slot"
                )
            }

            # ensure ID col first
            setcolorder(meta[], "cell_ID")
        })
    }
}




#' @keywords internal
#' @noRd
.check_feat_metadata <- function(gobject,
    verbose = TRUE) {
    # data.table vars
    feat_ID <- spat_unit <- feat_type <- NULL

    # find available feat metadata
    avail_fm <- list_feat_metadata(gobject)
    avail_ex <- list_expression(gobject)
    g_ft <- list_feat_id_names(gobject)
    used_ft <- unique(avail_fm$feat_type)


    # check hierarchical
    missing_ft <- !used_ft %in% g_ft
    if (any(missing_ft)) {
        .init_check_log(
            type = "stop",
            slotname = "feature metadata",
            feat_type = used_ft[missing_ft],
            "No expression values found with this feature type.
            Please add expression values data first."
        )
    }

    for (ft_i in used_ft) {
        ft_fm <- avail_fm[feat_type == ft_i, ]
        lapply(seq(nrow(ft_fm)), function(obj_i) {
            su_i <- ft_fm$spat_unit[[obj_i]]

            .fm_log <- function(x, ..., type = "warning") {
                .init_check_log(
                    x = x,
                    ...,
                    type = type,
                    slotname = "feature metadata",
                    spat_unit = su_i,
                    feat_type = ft_i,
                    verbose = verbose
                )
            }

            # get metadata
            meta <- getFeatureMetadata(
                gobject = gobject,
                spat_unit = su_i,
                feat_type = ft_i,
                output = "featMetaObj",
                copy_obj = FALSE,
                set_defaults = FALSE
            )

            #----------------- early exit cases -------------------#
            # Start checking values when specific expression value is added
            if (is.null(avail_ex)) {
                return()
            }
            if (nrow(avail_ex[spat_unit == su_i & feat_type == ft_i]) == 0L) {
                return() # skip checks if no expression found

            }
            #------------------------------------------------------#

            # check IDs based on expression value info
            IDs <- featIDs(getExpression(
                gobject = gobject,
                spat_unit = su_i,
                feat_type = ft_i,
                output = "exprObj"
            ))
            search_ids <- c(head(IDs, 10L), tail(IDs, 10L))

            # no feat_IDs (attempts repair)
            if (any(meta[][, is.na(feat_ID)])) {
                # denotes missing or need to repair IDs
                # works by reference
                .check_metadata_repair_ids(meta,
                    meta_type = "feature",
                    search_ids = search_ids,
                    gobj_ids = IDs,
                    verbose = verbose,
                    .log_fun = .fm_log
                )
            }

            # duplicated IDs
            if (any(meta[][, duplicated(feat_ID)])) {
                .fm_log(
                    type = .init_check_severity(),
                    "Duplicates found in feat_ID column."
                )
            }

            # length mismatch
            if (nrow(meta[]) > length(IDs)) {
                m_IDs <- meta[][["feat_ID"]]
                filter_bool_feats <- m_IDs %in% IDs

                meta[] <- meta[][filter_bool_feats]
            }

            if (nrow(meta[]) < length(IDs)) {
                ID_dt <- data.table::data.table(feat_ID = IDs)
                meta[] <- merge(ID_dt, meta[], all.x = TRUE)
            }

            if (nrow(meta[]) != length(IDs)) {
                .fm_log(
                    type = .init_check_severity(),
                    sprintf(
                        "Number of entries (%d) != number of gobject IDs (%d)",
                        nrow(meta[]), length(IDs)
                    )
                )
            }

            # feat_ID  contents mismatch
            if (!meta[][, setequal(feat_ID, IDs)]) {
                .fm_log(
                    type = .init_check_severity(),
                    "IDs do not match between metadata and feat_ID slot"
                )
            }

            # ensure ID col first
            setcolorder(meta[], "feat_ID")
        })
    }
}

.check_metadata_repair_ids <- function(
        meta, meta_type, search_ids, gobj_ids, verbose = NULL, .log_fun
) {
    switch(meta_type,
        "cell" = {
            id_term <- "cell_ID"
        },
        "feature" = {
            id_term <- "feat_ID"
        },
        stop("unknown meta_type")
    )

    # check across columns for presence of known IDs
    id_col_matches <- vapply(meta[],
        function(x) sum(search_ids %in% x),
        FUN.VALUE = integer(1L)
    )
    # select the col with most matches
    id_col_guess <- which.max(id_col_matches)
    # set to 0 to signal failure if no matches are found
    if (setequal(id_col_matches, 0)) id_col_guess <- 0

    if (id_col_guess == 0L) {
        # likely that ID col does not exist yet
        if (length(gobj_ids) == nrow(meta[])) {
            .log_fun(
                type = "message",
                sprintf(
                    "No %s info found within %s metadata.
                    Directly assigning based on gobject %s",
                    id_term, meta_type, id_term
                )
            )
            meta[][, (id_term) := gobj_ids] # set by reference
        } else {
            .log_fun(
                type = "stop",
                sprintf(
                    "No %s info found within %s metadata.
                    Unable to guess IDs based on gobject %s",
                    id_term, meta_type, id_term
                )
            )
        }
    } else { # otherwise, ID col found
        id_col_name <- names(id_col_guess)
        meta[][, (id_term) := NULL] # remove older column
        data.table::setnames(
            meta[],
            old = id_col_name, new = id_term
        )
        .log_fun(
            type = "message",
            sprintf("Guessing %s as %s column", id_col_name, id_term)
        )
    }
    # this function works via set by reference
    # no values to return
    return()
}






#' @title Check spatial location data
#' @name .check_spatial_location_data
#' @description check cell ID (spatial unit) names between spatial location
#' and expression data. It will look for identical IDs after sorting.
#' @keywords internal
#' @returns character or NULL
.check_spatial_location_data <- function(gobject) {
    # define for data.table
    cell_ID <- spat_unit <- name <- NULL

    # find available spatial locations
    avail_sl <- list_spatial_locations(gobject)
    # avail_ex <- list_expression(gobject)
    # avail_si <- list_spatial_info(gobject)

    # # check hierarchical
    # missing_unit <- !(avail_sl$spat_unit) %in%
    #     c(avail_ex$spat_unit, avail_si$spat_info)
    # if (any(missing_unit)) {
    #     .init_check_log(
    #         type = .init_check_severity(),
    #         slotname = "spatial locations",
    #         spat_unit = avail_sl$spat_unit[missing_unit],
    #         "No expression values or polygon information discovered.
    #         Please add expression values or polygon information for this
    #         spatial unit first."
    #     )
    # }

    for (su_i in avail_sl[["spat_unit"]]) {
        gobj_ids <- spatIDs(gobject, spat_unit = su_i)

        if (length(gobj_ids) == 0L) {
            # no values means that the expression information or polygons
            # data has not been added yet. Nothing to check against.
            next
        }

        for (coord_i in avail_sl[spat_unit == su_i, name]) {
            # 1. get colnames
            spatlocs <- get_spatial_locations(gobject,
                spat_unit = su_i,
                spat_loc_name = coord_i,
                output = "data.table",
                copy_obj = FALSE
            )
            missing_ids <- spatlocs[, all(is.na(cell_ID))]

            # setup console prints
            .sl_log <- function(x, ..., type = "warning") {
                .init_check_log(
                    x = x,
                    ...,
                    type = type,
                    slotname = "spatial locations",
                    spat_unit = su_i,
                    name = coord_i
                )
            }

            ## check if spatlocs and cell_ID do not match in length
            if (spatlocs[, .N] != length(gobj_ids)) {
                .sl_log(
                    type = .init_check_severity(),
                    sprintf(
                        "Number of entries (%d) != number of gobject IDs (%d)",
                        spatlocs[, .N], length(gobj_ids)
                    )
                )
                if (missing_ids) { # if also missing IDs for spatlocs...
                    # hardcode this error since the reason it occurs is
                    # easier to understand when reported here
                    .sl_log(
                        type = "stop",
                        "Number of entries mismatch with gobject IDs AND
                        missing spatial locations IDs. Aborting."
                    )
                }
            }

            if (missing_ids) {
                ## ! modify coords within gobject by reference
                spatlocs <- spatlocs[, cell_ID := gobj_ids]
            }

            # if cell_ID column is provided then compare with expected cell_IDs
            if (!missing_ids) {
                spatial_cell_id_names <- spatlocs[["cell_ID"]]

                if (!setequal(spatial_cell_id_names, gobj_ids)) {
                    .sl_log(
                        type = .init_check_severity(),
                        "cell_IDs mismatch with gobject IDs"
                    )
                }
            }
        }
    }
    return(invisible())
}






#' @keywords internal
#' @noRd
.check_spatial_networks <- function(gobject) {
    # DT vars
    spat_unit <- NULL

    avail_sn <- list_spatial_networks(gobject = gobject)
    avail_sl <- list_spatial_locations(gobject = gobject)
    used_su <- unique(avail_sn$spat_unit)

    # check hierarchical
    missing_su <- !used_su %in% avail_sl$spat_unit
    if (sum(missing_su != 0L)) {
        .init_check_log(
            slotname = "spatial networks",
            type = .init_check_severity(),
            "Matching spatial locations in spat_unit(s)",
            used_su[missing_su],
            "must be added before the respective spatial networks."
        )
    }

    if (!is.null(used_su)) {
        for (su_i in used_su) {
            IDs <- spatIDs(gobject, spat_unit = su_i)

            su_sn <- avail_sn[spat_unit == su_i, ]
            lapply(seq(nrow(su_sn)), function(obj_i) {
                sn_obj <- get_spatialNetwork(
                    gobject = gobject,
                    spat_unit = su_i,
                    name = su_sn$name[[obj_i]],
                    output = "spatialNetworkObj",
                    set_defaults = FALSE,
                    copy_obj = FALSE,
                    verbose = FALSE
                )
                if (!all(spatIDs(sn_obj) %in% IDs)) {
                    .init_check_log(
                        type = "warning",
                        slotname = "spatial networks",
                        spat_unit = su_i,
                        name = su_sn$name[[obj_i]],
                        "Spatial network vertex names are not all found in
                        gobject IDs."
                    )
                }
            })
        }
    }
}






#' @name .check_spatial_enrichment
#' @description check the spatial enrichment information within the gobject
#' @keywords internal
#' @noRd
.check_spatial_enrichment <- function(gobject) {
    # DT vars
    spat_unit <- NULL

    avail_se <- list_spatial_enrichments(gobject = gobject)
    avail_sl <- list_spatial_locations(gobject = gobject)
    used_su <- unique(avail_se$spat_unit)

    # check hierarchical
    missing_su <- !used_su %in% avail_sl$spat_unit
    if (sum(missing_su != 0L)) {
        .init_check_log(
            slotname = "spatial enrichments",
            type = .init_check_severity(),
            "Matching spatial locations in spat_unit(s)",
            used_su[missing_su],
            "must be added before the respective spatial enrichments."
        )
    }

    if (!is.null(used_su)) {
        for (su_i in used_su) {
            IDs <- spatIDs(gobject, spat_unit = su_i)

            su_se <- avail_se[spat_unit == su_i, ]
            lapply(seq(nrow(su_se)), function(obj_i) {
                se_obj <- get_spatial_enrichment(
                    gobject = gobject,
                    spat_unit = su_i,
                    feat_type = su_se$feat_type[[obj_i]],
                    enrichm_name = su_se$name[[obj_i]],
                    output = "spatEnrObj",
                    copy_obj = FALSE,
                    set_defaults = FALSE
                )
                if (!setequal(spatIDs(se_obj), IDs)) {
                    .init_check_log(
                        slotname = "spatial enrichments",
                        type = "warning",
                        spat_unit = su_i,
                        feat_type = su_se$feat_type[[obj_i]],
                        name = su_se$name[[obj_i]],
                        "Spatial enrichment IDs are not all found in gobject
                        IDs."
                    )
                }
            })
        }
    }
}










#' @name .check_dimension_reduction
#' @keywords internal
#' @noRd
.check_dimension_reduction <- function(gobject) {
    # DT vars
    spat_unit <- feat_type <- NULL

    # check that all spatIDs of coordinates setequals with gobject cell_ID
    # for the particular spat_unit
    avail_dr <- list_dim_reductions(gobject = gobject)
    avail_ex <- list_expression(gobject = gobject)
    used_su <- unique(avail_dr$spat_unit)
    used_su_ft <- unique(avail_dr[
        ,
        paste0("[", spat_unit, "][", feat_type, "]")
    ])
    ex_su_ft <- unique(avail_ex[
        ,
        paste0("[", spat_unit, "][", feat_type, "]")
    ])

    # check hierarchical
    missing_su_ft <- !used_su_ft %in% ex_su_ft
    if (sum(missing_su_ft != 0L)) {
        .init_check_log(
            type = .init_check_severity(),
            slotname = "dimension reduction",
            "Matching expression values [spat_unit][feat_type]:\n",
            used_su_ft[missing_su_ft],
            "\nmust be added before the respective dimension reductions."
        )
    }

    if (!is.null(used_su)) {
        for (su_i in used_su) {
            IDs <- spatIDs(gobject, spat_unit = su_i)

            su_dr <- avail_dr[spat_unit == su_i, ]
            lapply(seq(nrow(su_dr)), function(obj_i) {
                dr_obj <- get_dimReduction(
                    gobject = gobject,
                    spat_unit = su_i,
                    feat_type = su_dr$feat_type[[obj_i]],
                    reduction = su_dr$data_type[[obj_i]],
                    reduction_method = su_dr$dim_type[[obj_i]],
                    name = su_dr$name[[obj_i]],
                    output = "dimObj",
                    set_defaults = FALSE
                )

                # if matrix has no IDs, regenerate from gobject IDs
                if (is.null(spatIDs(dr_obj))) {
                    if (nrow(dr_obj[]) == length(IDs)) {
                        # if nrow of matrix and number of gobject cell_IDs for
                        # the spat unit
                        # match, then try guessing then set data back to replace
                        warning(wrap_txt(
                            "data_type:", su_dr$data_type[[obj_i]],
                            "spat_unit:", su_i,
                            "feat_type:", su_dr$feat_type[[obj_i]],
                            "dim_type:", su_dr$dim_type[[obj_i]],
                            "name:", su_dr$name[[obj_i]], "\n",
                            "Dimension reduction has no cell_IDs.
                            Guessing based on existing expression cell_IDs"
                        ))
                        rownames(dr_obj[]) <- IDs
                        ### ### ### ### ### ### ### ### ### ### ### ### ### ###
                        gobject <- set_dimReduction(
                            gobject = gobject,
                            dimObject = dr_obj,
                            set_defaults = FALSE,
                            initialize = TRUE,
                            verbose = FALSE
                        )
                        ### ### ### ### ### ### ### ### ### ### ### ### ### ###
                    } else {
                        # if number of values do NOT match, throw error
                        stop(wrap_txt(
                            "data_type:", su_dr$data_type[[obj_i]],
                            "spat_unit:", su_i,
                            "feat_type:", su_dr$feat_type[[obj_i]],
                            "dim_type:", su_dr$dim_type[[obj_i]],
                            "name:", su_dr$name[[obj_i]], "\n",
                            "Dimension reduction has no cell_IDs.
                            Number of rows also does not match expression
                            columns"
                        ))
                    }
                }

                # if does not have all the IDS seen in the gobject
                if (!all(spatIDs(dr_obj) %in% IDs)) {
                    warning(wrap_txt(
                        "data_type:", su_dr$data_type[[obj_i]],
                        "spat_unit:", su_i,
                        "feat_type:", su_dr$feat_type[[obj_i]],
                        "dim_type:", su_dr$dim_type[[obj_i]],
                        "name:", su_dr$name[[obj_i]], "\n",
                        "Dimension reduction coord names are not all found in
                        gobject IDs"
                    ))
                }
            })
        }
    }
    return(gobject)
}







#' @keywords internal
#' @noRd
.check_nearest_networks <- function(gobject) {
    # DT vars
    spat_unit <- feat_type <- NULL

    avail_nn <- list_nearest_networks(gobject = gobject)
    avail_dr <- list_dim_reductions(gobject = gobject)
    used_su <- unique(avail_nn$spat_unit)
    used_su_ft <- unique(avail_nn[
        ,
        paste0("[", spat_unit, "][", feat_type, "]")
    ])
    dr_su_ft <- unique(avail_dr[, paste0("[", spat_unit, "][", feat_type, "]")])

    # check hierarchical
    missing_su_ft <- !used_su_ft %in% dr_su_ft
    if (sum(missing_su_ft != 0L)) {
        .init_check_log(
            type = .init_check_severity(),
            slotname = "nearest networks",
            "Matching dimension reductions [spat_unit][feat_type]:\n",
            used_su_ft[missing_su_ft],
            "\nmust be added before the respective nearest neighbor networks."
        )
    }

    if (!is.null(used_su)) {
        for (su_i in used_su) {
            IDs <- spatIDs(gobject, spat_unit = su_i)

            su_nn <- avail_nn[spat_unit == su_i, ]
            lapply(seq(nrow(su_nn)), function(obj_i) {
                nn_obj <- get_NearestNetwork(
                    gobject = gobject,
                    spat_unit = su_i,
                    feat_type = su_nn$feat_type[[obj_i]],
                    nn_network_to_use = su_nn$nn_type[[obj_i]],
                    network_name = su_nn$name[[obj_i]],
                    output = "nnNetObj",
                    set_defaults = FALSE
                )
                if (!all(spatIDs(nn_obj) %in% IDs)) {
                    warning(wrap_txt(
                        "spat_unit:", su_i,
                        "feat_type:", su_nn$feat_type[[obj_i]],
                        "nn_type:", su_nn$nn_type[[obj_i]],
                        "name:", su_nn$name[[obj_i]], "\n",
                        "Nearest network vertex names are not all found in
                        gobject IDs"
                    ))
                }
            })
        }
    }
}








#' @name .check_spatial_info
#' @keywords internal
#' @noRd
.check_spatial_info <- function(gobject) {
    # DT vars
    spat_unit <- NULL

    avail_sinfo <- list_spatial_info(gobject)
    if (is.null(avail_sinfo)) {
        return(gobject)
    } # quit early if none available

    avail_slocs <- list_spatial_locations(gobject)

    common_su <- intersect(avail_sinfo$spat_info, avail_slocs$spat_unit)

    # If there are any shared spatial units, match IDs
    if (length(common_su) != 0) {
        for (su_i in common_su) {
            # get spat_info
            sinfo <- get_polygon_info(
                gobject = gobject,
                polygon_name = su_i,
                return_giottoPolygon = TRUE
            )

            # get spatlocs
            su_sloc <- avail_slocs[spat_unit == su_i]
            lapply(seq(nrow(su_sloc)), function(obj_i) {
                spatlocs <- get_spatial_locations(
                    gobject = gobject,
                    spat_unit = su_i,
                    spat_loc_name = su_sloc$name[[obj_i]],
                    output = "spatLocsObj",
                    set_defaults = FALSE,
                    copy_obj = FALSE
                )
                if (!all(spatIDs(spatlocs) %in% spatIDs(sinfo))) {
                    warning(wrap_txt(
                        "spat_unit:", su_i,
                        "spatloc name:", su_sloc$name[[obj_i]], "\n",
                        "cell IDs in spatial locations are missing from spatial
                        polygon info"
                    ))
                }
            })
        }
    }
}





#' @name .check_feature_info
#' @keywords internal
#' @noRd
.check_feature_info <- function(gobject) {
    # TODO ... expr info or meta info w/ IDs not in feature info
}
