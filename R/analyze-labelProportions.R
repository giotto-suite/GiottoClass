#' @include classes-utils.R
#' @include generics.R
#' @include combine_metadata.R
NULL

# ============================================================================
# labelProportionsParam + analyzeData methods.
#
# Per-cell neighborhood label compositions. Output: spatEnrObj of K x G
# proportions (groups x labels). Downstream: clusterData(spatEnrObj, ...)
# produces niche clusters. Mirrors calculateLabelProportions(); the latter
# now wraps this dispatch path.
#
# Dispatch shape (canonical: param dispatches on the data class):
#
#   analyzeData(giotto,           labelProportionsParam)   # sugar router
#   analyzeData(igraph,           labelProportionsParam)   # in-mem network
#   analyzeData(parquetEdgeStore, labelProportionsParam)   # GiottoDisk
#
# The giotto-class method is a thin router: it extracts the relevant data
# (network internal, label DT, polygon, ...) and delegates. For methods
# without a clean primary data class (`"table"` accepts a generic
# data.frame, `"polygon"` needs gobject context for spatial_locs), it
# handles them inline using the .clp_group_table / .clp_group_polygon
# helpers in R/combine_metadata.R.
# ============================================================================


# class ####

#' @name labelProportionsParam-class
#' @title Label proportions analysis parameter
#' @description
#' Parameter class for [analyzeData()] dispatching to neighborhood-level label
#' compositions. The result is a [spatEnrObj-class] of proportions
#' (groups x labels), suitable as input to [clusterData()] for niche
#' clustering.
#'
#' Three grouping methods, mirroring [calculateLabelProportions()]:
#' * `"table"` — explicit cell -> group mapping (a `data.frame` or a metadata
#'   column name).
#' * `"spatialnetwork"` — per-cell neighborhoods derived from a spatial
#'   network.
#' * `"polygon"` — cells grouped under tessellation / region polygons.
#'
#' `group_method` is read only by the [analyzeData()] giotto-class router
#' to select the primary data class to dispatch on; the primary methods
#' themselves dispatch on the data and never read it.
#' @seealso [analyzeData()], [labelProportionsParam()],
#'   [calculateLabelProportions()]
#' @exportClass labelProportionsParam
setClass("labelProportionsParam", contains = "analyzeParam")


# constructor ####

#' @name labelProportionsParam
#' @title Construct a [labelProportionsParam-class]
#' @description Factory for the label-proportions analysis parameter object.
#'   See [calculateLabelProportions()] for full descriptions of the grouping
#'   methods and their parameters.
#' @param labels character. Cell metadata column with labels to compose.
#' @param group_method character. One of `"table"`, `"spatialnetwork"`,
#'   `"polygon"`. Used by the [analyzeData()] giotto-class router to pick
#'   the primary data class to dispatch on.
#' @param groups,column_cell_id,column_group_id table-method params.
#' @param spatial_network_name,alpha,weights spatialnetwork-method params.
#' @param spat_info,select_on,centroids,spat_loc_name polygon-method params.
#' @param name character. Name to assign to result if returned as
#'   `spatEnrObj` / `gobject`.
#' @param ... additional named entries to attach to `@param`.
#' @returns A [labelProportionsParam-class] object.
#' @examples
#' p <- labelProportionsParam(labels = "leiden_clus",
#'     group_method = "spatialnetwork", spatial_network_name = "knn8")
#' @export
labelProportionsParam <- function(
    labels,
    group_method         = c("table", "spatialnetwork", "polygon"),
    groups               = NULL,
    column_cell_id       = "cell_ID",
    column_group_id      = NULL,
    spatial_network_name = NULL,
    alpha                = 1,
    weights              = FALSE,
    spat_info            = NULL,
    select_on            = c("spatial_locs", "polygons"),
    centroids            = TRUE,
    spat_loc_name        = NULL,
    name                 = "proportions",
    ...
) {
    group_method <- match.arg(
        group_method, c("table", "spatialnetwork", "polygon")
    )
    select_on <- match.arg(select_on, c("spatial_locs", "polygons"))
    checkmate::assert_character(labels, len = 1L)
    checkmate::assert_character(name, len = 1L)
    checkmate::assert_character(column_cell_id, len = 1L)
    checkmate::assert_character(column_group_id, len = 1L, null.ok = TRUE)
    checkmate::assert_numeric(alpha, lower = 0, upper = 1, len = 1L)
    checkmate::assert_logical(weights, len = 1L)

    p <- new("labelProportionsParam", param = list(...))
    p$labels               <- labels
    p$group_method         <- group_method
    p$groups               <- groups
    p$column_cell_id       <- column_cell_id
    p$column_group_id      <- column_group_id
    p$spatial_network_name <- spatial_network_name
    p$alpha                <- alpha
    p$weights              <- weights
    p$spat_info            <- spat_info
    p$select_on            <- select_on
    p$centroids            <- centroids
    p$spat_loc_name        <- spat_loc_name
    p$name                 <- name
    p
}


# shared helper ####

# Given a long-format groups DT (group, cell_ID [, weight]) and a labels DT
# (cell_ID + the label column), produce the wide-format proportions DT
# (group rows × label cols, prop values). Backends produce the rels DT in
# their own way (igraph: symmetrize edges; arrow: union_all then collect;
# polygon: spatial relate; table: identity) — this helper is the shared
# tail of the pipeline.
.lp_aggregate <- function(rels, labels, labels_col,
    column_cell_id = "cell_ID", column_group_id = "group") {
    .LPG <- .NPG <- weight <- NULL  # NSE

    labels <- data.table::copy(labels)
    if (column_cell_id != "cell_ID") {
        data.table::setnames(labels, "cell_ID", column_cell_id)
    }
    comb <- merge(rels, labels, by = column_cell_id, all.x = TRUE)
    if ("weight" %in% colnames(comb)) {
        labs_per_group <- comb[, sum(weight), by = c(column_group_id, labels_col)]
        n_per_group    <- comb[, sum(weight), by = column_group_id]
        data.table::setnames(labs_per_group, old = "V1", new = ".LPG")
        data.table::setnames(n_per_group,    old = "V1", new = ".NPG")
    } else {
        labs_per_group <- comb[, .N, by = c(column_group_id, labels_col)]
        n_per_group    <- comb[, .N, by = column_group_id]
        data.table::setnames(labs_per_group, old = "N", new = ".LPG")
        data.table::setnames(n_per_group,    old = "N", new = ".NPG")
    }
    prop_table <- merge(labs_per_group, n_per_group, by = column_group_id)
    prop_table[, "prop" := .LPG / .NPG]
    data.table::dcast(prop_table,
        formula = paste(column_group_id, labels_col, sep = "~"),
        fill = 0,
        value.var = "prop"
    )
}


# analyzeData(igraph, labelProportionsParam) ####

#' @rdname analyzeData
#' @export
setMethod("analyzeData",
    signature(x = "igraph", param = "labelProportionsParam"),
    function(x, param, ..., labels = NULL) {
        if (is.null(labels)) {
            stop("[analyzeData(igraph, labelProportionsParam)] `labels` ",
                 "data.table is required (cell_ID + label column)",
                 call. = FALSE)
        }
        labels_col <- param$labels
        alpha      <- param$alpha
        weights    <- param$weights

        # Symmetrize edges: every A -> B contributes B -> A.
        sn <- data.table::as.data.table(
            igraph::as_data_frame(x, what = "edges")
        )
        rev <- data.table::copy(sn)
        data.table::setnames(rev, c("from", "to"), c("to", "from"))
        sn <- unique(rbind(sn, rev))
        data.table::setnames(sn, c("from", "to"), c("source", "target"))
        needed_cols <- c("source", "target")
        if ("weight" %in% colnames(sn)) needed_cols <- c(needed_cols, "weight")
        sn <- sn[, needed_cols, with = FALSE]
        if (isFALSE(weights) && alpha == 1) {
            sn <- sn[, c("source", "target"), with = FALSE]
        } else if (!"weight" %in% colnames(sn) || isFALSE(weights)) {
            warning(wrap_txt("No 'weight' information present in spatial network.
                            Using adjacency instead."), call. = FALSE)
            sn[, "weight" := 1]
        }
        rels <- unique(sn)
        # self edges gated by alpha
        if (alpha != 0) {
            src <- unique(sn$source)
            self_rels <- data.table::data.table(source = src, target = src)
            if (alpha != 1 || "weight" %in% colnames(rels)) {
                self_rels[, "weight" := alpha]
            }
            rels <- rbind(rels, self_rels, fill = TRUE)
        }
        data.table::setnames(rels,
            old = c("source", "target"),
            new = c("group", "cell_ID")
        )

        .lp_aggregate(rels, labels, labels_col,
            column_cell_id = "cell_ID", column_group_id = "group")
    }
)


# analyzeData(giottoPolygon, labelProportionsParam) ####

#' @rdname analyzeData
#' @export
setMethod("analyzeData",
    signature(x = "giottoPolygon", param = "labelProportionsParam"),
    function(x, param, ..., labels = NULL, y = NULL) {
        if (is.null(labels)) {
            stop("[analyzeData(giottoPolygon, labelProportionsParam)] ",
                 "`labels` data.table is required (cell_ID + label column)",
                 call. = FALSE)
        }
        if (is.null(y)) {
            stop("[analyzeData(giottoPolygon, labelProportionsParam)] ",
                 "`y` (target geometries: giottoPoints from spat_locs, or ",
                 "a giottoPolygon for spat_unit polygons) is required",
                 call. = FALSE)
        }
        # x: grouping polygons; y: target geometries (cells as points or polys)
        rels <- relate(x, y, relation = "intersects",
                       pairs = TRUE, output = "data.table",
                       use_names = TRUE)
        data.table::setnames(rels, old = c("x", "y"),
                              new = c("group", "cell_ID"))
        .lp_aggregate(rels, labels, param$labels,
            column_cell_id = "cell_ID", column_group_id = "group")
    }
)


# analyzeData(giotto, labelProportionsParam) ####

#' @rdname analyzeData
#' @export
setMethod("analyzeData",
    signature(x = "giotto", param = "labelProportionsParam"),
    function(x, param, ...,
             spat_unit = NULL,
             feat_type = NULL,
             output = c("data.table", "matrix", "spatEnrObj", "gobject"),
             verbose = NULL) {

        fname <- "[analyzeData(giotto, labelProportionsParam)]"

        group_method    <- param$group_method
        labels_col      <- param$labels
        name            <- param$name
        column_cell_id  <- param$column_cell_id
        column_group_id <- param$column_group_id

        output <- match.arg(
            output,
            choices = c("data.table", "matrix", "spatEnrObj", "gobject")
        )
        gm_dt_incompat <- c("spatEnrObj", "gobject")
        if (output %in% gm_dt_incompat && group_method == "table") {
            stop(wrap_txtf("%s %s outputs are not available for %s",
                fname, paste(gm_dt_incompat, collapse = " and "),
                "group_method = \"table\""
            ), call. = FALSE)
        }

        spat_unit <- set_default_spat_unit(gobject = x, spat_unit = spat_unit)
        feat_type <- set_default_feat_type(
            gobject = x, spat_unit = spat_unit, feat_type = feat_type
        )

        labs <- spatValues(x,
            feats = labels_col,
            spat_unit = spat_unit,
            feat_type = feat_type,
            verbose = FALSE
        )

        # Dispatch on the data class the chosen group_method works against.
        # spatialnetwork → delegate to analyzeData(<network@network>, param)
        # so the disk-backed (parquetEdgeStore) method picks up automatically
        # when GiottoDisk is loaded. table / polygon stay inline since they
        # don't have a clean primary data class.
        res <- switch(group_method,
            "spatialnetwork" = {
                sn_obj <- getSpatialNetwork(x,
                    spat_unit = spat_unit,
                    name = param$spatial_network_name,
                    output = "spatialNetworkObj",
                    verbose = verbose
                )
                analyzeData(sn_obj[], param, labels = labs)
            },
            "table" = {
                groups <- .clp_group_table(
                    gobject = x,
                    groups = param$groups,
                    spat_unit = spat_unit,
                    feat_type = feat_type,
                    verbose = verbose
                )
                if (!column_cell_id %in% colnames(groups)) {
                    stop(wrap_txt(
                        fname, "'column_cell_id' must be a colname",
                        "of 'groups' table\n"), call. = FALSE)
                }
                if (!is.null(column_group_id) &&
                    isTRUE(!column_group_id %in% colnames(groups))) {
                    stop(wrap_txt(
                        fname, "if provided, 'column_group_id' must be a colname",
                        "of 'groups' table\n"), call. = FALSE)
                }
                column_group_id <- .clp_detect_group_col(
                    groups, column_cell_id, column_group_id, verbose = verbose
                )
                .lp_aggregate(groups, labs, labels_col,
                    column_cell_id = column_cell_id,
                    column_group_id = column_group_id)
            },
            "polygon" = {
                checkmate::assert_character(param$spat_info, len = 1L)
                x_poly <- getPolygonInfo(x,
                    polygon_name = param$spat_info,
                    return_giottoPolygon = TRUE,
                    verbose = verbose
                )
                y_geom <- switch(param$select_on,
                    "spatial_locs" = {
                        sl <- getSpatialLocations(x,
                            spat_unit = spat_unit,
                            name = param$spat_loc_name,
                            output = "spatLocsObj",
                            verbose = FALSE)
                        createGiottoPoints(as.points(sl))
                    },
                    "polygons" = {
                        yp <- getPolygonInfo(x,
                            polygon_name = spat_unit,
                            return_giottoPolygon = TRUE,
                            verbose = FALSE)
                        if (isTRUE(param$centroids)) {
                            createGiottoPoints(centroids(yp))
                        } else {
                            yp
                        }
                    }
                )
                analyzeData(x_poly, param, labels = labs, y = y_geom)
            }
        )

        # Output wrapping
        if (output %in% c("spatEnrObj", "gobject")) {
            data.table::setnames(res, old = "group", new = "cell_ID")
            enr <- createSpatEnrObj(res,
                name = name,
                spat_unit = spat_unit,
                feat_type = feat_type,
                method = "calculateLabelProportions",
                verbose = FALSE
            )
            if (group_method == "polygon") {
                spatUnit(enr) <- param$spat_info
                has_sl <- isTRUE(nrow(list_giotto_data(x,
                    slot = "spatial_locs", spat_unit = param$spat_info
                )) >= 1)
                if (!has_sl) {
                    x <- addSpatialCentroidLocations(x,
                        poly_info = param$spat_info, verbose = FALSE
                    )
                }
            }
            switch(output,
                "spatEnrObj" = return(enr),
                "gobject" = {
                    # NOTE: param-history logging via update_giotto_params
                    # is performed by the calculateLabelProportions()
                    # wrapper, not here — get_args / match.call walks the
                    # call stack and S4's .local dispatch wrapper breaks
                    # the frame arithmetic. Direct analyzeData callers
                    # get the setGiotto but no history entry.
                    return(setGiotto(x, enr, verbose = verbose))
                }
            )
        } else {
            switch(output,
                "data.table" = return(res),
                "matrix" = return(dt_to_matrix(res))
            )
        }
    }
)
