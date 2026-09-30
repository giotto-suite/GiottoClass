#' @include classes-virtuals.R
#' @include classes-utils.R
#' @include generics.R
NULL

# ============================================================================
# featStatsParam / cellStatsParam + analyzeData methods.
#
# Per-feature and per-cell summary statistics. These are arithmetic summaries
# with no analysis choice in them, which other packages build on -- cluster
# means for heatmaps and trees, gini markers, HVF inputs -- so they live next
# to the generic rather than with the analysis methods in Giotto.
#
# The grouped featStats path defines a column and row-order contract that
# disk backends reproduce, so a caller cannot tell the backends apart.
#
# No factory here: the classes are built with `new()`. Giotto's
# `analyzeParam("feat_stats" / "cell_stats")` stays as the user-facing sugar,
# so the methods default `detection_threshold` themselves.
# ============================================================================


# classes ####

#' @name featStatsParam-class
#' @title Feature and cell summary statistics parameters
#' @aliases featStatsParam cellStatsParam cellStatsParam-class
#' @description
#' Parameter classes for [analyzeData()] that compute summary statistics over
#' matrix-type data.
#'
#' * `featStatsParam` — per feature: `nr_cells`, `perc_cells`, `total_expr`,
#'   `mean_expr`, `mean_expr_det`. With `groups =`, the statistics are taken
#'   per (feature, group) instead.
#' * `cellStatsParam` — per cell: `nr_feats`, `perc_feats`, `total_expr`.
#'
#' Both read one setting from `@param`, `detection_threshold` (default `0`):
#' values above it count as detected. Build them with `new()`, e.g.
#' `new("featStatsParam", param = list(detection_threshold = 0))`.
#' @seealso [analyzeData()]
#' @exportClass featStatsParam
setClass("featStatsParam", contains = "analyzeParam")

#' @rdname featStatsParam-class
#' @exportClass cellStatsParam
setClass("cellStatsParam", contains = "analyzeParam")


# methods ####

# exprObj base dispatch
#' @rdname analyzeData
setMethod("analyzeData",
    signature(x = "exprObj", param = "analyzeParam"),
    function(x, param, ...) {
        analyzeData(x[], param, ...)
    }
)

# * featStatsParam ####
#' @rdname analyzeData
#' @param groups optional vector of group assignments, one per column of `x`,
#' `NA` to exclude. When supplied, the statistics are taken per
#' (feature, group) instead of over every cell, and the result gains `group`
#' and `n_cells` columns.
#' @param stats optional character vector of accumulators to compute, any of
#' `"sum"`, `"sumsq"`, `"nnz"`, `"sum_det"`. Grouped path only; emitted columns
#' are whichever the requested accumulators support.
setMethod("analyzeData",
    signature(x = "allMatrix", param = "featStatsParam"),
    function(x, param, ..., groups = NULL, stats = NULL) {
        det_thresh <- param$detection_threshold %null% 0

        if (!is.null(groups)) {
            return(.feat_stats_grouped(
                x, det_thresh, align_groups(x, groups),
                stats = stats %null% c("sum", "sumsq", "nnz", "sum_det")
            ))
        }
        if (!is.null(stats)) {
            stop("[feat_stats] `stats` selection is only available on the ",
                "grouped path (pass `groups`). The ungrouped verb has a ",
                "fixed column contract.", call. = FALSE)
        }

        mean_expr_det <- NULL
        n_detected <- rowSums_flex(x > det_thresh)
        feat_stats <- data.table::data.table(
            feats      = rownames(x),
            nr_cells   = n_detected,
            perc_cells = (n_detected / ncol(x)) * 100,
            total_expr = rowSums_flex(x),
            mean_expr  = rowMeans_flex(x)
        )
        feat_stats[, mean_expr_det := .mean_expr_det_test(
            x, detection_threshold = det_thresh
        )]
        feat_stats
    }
)

# * cellStatsParam ####
#' @rdname analyzeData
setMethod("analyzeData",
    signature(x = "allMatrix", param = "cellStatsParam"),
    function(x, param, ...) {
        det_thresh <- param$detection_threshold %null% 0
        n_detected <- colSums_flex(x > det_thresh)
        data.table::data.table(
            cells      = colnames(x),
            nr_feats   = n_detected,
            perc_feats = (n_detected / nrow(x)) * 100,
            total_expr = colSums_flex(x)
        )
    }
)


# internals ####

#' @title Align a grouping to the columns of a matrix
#' @name align_groups
#' @description
#' Put `groups` into the column order of `x`. Named input is matched on
#' column (cell) ID; unnamed input is matched by position, with a warning.
#' Columns the grouping does not name become `NA`.
#' @param x matrix-like object with column names
#' @param groups vector or factor of group assignments, ideally named by
#' column ID
#' @returns `groups`, reordered to the columns of `x`
#' @keywords internal
#' @export
# A grouping has to mean the same set of cells on both sides. Callers build it
# from cell metadata, and the expression matrix and the metadata are fetched
# independently with no guarantee they share a cell order, so a bare positional
# vector can silently assign each cell another cell's group.
#
# Named input is matched on cell ID and cannot be misaligned. Names carry
# through `factor()` and `droplevels()`, so a named factor works too and keeps
# its level order. Unnamed input still works positionally, with a warning --
# same contract `addCellMetadata()` uses for its key column.
#
# Columns the grouping does not name become NA and drop out of the statistics,
# which is already what an NA label means here -- so narrowing to a few groups by
# masking the rest is a supported way to call this, and the disk backend resolves
# a grouping the same way. No overlap at all is a mistake, not an empty
# selection, and says so.
align_groups <- function(x, groups) {
    if (is.null(groups)) {
        return(NULL)
    }
    ids <- colnames(x)

    if (!is.null(names(groups)) && !is.null(ids)) {
        ord <- match(ids, names(groups))
        if (all(is.na(ord))) {
            stop("`groups` is named but none of its names are columns of `x`.",
                call. = FALSE
            )
        }
        return(groups[ord])
    }

    if (length(groups) != ncol(x)) {
        stop("`groups` must have one entry per column of `x` (", ncol(x),
            "), got ", length(groups), ".",
            call. = FALSE
        )
    }
    if (!is.null(ids)) {
        warning("`groups` is unnamed and is being matched to `x` by position. ",
            "Pass a vector or factor named by cell ID to match on identity ",
            "instead -- cell metadata and expression are not guaranteed to ",
            "share a column order.",
            call. = FALSE
        )
    }
    groups
}


# Per-(feature, group) statistics: the same accumulators, partitioned by a
# per-cell grouping instead of taken over every cell.
#
# The contract matches GiottoDisk's streaming implementation exactly, because
# the whole point is that a caller cannot tell the backends apart -- same
# columns, same zero-filled feats x groups cross product, same threshold
# semantics where the detection threshold gates `nr_cells` but never the sums.
#
# Emitting the complete cross product matters: a feature with no expression in
# a group has mean 0 over that group's cells, not a missing row. Gini is taken
# over the length-G vector per feature, so a dropped row would silently change
# the coefficient.
.feat_stats_grouped <- function(x, thr, groups,
    stats = c("sum", "sumsq", "nnz", "sum_det")) {
    stats <- match.arg(stats, several.ok = TRUE)

    # `droplevels` so an unused level cannot surface as a group of zero cells
    g <- droplevels(if (is.factor(groups)) groups else factor(groups))
    lvls <- levels(g)
    if (length(lvls) < 1L) {
        stop("[feat_stats] `groups` has no non-empty levels.", call. = FALSE)
    }

    n_feats <- nrow(x)
    nk <- as.numeric(tabulate(as.integer(g), nbins = length(lvls)))
    names(nk) <- lvls

    per <- lapply(lvls, function(k) {
        sub <- x[, which(g == k), drop = FALSE]
        det <- if (any(c("nnz", "sum_det") %in% stats)) sub > thr
        list(
            # `rowMeans_flex` rather than sum/n keeps the gini path
            # bit-identical to the per-cluster means it replaced, rather than
            # merely equal to tolerance.
            mean = if ("sum" %in% stats) rowMeans_flex(sub),
            sum = if ("sum" %in% stats) rowSums_flex(sub),
            sumsq = if ("sumsq" %in% stats) rowSums_flex(sub * sub),
            nnz = if ("nnz" %in% stats) rowSums_flex(det),
            sum_det = if ("sum_det" %in% stats) rowSums_flex(sub * det)
        )
    })

    # groups slowest, feats cycling within -- the order `matrix` unrolls in,
    # matching what the streaming backend emits
    nn <- rep(nk, each = n_feats)
    out <- data.table::data.table(
        feats = rep(rownames(x), times = length(lvls)),
        group = rep(lvls, each = n_feats),
        n_cells = nn
    )
    pull <- function(nm) as.numeric(unlist(lapply(per, `[[`, nm)))

    if ("sum" %in% stats) {
        gene_sum <- pull("sum")
        out[, "total_expr" := gene_sum]
        out[, "mean_expr" := pull("mean")]

        if ("sumsq" %in% stats) {
            gene_sumsq <- pull("sumsq")
            out[, "sumsq" := gene_sumsq]
            # clamped: the subtraction can go slightly negative when the mean
            # dominates the spread, and a negative variance would surface as
            # an NaN standard deviation
            gene_var <- ifelse(nn > 1,
                pmax((gene_sumsq - gene_sum * gene_sum / nn) / (nn - 1), 0), 0
            )
            out[, "sd" := sqrt(gene_var)]
        }
    }
    if ("nnz" %in% stats) {
        gene_nnz <- pull("nnz")
        out[, "nr_cells" := as.integer(gene_nnz)]
        out[, "perc_cells" := ifelse(nn > 0, gene_nnz / nn * 100, NaN)]

        if ("sum_det" %in% stats) {
            out[, "mean_expr_det" := ifelse(
                gene_nnz > 0, pull("sum_det") / gene_nnz, NaN
            )]
        }
    }

    out[]
}


# Mean over detected values only, per feature. NaN where nothing is detected.
.mean_expr_det_test <- function(mymatrix, detection_threshold = 1) {
    if (inherits(mymatrix, "IterableMatrix")) {
        count_detected <- rowSums_flex(mymatrix > detection_threshold)
        if (detection_threshold > 0) {
            # BPCells does not support IterableMatrix * IterableMatrix. Use
            # sum(x[x > t]) = rowSums(x) - rowSums(pmin(x, t)) + count * t.
            # `min_scalar` requires t > 0, which this branch guarantees.
            sum_detected <- rowSums_flex(mymatrix) -
                rowSums_flex(BPCells::min_scalar(mymatrix, detection_threshold)) +
                count_detected * detection_threshold
        } else {
            # expression values are non-negative; all stored values pass
            sum_detected <- rowSums_flex(mymatrix)
        }
    } else {
        mask <- mymatrix > detection_threshold
        sum_detected <- rowSums_flex(mymatrix * mask)
        count_detected <- rowSums_flex(mask)
    }

    out <- sum_detected / count_detected
    out[count_detected == 0] <- NaN
    out
}
