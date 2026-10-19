#' @include classes-utils.R
#' @include generics.R
#' @include analyze-edgeStats.R
NULL

# ============================================================================
# spatialSmoothParam + processData methods.
#
# Each cell's values averaged with its spatial neighbours': S = X M with
# M = (1 - b) I + b P, P the column-normalized symmetric adjacency, applied
# `hops` times. The same operator labelProportionsParam applies to one-hot
# labels, applied to expression. Building block for spatial co-expression,
# neighbourhood-augmented features and pooled counts.
#
#   processData(allMatrix, spatialSmoothParam, network = )  # in memory
#   processData(giotto,    spatialSmoothParam)              # router
#
# Disk backends add a method for their expression store; the router passes
# the stored network through, so a GiottoDisk store and parquetEdgeStore
# dispatch there.
# ============================================================================


# class ####

#' @name spatialSmoothParam-class
#' @title Spatial smoothing parameter
#' @description
#' Parameter class for [processData()] that averages each cell's values with
#' those of its neighbours in a spatial network. See [spatialSmoothParam()].
#' @seealso [spatialSmoothParam()]
#' @exportClass spatialSmoothParam
setClass("spatialSmoothParam", contains = "processParam")


# constructor ####

#' @name spatialSmoothParam
#' @title Smooth expression over a spatial network
#' @description
#' Parameters for [processData()] to replace each cell's values with a
#' weighted average of its own values and its neighbours' mean:
#' \deqn{s_j = (1 - b) x_j + b \frac{1}{d_j} \sum_{i \sim j} w_{ij} x_i}
#' where \eqn{d_j} is the summed weight of cell \eqn{j}'s edges. Edges count
#' in both directions whatever the network stores; a pair listed twice keeps
#' the mean of its weights. Cells without neighbours keep their values.
#' @param smoothing numeric in `[0, 1]`, the weight \eqn{b} of the neighbours.
#' `NULL` (default) uses \eqn{1 - 1/k} with \eqn{k} the median number of
#' neighbours, so a cell weighs as much as one neighbour. `0` returns the
#' values unchanged.
#' @param weighted logical. Weight neighbours by the network's `weight`
#' column instead of equally.
#' @param hops integer. Apply the average this many times, reaching
#' neighbours up to `hops` edges away.
#' @param name character. Name of the expression values written by the
#' giotto method.
#' @param ... additional named entries to attach to `@param`.
#' @returns A `spatialSmoothParam` object.
#' @examples
#' spatialSmoothParam(smoothing = 0.5)
#' @export
spatialSmoothParam <- function(smoothing = NULL, weighted = FALSE, hops = 1L,
    name = "smoothed", ...) {
    checkmate::assert_number(smoothing, lower = 0, upper = 1, null.ok = TRUE)
    checkmate::assert_flag(weighted)
    checkmate::assert_count(hops, positive = TRUE)
    checkmate::assert_string(name)
    p <- new("spatialSmoothParam", param = list(...))
    p$smoothing <- smoothing
    p$weighted <- weighted
    p$hops <- as.integer(hops)
    p$name <- name
    p
}


# processData(allMatrix, spatialSmoothParam) ####

#' @rdname processData
#' @param network a spatial network: an `igraph` or a data.frame with
#' `from`, `to` (cell IDs) and, for `weighted = TRUE`, `weight`
#' @export
setMethod("processData",
    signature(x = "allMatrix", param = "spatialSmoothParam"),
    function(x, param, ..., network) {
        M <- .smooth_operator(network, colnames(x), param$smoothing,
            param$weighted)
        for (h in seq_len(param$hops)) x <- x %*% M
        x
    }
)


# processData(giotto, spatialSmoothParam) ####

#' @rdname processData
#' @param spat_unit,feat_type spatial unit and feature type
#' @param expression_values expression values to smooth
#' @param spatial_network_name spatial network to smooth over
#' @param feats features to smooth (default all)
#' @export
setMethod("processData",
    signature(x = "giotto", param = "spatialSmoothParam"),
    function(x, param, ..., spat_unit = NULL, feat_type = NULL,
        expression_values = "normalized",
        spatial_network_name = "Delaunay_network", feats = NULL) {
        spat_unit <- set_default_spat_unit(gobject = x, spat_unit = spat_unit)
        feat_type <- set_default_feat_type(gobject = x, spat_unit = spat_unit,
            feat_type = feat_type)
        eo <- getExpression(x, spat_unit = spat_unit, feat_type = feat_type,
            values = expression_values, output = "exprObj")
        net <- getSpatialNetwork(x, spat_unit = spat_unit,
            name = spatial_network_name, output = "spatialNetworkObj",
            verbose = FALSE)
        m <- eo[]
        if (!is.null(feats)) m <- m[.present_feats(m, feats), , drop = FALSE]
        eo[] <- processData(m, param, network = net[])
        objName(eo) <- param$name
        setGiotto(x, eo, verbose = FALSE)
    }
)


# internals ####

# operator over the cells `ids` of a network
.smooth_operator <- function(network, ids, b, weighted) {
    e <- .edge_list_idx(network, ids, weighted)
    # .edge_list_idx already gives one row per pair of cells
    spatial_smooth_operator(e$from, e$to, e$w, n = length(ids),
        smoothing = b, names = ids, dedupe = FALSE)
}

#' @title Spatial smoothing operator
#' @name spatial_smooth_operator
#' @description The cells x cells matrix \eqn{M} that [spatialSmoothParam()]
#' applies as \eqn{S = X M}: \eqn{M = (1 - b) I + b A D^{-1}}, with \eqn{A}
#' the symmetric weight matrix of the edges and \eqn{D} its column sums. A
#' cell without neighbours keeps a self weight of 1. For backends that apply
#' the smoothing themselves.
#' @param from,to integer cell indices (`1..n`) of the edges' ends; a pair
#' counts once whichever way and however often it is listed, at the mean of
#' its weights, and self edges are dropped
#' @param weight edge weights, recycled
#' @param n number of cells
#' @param smoothing neighbour weight \eqn{b}; `NULL` uses \eqn{1 - 1/k},
#' \eqn{k} the median number of neighbours
#' @param names optional cell IDs for the dimnames
#' @param dedupe logical. `FALSE` skips merging pairs listed more than once,
#' for callers whose pairs are already one per pair of cells, which saves a
#' pass over the edges on large networks
#' @returns a sparse `n x n` matrix
#' @examples
#' spatial_smooth_operator(c(1, 2), c(2, 3), n = 3, smoothing = 0.5)
#' @export
spatial_smooth_operator <- function(from, to, weight = 1, n, smoothing = NULL,
    names = NULL, dedupe = TRUE) {
    w <- NULL  # NSE
    weight <- rep_len(as.numeric(weight), length(from))
    if (isTRUE(dedupe)) {
        e <- data.table::data.table(from = pmin(from, to),
            to = pmax(from, to), w = weight)
        e <- e[e$from != e$to][, list(w = mean(w)), by = c("from", "to")]
        from <- e$from
        to <- e$to
        weight <- e$w
        rm(e)
    } else if (any(from == to)) {
        keep <- from != to
        from <- from[keep]
        to <- to[keep]
        weight <- weight[keep]
    }
    # one entry per (cell, neighbour) both ways and one per cell on the
    # diagonal, built in a single construction: no intermediate n x n matrices
    k <- tabulate(c(from, to), nbins = n)
    d <- as.numeric(k)
    if (any(weight != 1)) {
        rs <- rowsum(c(weight, weight), c(from, to))
        d[] <- 0
        d[as.integer(rownames(rs))] <- rs[, 1L]
    }
    b <- smoothing
    if (is.null(b)) {
        med <- stats::median(k[k > 0])
        b <- if (is.finite(med) && med > 0) 1 - 1 / med else 0
    }
    M <- Matrix::sparseMatrix(
        i = c(from, to, seq_len(n)),
        j = c(to, from, seq_len(n)),
        x = c(b * weight / d[to], b * weight / d[from],
            ifelse(d > 0, 1 - b, 1)),
        dims = c(n, n))
    if (!is.null(names)) dimnames(M) <- list(names, names)
    M
}

# features of x among `feats`, in the order given; stops on missing ones
.present_feats <- function(x, feats) {
    miss <- setdiff(feats, rownames(x))
    if (length(miss)) {
        stop("[spatialSmoothParam] features not found: ",
            paste(utils::head(miss, 5), collapse = ", "),
            if (length(miss) > 5) ", ...", call. = FALSE)
    }
    feats
}
