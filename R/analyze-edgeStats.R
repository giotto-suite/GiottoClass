#' @include classes-virtuals.R
#' @include classes-utils.R
#' @include generics.R
NULL

# ============================================================================
# edgeStatsParam + analyzeData method.
#
# The per-feature sums over a spatial network that every edge statistic is a
# function of -- Moran's I, Geary's C and join counts all are. Like featStats,
# arithmetic with no analysis choice in it, so it lives next to the generic and
# defines a contract each backend reproduces; the statistics themselves are
# computed from the contract in Giotto, once, whatever the backend.
#
# Contract: a data.table, one row per feature,
#   feats      feature ID
#   sum        sum_i x_i            sumsq  sum_i x_i^2
#   sum3       sum_i x_i^3          sum4   sum_i x_i^4
#   deg_sum    sum_i d_i x_i        deg_sumsq  sum_i d_i x_i^2
#   edge_prod  sum_e w_e x_i x_j    over the edges, each once
# with attribute "graph" = list(n_cells, n_edges, sum_w, sum_w2, sum_deg2).
# i runs over the cells of the data; an edge counts only when both its ends
# are among them, and once per pair of cells whichever way round it is listed
# (a kNN network lists a mutual pair in both directions). d_i is the summed weight of cell i's edges (its degree when
# unweighted), w_e the edge weight (1 when unweighted).
#
# With a cell grouping (`groups =`, K groups), attribute "grouped" adds what
# a null conditioned on the grouping needs -- matrices, rows named by feature
# or group, columns by group:
#   levels     the K group labels          n_cells  cells per group
#   sum, sumsq, sum3, sum4                 G x K: power sums within each group
#   nbr_sum    G x K: sum_i x_i * a_il, a_il the summed weight of cell i's
#              edges to cells of group l
#   edges      K x K: sum over cells i of group k of a_il
#   paths      K x K x K: [k, l, m] = sum over cells i of group k of a_il a_im
# ============================================================================


# classes ####

#' @name edgeStatsParam-class
#' @title Edge statistics parameters
#' @description
#' Parameter class for [analyzeData()] that computes, per feature, the sums
#' over a spatial network from which edge statistics such as Moran's I,
#' Geary's C and join counts are derived. Build it with [edgeStatsParam()].
#' @seealso [edgeStatsParam()]
#' @exportClass edgeStatsParam
setClass("edgeStatsParam", contains = "analyzeParam")


# factory ####

#' @title Edge statistics parameters
#' @name edgeStatsParam
#' @description
#' Settings for [analyzeData()] that compute per-feature sums over a spatial
#' network: power sums of the values, degree-weighted sums, and the sum over
#' edges of the product of the values at both ends.
#' @param weighted logical. Use the network's `weight` column (`TRUE`) or
#'   count every edge as 1 (`FALSE`).
#' @param ... additional named entries to attach to `@param`.
#' @returns An [edgeStatsParam-class] object.
#' @examples
#' m <- Matrix::rsparsematrix(4, 6, 0.6, rand.x = function(n) rpois(n, 3) + 1)
#' dimnames(m) <- list(paste0("g", 1:4), paste0("c", 1:6))
#' net <- data.frame(from = paste0("c", 1:5), to = paste0("c", 2:6))
#' analyzeData(m, edgeStatsParam(), network = net)
#' @export
edgeStatsParam <- function(weighted = FALSE, ...) {
    checkmate::assert_flag(weighted)
    p <- new("edgeStatsParam", param = list(...))
    p$weighted <- weighted
    p
}


# methods ####

#' @rdname analyzeData
#' @param network for an [edgeStatsParam-class]: the spatial network, as a
#' `data.frame` with `from` and `to` columns of cell IDs (plus `weight` when
#' weighted), one row per edge, or an `igraph`.
setMethod("analyzeData",
    signature(x = "allMatrix", param = "edgeStatsParam"),
    function(x, param, ..., network, groups = NULL) {
        weighted <- isTRUE(param$weighted)
        x <- methods::as(x, "CsparseMatrix")
        e <- .edge_list_idx(network, colnames(x), weighted)

        N <- ncol(x)
        deg <- .edge_degree_vec(e, N)
        x2 <- .pow_x(x, 2)
        out <- data.table::data.table(
            feats     = rownames(x),
            sum       = Matrix::rowSums(x),
            sumsq     = Matrix::rowSums(x2),
            sum3      = Matrix::rowSums(.pow_x(x, 3)),
            sum4      = Matrix::rowSums(.pow_x(x, 4)),
            deg_sum   = as.numeric(x %*% deg),
            deg_sumsq = as.numeric(x2 %*% deg),
            edge_prod = .edge_products(x, e)
        )
        data.table::setattr(out, "graph", list(
            n_cells = N, n_edges = nrow(e),
            sum_w = sum(e$w), sum_w2 = sum(e$w^2), sum_deg2 = sum(deg^2)
        ))
        if (!is.null(groups)) {
            data.table::setattr(out, "grouped",
                .edge_stats_grouped(x, e, .edge_align_groups(x, groups)))
        }
        out
    }
)


# internals ####

# Network -> data.table(from, to, w) of column indices of the data, keeping
# only edges with both ends among `ids`.
.edge_list_idx <- function(network, ids, weighted) {
    from <- to <- NULL  # NSE
    if (inherits(network, "igraph")) {
        el <- igraph::as_data_frame(network, what = "edges")
    } else {
        el <- as.data.frame(network)
    }
    if (!all(c("from", "to") %in% names(el))) {
        stop("[edgeStatsParam] `network` needs `from` and `to` columns.",
            call. = FALSE)
    }
    if (weighted && !"weight" %in% names(el)) {
        stop("[edgeStatsParam] weighted = TRUE but `network` has no ",
            "`weight` column.", call. = FALSE)
    }
    a <- match(as.character(el$from), ids)
    b <- match(as.character(el$to), ids)
    e <- data.table::data.table(
        from = pmin(a, b), to = pmax(a, b),
        w = if (weighted) as.numeric(el$weight) else 1
    )
    e <- e[!is.na(from) & !is.na(to)]
    # one row per pair of cells, the first listing's weight
    unique(e, by = c("from", "to"))
}

# x with each stored value raised to `k`: elementwise, without forming a
# sparse x sparse product
.pow_x <- function(x, k) {
    x@x <- x@x^k
    x
}

.edge_degree_vec <- function(e, N) {
    as.numeric(tapply(c(e$w, e$w), factor(c(e$from, e$to), levels = seq_len(N)),
        sum, default = 0))
}

# sum_e w_e x_i x_j per feature: with A the from -> to weight matrix, the
# row sums of x * (x A)
.edge_products <- function(x, e) {
    if (nrow(e) == 0L) return(numeric(nrow(x)))
    A <- Matrix::sparseMatrix(i = e$from, j = e$to, x = e$w,
        dims = c(ncol(x), ncol(x)))
    Matrix::rowSums(x * (x %*% A))
}

# Every cell needs a group: a conditioned null has no place for a cell without
# one. Matched by cell ID through .align_groups().
.edge_align_groups <- function(x, groups) {
    g <- .align_groups(x, groups)
    if (anyNA(g)) {
        stop("[edgeStatsParam] every cell needs a group; ", sum(is.na(g)),
            " have none.", call. = FALSE)
    }
    droplevels(if (is.factor(g)) g else factor(g))
}

.edge_stats_grouped <- function(x, e, g) {
    lv <- levels(g)
    k <- as.integer(g)
    K <- length(lv)
    N <- ncol(x)
    Z <- Matrix::sparseMatrix(i = seq_len(N), j = k, x = 1, dims = c(N, K))
    # weighted neighbour counts per group: a_il
    A <- Matrix::sparseMatrix(i = c(e$from, e$to), j = k[c(e$to, e$from)],
        x = c(e$w, e$w), dims = c(N, K))
    x2 <- .pow_x(x, 2)
    gm <- function(m) {
        m <- as.matrix(m)
        dimnames(m) <- list(rownames(x), lv)
        m
    }
    paths <- array(0, c(K, K, K), dimnames = list(lv, lv, lv))
    for (i in seq_len(K)) {
        sel <- k == i
        if (any(sel)) paths[i, , ] <- as.matrix(Matrix::crossprod(A[sel, , drop = FALSE]))
    }
    edges <- as.matrix(Matrix::crossprod(Z, A))
    dimnames(edges) <- list(lv, lv)
    list(
        levels = lv,
        n_cells = stats::setNames(as.numeric(tabulate(k, K)), lv),
        sum = gm(x %*% Z), sumsq = gm(x2 %*% Z),
        sum3 = gm(.pow_x(x, 3) %*% Z), sum4 = gm(.pow_x(x, 4) %*% Z),
        nbr_sum = gm(x %*% A),
        edges = edges,
        paths = paths
    )
}
