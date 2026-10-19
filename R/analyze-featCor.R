#' @include classes-utils.R
#' @include generics.R
NULL

# ============================================================================
# featCorParam + analyzeData methods.
#
# Feature x feature correlation. Pearson is computed from moment sums
# (x x^T, row sums), which is what a streaming backend accumulates, so a disk
# store needs only those sums to provide the same result
# (feat_cor_from_moments()). With `smooth =` the values are first smoothed
# over a spatial network (spatialSmoothParam); a backend can then fuse the
# two and never store the smoothed values.
#
#   analyzeData(allMatrix, featCorParam, network =)   # in memory
# ============================================================================


# class ####

#' @name featCorParam-class
#' @title Feature correlation parameter
#' @description
#' Parameter class for [analyzeData()] returning the feature x feature
#' correlation matrix. See [featCorParam()].
#' @seealso [featCorParam()]
#' @exportClass featCorParam
setClass("featCorParam", contains = "analyzeParam")


# constructor ####

#' @name featCorParam
#' @title Correlation between features
#' @description
#' Parameters for [analyzeData()] to correlate every pair of features (rows)
#' across cells (columns).
#' @param method character. `"pearson"` (default) or `"spearman"`.
#' @param smooth optional [spatialSmoothParam()]: correlate the values after
#' smoothing them over the spatial network passed to [analyzeData()] as
#' `network`
#' @param ... additional named entries to attach to `@param`.
#' @returns A `featCorParam` object.
#' @details A feature with the same value in every cell has no defined
#' correlation; its row and column are `NA`.
#' @examples
#' m <- matrix(rpois(60, 3), nrow = 3,
#'     dimnames = list(c("a", "b", "c"), NULL))
#' analyzeData(m, featCorParam())
#' @export
featCorParam <- function(method = c("pearson", "spearman"), smooth = NULL,
    ...) {
    method <- match.arg(method)
    checkmate::assert_class(smooth, "spatialSmoothParam", null.ok = TRUE)
    p <- new("featCorParam", param = list(...))
    p$method <- method
    p$smooth <- smooth
    p
}


# analyzeData(allMatrix, featCorParam) ####

#' @rdname analyzeData
#' @param network for a `featCorParam` with `smooth`: the spatial network to
#' smooth over
#' @export
setMethod("analyzeData",
    signature(x = "allMatrix", param = "featCorParam"),
    function(x, param, ..., network = NULL) {
        if (!is.null(param$smooth)) {
            if (is.null(network)) {
                stop("[featCorParam] `smooth` needs a `network`.",
                    call. = FALSE)
            }
            x <- processData(x, param$smooth, network = network)
        }
        if (identical(param$method, "spearman")) {
            x <- t(apply(as.matrix(x), 1L, rank))
        }
        m <- .gram_moments(x)
        feat_cor_from_moments(m$G, m$s, ncol(x), rownames(x))
    }
)


# internals ####

# x x^T and the row sums, over dense blocks of cells: a dense product is
# far faster than a sparse one once the values are dense (as smoothed values
# are), and the blocks bound the memory
.gram_moments <- function(x, block_values = 5e7) {
    P <- nrow(x)
    G <- matrix(0, P, P)
    s <- numeric(P)
    step <- max(1L, as.integer(block_values %/% max(1L, P)))
    for (a in seq(1L, ncol(x), by = step)) {
        blk <- as.matrix(x[, a:min(ncol(x), a + step - 1L), drop = FALSE])
        G <- G + tcrossprod(blk)
        s <- s + rowSums(blk)
    }
    list(G = G, s = s)
}

#' @title Feature correlation from moment sums
#' @name feat_cor_from_moments
#' @description The Pearson correlation between features from \eqn{X X^T},
#' the row sums of \eqn{X} and the number of columns -- the sums a streaming
#' backend accumulates for [featCorParam()]. Features without variance get
#' `NA`.
#' @param G features x features matrix \eqn{X X^T}
#' @param s per-feature sums
#' @param n number of cells
#' @param names feature names
#' @returns a features x features correlation matrix
#' @examples
#' x <- matrix(rnorm(30), 3)
#' feat_cor_from_moments(tcrossprod(x), rowSums(x), ncol(x))
#' @export
feat_cor_from_moments <- function(G, s, n, names = NULL) {
    cov <- (G - tcrossprod(s) / n) / (n - 1)
    v <- diag(cov)
    sdv <- sqrt(ifelse(v > 0, v, NA_real_))
    r <- cov / outer(sdv, sdv)
    r <- pmin(pmax(r, -1), 1)
    diag(r)[!is.na(sdv)] <- 1
    dimnames(r) <- list(names, names)
    r
}
