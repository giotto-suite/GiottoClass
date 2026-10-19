#' @include classes-utils.R
#' @include generics.R
NULL

# ============================================================================
# featCorParam + analyzeData methods.
#
# Feature x feature correlation. Pearson is computed from moment sums
# (x x^T, row sums), which is what a streaming backend accumulates, so a disk
# store needs only those sums to provide the same result.
#
#   analyzeData(allMatrix, featCorParam)   # in memory
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
#' @param ... additional named entries to attach to `@param`.
#' @returns A `featCorParam` object.
#' @details A feature with the same value in every cell has no defined
#' correlation; its row and column are `NA`.
#' @examples
#' m <- matrix(rpois(60, 3), nrow = 3,
#'     dimnames = list(c("a", "b", "c"), NULL))
#' analyzeData(m, featCorParam())
#' @export
featCorParam <- function(method = c("pearson", "spearman"), ...) {
    method <- match.arg(method)
    p <- new("featCorParam", param = list(...))
    p$method <- method
    p
}


# analyzeData(allMatrix, featCorParam) ####

#' @rdname analyzeData
#' @export
setMethod("analyzeData",
    signature(x = "allMatrix", param = "featCorParam"),
    function(x, param, ...) {
        if (identical(param$method, "spearman")) {
            x <- t(apply(as.matrix(x), 1L, rank))
        }
        n <- ncol(x)
        s <- Matrix::rowSums(x)
        cov <- (as.matrix(Matrix::tcrossprod(x)) - tcrossprod(s) / n) / (n - 1)
        .cov_to_cor(cov)
    }
)


# internals ####

# covariance -> correlation; features without variance get NA
.cov_to_cor <- function(cov) {
    v <- diag(cov)
    sdv <- sqrt(ifelse(v > 0, v, NA_real_))
    r <- cov / outer(sdv, sdv)
    r <- pmin(pmax(r, -1), 1)
    diag(r)[!is.na(sdv)] <- 1
    dimnames(r) <- dimnames(cov)
    r
}
