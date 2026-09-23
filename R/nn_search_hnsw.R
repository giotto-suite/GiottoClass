# nn_search_hnsw ####
# Approximate nearest-neighbor search for network construction.
#
# GiottoClass builds kNN / sNN networks through `.net_dt_knn()` /
# `.net_dt_snn()`, both of which call `dbscan::kNN()`. That is exact and
# single-threaded, and it degrades toward brute force as dimensionality rises
# -- at the ~15-50 PCs a network is normally built on it is the dominant cost
# of `createNearestNetwork()`.
#
# `hnswKNN()` is the drop-in alternative: an HNSW index (hnswlib, via
# RcppHNSW) built once and queried in one shot, parallel over `n_threads`.
# It returns the same shape `dbscan::kNN()` does -- a `c("kNN", "NN")` object
# carrying `id` / `dist` / `k` / `sort` / `metric` -- so `dbscan::sNN()`
# consumes it directly and nothing downstream of the search changes.
#
# The trade is exactness: HNSW is approximate, so recall is high but below
# 1.0. It is therefore opt-in (`engine = "hnsw"` on the network params), never
# the default -- `dbscan::kNN()` is exact and faster on small data, where the
# index build is pure overhead.
#
# Reproducibility: only the BUILD is nondeterministic. Concurrent insertion
# makes the graph depend on thread interleaving, while the search is read-only
# over a fixed index with independent queries. Measured on 158,662 Xenium cells
# (k = 30, ef = 200): the same index searched twice multithreaded gives
# identical ids, while two multithreaded builds searched single-threaded do
# not.
#
# The build is therefore ALWAYS single-threaded, and that is not adjustable.
# It was briefly an argument (`n_threads_build`); it is not one any more,
# because the failure it invites is silent and severe. On a 169,528-cell Atera
# section, two runs of an otherwise identical script with a parallel build
# disagreed on the number of cell types -- 34 against 33, ARI 0.7998 -- while
# two runs with the serial build were bit-identical. No seed can recover that:
# the interleaving is not drawn from any RNG, so there is nothing to pin. An
# argument that can only be set to a wrong value is better not offered.
#
# The search stays parallel over `n_threads`, which is safe and is where the
# time goes. Cost of the serial build: 2.82s -> 11.8s, still 6.8x faster than
# the 79.85s exact search, with accuracy untouched (recall 0.999980,
# undirected Jaccard 0.99995).

#' @title Approximate k-nearest neighbors via HNSW
#' @name hnswKNN
#' @description
#' Find the `k` nearest neighbors of every row of `x` using an HNSW index
#' (Hierarchical Navigable Small World), returning the same structure as
#' [dbscan::kNN()] so the two are interchangeable as the search step of
#' network construction.
#'
#' HNSW is *approximate*: recall is high but not guaranteed to be 1.0. Use
#' [dbscan::kNN()] when exactness matters, or on small data where the exact
#' search is both faster and exact.
#'
#' Repeated calls are reproducible. The index build is the only
#' nondeterministic phase -- concurrent insertion makes the graph depend on
#' thread interleaving -- so it always runs on one thread and this cannot be
#' changed. Searching is unaffected and stays parallel over `n_threads`.
#'
#' @param x numeric matrix. Rows are observations (cells), columns are
#'   dimensions (typically PCA coordinates).
#' @param k integer. Number of neighbors to return per observation, excluding
#'   the observation itself.
#' @param distance character. Metric, one of `"euclidean"` (default),
#'   `"cosine"`, `"l2"` (squared euclidean) or `"ip"` (inner product).
#' @param M integer. HNSW graph degree (default 16). Higher improves recall
#'   at the cost of memory and build time.
#' @param ef_construction integer. Beam width during index construction
#'   (default 200). Higher improves recall at the cost of build time.
#' @param ef integer. Beam width during search (default 200). This is the
#'   recall/speed dial: higher `ef` searches more of the graph, bringing the
#'   result closer to the exact [dbscan::kNN()] answer at the cost of query
#'   time. Raised to at least `k + 1`. On 158,662 cells at `k = 30`, `ef = 50`
#'   reproduced 99.225% of the exact network's undirected edges and `ef = 200`
#'   reproduced 99.995%, for 2.30s against 2.83s.
#' @param n_threads integer. Threads for the **search**. Defaults to
#'   [GiottoUtils::determine_cores()]. The search is deterministic at any
#'   thread count, so this can be left parallel. The index **build** is not
#'   threaded and is not configurable; see Details.
#' @param ... unused, for signature compatibility with [dbscan::kNN()].
#' @returns object of class `c("kNN", "NN")` with elements `id` (integer
#'   matrix, `nrow(x)` x `k`), `dist` (numeric matrix, same shape), `k`,
#'   `sort` and `metric`. `id` and `dist` carry [dbscan::kNN()]'s dimnames
#'   convention: `rownames(x)` on the rows, `"1".."k"` on the columns.
#' @examples
#' \dontrun{
#' m <- matrix(rnorm(1000 * 20), nrow = 1000)
#' nn <- hnswKNN(m, k = 30)
#' str(nn$id)
#' }
#' @export
hnswKNN <- function(x,
    k,
    distance = c("euclidean", "cosine", "l2", "ip"),
    M = 16L,
    ef_construction = 200L,
    ef = 200L,
    n_threads = NULL,
    ...
) {
    GiottoUtils::package_check("RcppHNSW")
    distance <- match.arg(distance)

    # `...` is here for dbscan::kNN() signature compatibility, so it would
    # otherwise absorb `n_threads_build` in silence and leave the caller
    # believing they had enabled a parallel build. Say so instead.
    if ("n_threads_build" %in% names(list(...))) {
        warning("[hnswKNN] `n_threads_build` no longer exists; the index ",
                "build is always single-threaded, because a parallel build ",
                "is not reproducible and no seed can make it so. ",
                "`n_threads` still controls the search.", call. = FALSE)
    }

    if (!is.matrix(x)) x <- as.matrix(x)
    checkmate::assert_matrix(x, mode = "numeric")
    k <- as.integer(k)
    n <- nrow(x)
    if (k >= n) {
        stop("[hnswKNN] k (", k, ") must be less than nrow(x) (", n, ").",
             call. = FALSE)
    }

    n_threads <- as.integer(n_threads %null% GiottoUtils::determine_cores())
    # A self-query returns the point itself, so ask for one extra and drop it
    # below. ef must cover the widened request or recall degrades at the tail.
    k_query <- k + 1L
    ef <- max(as.integer(ef), k_query)

    # One thread, always. See the reproducibility note in the file header:
    # a parallel build races on insertion order, and the resulting graph
    # difference survives all the way to a different number of clusters.
    ann <- RcppHNSW::hnsw_build(x,
        distance = distance,
        M = as.integer(M),
        ef = as.integer(ef_construction),
        n_threads = 1L
    )
    res <- RcppHNSW::hnsw_search(x,
        ann = ann,
        k = k_query,
        ef = ef,
        n_threads = n_threads
    )

    keep <- .hnsw_drop_self(res$idx)
    out_dist <- .hnsw_compact(res$dist, keep, k)
    out_id <- .hnsw_compact(res$idx, keep, k, as_int = TRUE)

    # Match dbscan::kNN()'s dimnames convention: rows carry the input's
    # rownames (NULL when it has none), columns are always "1".."k". Without
    # this the two objects differ under identical() even when every value
    # agrees, which breaks the drop-in claim for any caller that compares or
    # relies on the labels.
    dn <- list(rownames(x), seq_len(k))
    dimnames(out_dist) <- dn
    dimnames(out_id) <- dn

    list_out <- list(
        dist = out_dist,
        id = out_id,
        k = k,
        sort = TRUE,
        metric = distance
    )
    structure(list_out, class = c("kNN", "NN"))
}


# Index of the entries to keep after removing each row's self-hit.
#
# The self-hit is normally column 1, but with duplicate coordinates it can
# land anywhere in the row, and with enough duplicates it can be missing
# entirely. So this locates it per row rather than assuming, and falls back to
# dropping the last (furthest) entry when it is absent -- which keeps every
# row exactly k wide either way.
#
# Returns a logical matrix in column-major order, suitable for indexing
# `idx` / `dist` directly.
.hnsw_drop_self <- function(idx) {
    n <- nrow(idx)
    keep <- matrix(TRUE, nrow = n, ncol = ncol(idx))
    self_col <- max.col(idx == seq_len(n), ties.method = "first")
    has_self <- idx[cbind(seq_len(n), self_col)] == seq_len(n)
    # absent self -> drop the furthest neighbor instead
    self_col[!has_self] <- ncol(idx)
    keep[cbind(seq_len(n), self_col)] <- FALSE
    keep
}


# Apply a per-row keep mask, dropping one entry per row.
#
# Done on the transpose: `keep` holds exactly k TRUEs per ROW, so `t(keep)`
# holds exactly k per COLUMN, and a column-major extract from `t(m)` yields
# each row's kept values contiguously. Extracting from `m` directly would
# read column-major across rows whose dropped position differs, so columns
# contribute unequal counts and the reshape silently misaligns rows -- which
# is invisible whenever the self-hit happens to be column 1 in every row.
.hnsw_compact <- function(m, keep, k, as_int = FALSE) {
    out <- t(matrix(t(m)[t(keep)], nrow = k, ncol = nrow(m)))
    if (as_int) storage.mode(out) <- "integer"
    out
}


# nn_to_uwot ####

#' @title Convert a kNN object to uwot's precomputed-neighbor format
#' @name nnToUwot
#' @description
#' Reshape a `c("kNN", "NN")` object -- from [hnswKNN()] or [dbscan::kNN()] --
#' into the `list(idx =, dist =)` that [uwot::umap()] and [uwot::umap2()]
#' accept as `nn_method`, so a graph built once can be handed to UMAP instead
#' of letting uwot run a second, independent search.
#'
#' @details
#' Two differences have to be reconciled, and uwot validates neither, so
#' getting either wrong corrupts the embedding silently rather than raising an
#' error:
#'
#' - uwot names the neighbor matrix `idx`; `kNN` objects name it `id`.
#' - uwot requires each observation to be its own first neighbor
#'   (`idx[, 1] == seq_len(n)`, `dist[, 1] == 0`), because it drops column 1
#'   when fitting the local-connectivity offset. Both [hnswKNN()] and
#'   [dbscan::kNN()] *remove* self-matches. Passing their output unchanged
#'   therefore discards every observation's true nearest neighbor and fits
#'   `rho` against a shifted distance set.
#'
#' The returned matrices are `k + 1` columns wide: the self column plus the
#' `k` neighbors. uwot ignores `n_neighbors` when given a graph, so that width
#' is what sets the neighborhood size.
#'
#' @param nn object of class `kNN`/`NN`, or a list with `id` and `dist`
#'   matrices of equal dimensions.
#' @returns list with `idx` (integer matrix, `n` x `k + 1`) and `dist`
#'   (numeric matrix, same shape), suitable as uwot's `nn_method`.
#' @examples
#' \dontrun{
#' m <- matrix(rnorm(1000 * 20), nrow = 1000)
#' nn <- hnswKNN(m, k = 29)
#' uwot::umap2(m, nn_method = nnToUwot(nn))
#' }
#' @export
nnToUwot <- function(nn) {
    id <- nn[["id"]]
    dist <- nn[["dist"]]
    if (!is.matrix(id) || !is.matrix(dist)) {
        stop("[nnToUwot] `nn` must carry `id` and `dist` matrices, as ",
             "returned by hnswKNN() or dbscan::kNN().", call. = FALSE)
    }
    if (!identical(dim(id), dim(dist))) {
        stop("[nnToUwot] `id` and `dist` must have identical dimensions. ",
             "Got ", paste(dim(id), collapse = " x "), " and ",
             paste(dim(dist), collapse = " x "), ".", call. = FALSE)
    }
    n <- nrow(id)

    # A graph that already carries self would become doubly self-referential,
    # and uwot would then drop a real neighbor instead. Refuse rather than
    # guess which convention the caller meant.
    if (n > 0L && all(id[, 1L] == seq_len(n))) {
        stop("[nnToUwot] `nn` already lists each observation as its own ",
             "first neighbor; expected a self-free kNN object.", call. = FALSE)
    }

    # uwot reads the distances positionally when fitting rho and sigma, so a
    # row that is not ascending yields wrong weights and no error.
    if (ncol(dist) > 1L &&
            any(dist[, -1L, drop = FALSE] <
                    dist[, -ncol(dist), drop = FALSE])) {
        stop("[nnToUwot] `dist` rows must be sorted ascending. Build the ",
             "neighbors with `sort = TRUE`.", call. = FALSE)
    }

    out_idx <- cbind(seq_len(n), id)
    storage.mode(out_idx) <- "integer"
    out_dist <- cbind(rep.int(0, n), dist)
    # uwot takes the embedding's row names from the graph. Set both margins
    # at once: assigning rownames alone on a matrix whose colnames are NULL
    # leaves a `list(NULL, NULL)` dimnames that is not identical() to the
    # same values with no dimnames at all.
    rn <- rownames(id)
    dn <- if (is.null(rn)) NULL else list(rn, NULL)
    dimnames(out_idx) <- dn
    dimnames(out_dist) <- dn

    list(idx = out_idx, dist = out_dist)
}
