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
# not. `n_threads_build` therefore defaults to 1, which makes repeated calls
# bit-identical and a seeded Leiden fully reproducible (ARI 1.000000, against
# 0.9368-0.9655 with a parallel build). Cost: 2.82s -> 11.8s, still 6.8x faster
# than the 79.85s exact search, with accuracy untouched (recall 0.999980,
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
#' Repeated calls are reproducible by default. The index build is the only
#' nondeterministic phase -- concurrent insertion makes the graph depend on
#' thread interleaving -- so `n_threads_build` defaults to `1`. Searching is
#' unaffected and stays parallel.
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
#'   thread count, so this can be left parallel.
#' @param n_threads_build integer or `NULL`. Threads for the index **build**,
#'   default `1`. A multithreaded build is not reproducible: insertion order
#'   varies, so the graph and hence the neighbours differ slightly between
#'   runs, which propagates to clustering even with a fixed seed. Building on
#'   one thread makes repeated calls bit-identical. Set to `NULL` to inherit
#'   `n_threads` and trade reproducibility for speed while exploring --
#'   measured at 11.8s against 2.82s on 158,662 cells.
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
    n_threads_build = 1L,
    ...
) {
    GiottoUtils::package_check("RcppHNSW")
    distance <- match.arg(distance)

    if (!is.matrix(x)) x <- as.matrix(x)
    checkmate::assert_matrix(x, mode = "numeric")
    k <- as.integer(k)
    n <- nrow(x)
    if (k >= n) {
        stop("[hnswKNN] k (", k, ") must be less than nrow(x) (", n, ").",
             call. = FALSE)
    }

    n_threads <- as.integer(n_threads %null% GiottoUtils::determine_cores())
    n_threads_build <- as.integer(n_threads_build %null% n_threads)
    # A self-query returns the point itself, so ask for one extra and drop it
    # below. ef must cover the widened request or recall degrades at the tail.
    k_query <- k + 1L
    ef <- max(as.integer(ef), k_query)

    ann <- RcppHNSW::hnsw_build(x,
        distance = distance,
        M = as.integer(M),
        ef = as.integer(ef_construction),
        n_threads = n_threads_build
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
