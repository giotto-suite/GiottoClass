# hnswKNN() -- approximate kNN search used by engine = "hnsw".
#
# Restored to GiottoClass after GiottoDisk removed it (58253ba) without the
# move landing, which left createNearestNetwork(engine = "hnsw") erroring with
# "'hnswKNN' is not an exported object from 'namespace:GiottoDisk'".

skip_if_not_installed("RcppHNSW")

set.seed(1234)
m <- matrix(rnorm(2000 * 10), nrow = 2000)

test_that("hnswKNN returns the same structure as dbscan::kNN", {
    h <- hnswKNN(m, k = 15)
    d <- dbscan::kNN(m, k = 15, sort = TRUE)

    expect_s3_class(h, "kNN")
    expect_s3_class(h, "NN")
    expect_identical(class(h), class(d))
    expect_identical(dim(h$id), dim(d$id))
    expect_identical(dim(h$dist), dim(d$dist))
    expect_identical(h$k, 15L)
    expect_true(is.integer(h$id))
    # no self-hits: a point is never its own neighbour
    expect_false(any(h$id == seq_len(nrow(m))))
})

test_that("hnswKNN carries dbscan::kNN's dimnames convention", {
    # id/dist must be interchangeable with dbscan's, not merely value-equal:
    # rownames from the input, columns "1".."k". Without this the two differ
    # under identical() even when every neighbour agrees.
    rn <- paste0("cell", seq_len(nrow(m)))
    mn <- m; rownames(mn) <- rn

    h <- hnswKNN(mn, k = 15)
    d <- dbscan::kNN(mn, k = 15, sort = TRUE)
    expect_identical(dimnames(h$id), dimnames(d$id))
    expect_identical(dimnames(h$dist), dimnames(d$dist))
    expect_identical(rownames(h$id), rn)

    # and NULL rownames stay NULL, as dbscan does
    h0 <- hnswKNN(m, k = 15)
    d0 <- dbscan::kNN(m, k = 15, sort = TRUE)
    expect_null(rownames(h0$id))
    expect_identical(dimnames(h0$id), dimnames(d0$id))
})

test_that("hnswKNN recall is high against the exact search", {
    h <- hnswKNN(m, k = 15)
    d <- dbscan::kNN(m, k = 15, sort = TRUE)
    recall <- mean(vapply(
        seq_len(nrow(m)),
        function(i) sum(!is.na(match(d$id[i, ], h$id[i, ]))),
        integer(1)
    )) / 15
    expect_gt(recall, 0.99)
})

test_that("higher ef does not reduce recall", {
    d <- dbscan::kNN(m, k = 15, sort = TRUE)
    rec <- function(ef) {
        h <- hnswKNN(m, k = 15, ef = ef)
        mean(vapply(seq_len(nrow(m)),
            function(i) sum(!is.na(match(d$id[i, ], h$id[i, ]))),
            integer(1))) / 15
    }
    expect_gte(rec(200), rec(20))
})

test_that("the build is reproducible", {
    # The build runs on one thread precisely so repeated calls agree: a
    # multithreaded build varies with insertion order. This is the regression
    # guard for that, and the search is left parallel throughout.
    expect_identical(hnswKNN(m, k = 15)$id, hnswKNN(m, k = 15)$id)
    expect_identical(hnswKNN(m, k = 15)$dist, hnswKNN(m, k = 15)$dist)
})

test_that("k must be less than the number of observations", {
    expect_error(hnswKNN(m[1:10, ], k = 10), "must be less than")
})

test_that("duplicate coordinates still yield exactly k neighbours per row", {
    # the case .hnsw_drop_self() exists for: with duplicates the self-hit is
    # not necessarily column 1, and may be absent entirely.
    dup <- rbind(m[1:50, ], m[1:50, ], m[1:50, ])
    h <- hnswKNN(dup, k = 10)
    expect_identical(dim(h$id), c(nrow(dup), 10L))
    expect_false(anyNA(h$id))
    expect_false(any(h$id == seq_len(nrow(dup))))
})

test_that("engine = 'hnsw' runs through the network constructors", {
    knn <- createNetwork(m, kNNNetworkParam(k = 10, engine = "hnsw",
        output = "data.table"))
    expect_s3_class(knn, "data.table")
    expect_gt(nrow(knn), 0L)

    snn <- createNetwork(m, sNNNetworkParam(k = 10, engine = "hnsw",
        output = "data.table"))
    expect_s3_class(snn, "data.table")
})

test_that("dbscan remains the default engine", {
    expect_identical(kNNNetworkParam()$engine, "dbscan")
    expect_identical(sNNNetworkParam()$engine, "dbscan")
    expect_identical(eval(formals(createNearestNetwork)$engine)[1], "dbscan")
})

test_that("ef is carried on the params and ignored by dbscan", {
    p <- kNNNetworkParam(k = 10, ef = 300)
    expect_identical(p$ef, 300)
    # inert under dbscan rather than an error, so engines can be swapped freely
    expect_no_error(
        createNetwork(m, kNNNetworkParam(k = 10, engine = "dbscan",
            ef = 300, output = "data.table"))
    )
})

test_that("the index build cannot be made parallel", {
    # The serial build is the whole reproducibility guarantee: it takes no
    # seed, and none would help, because the interleaving of a parallel build
    # is not drawn from an RNG. So the knob is gone rather than defaulted --
    # on a 169,528-cell section, two runs of an identical script with a
    # parallel build disagreed on the number of clusters, 34 against 33.
    expect_false("n_threads_build" %in% names(formals(hnswKNN)))
    expect_false("n_threads_build" %in% names(formals(kNNNetworkParam)))
    expect_false("n_threads_build" %in% names(formals(sNNNetworkParam)))
    expect_false("n_threads_build" %in% names(formals(createNearestNetwork)))
    # `...` is kept for dbscan::kNN() compatibility, so it would swallow the
    # removed argument silently. Warn, so a caller who thinks they have
    # enabled a parallel build finds out they have not.
    expect_warning(hnswKNN(m, k = 15, n_threads_build = 8L),
                   "no longer exists")
    expect_identical(
        suppressWarnings(hnswKNN(m, k = 15, n_threads_build = 8L))$id,
        hnswKNN(m, k = 15)$id
    )
})

test_that("nnToUwot adds the self column uwot expects", {
    nn <- hnswKNN(m, k = 15)
    u <- nnToUwot(nn)

    expect_named(u, c("idx", "dist"))
    # k + 1 wide: uwot drops column 1 when fitting the local connectivity
    # offset, so the self entry has to be there or a real neighbour is lost
    expect_identical(dim(u$idx), c(nrow(m), 16L))
    expect_identical(dim(u$dist), dim(u$idx))
    expect_identical(u$idx[, 1L], seq_len(nrow(m)))
    expect_true(all(u$dist[, 1L] == 0))
    expect_type(u$idx, "integer")

    # the neighbours themselves are carried through untouched
    expect_identical(u$idx[, -1L, drop = FALSE],
                     matrix(as.integer(nn$id), nrow = nrow(m)))
    expect_equal(u$dist[, -1L, drop = FALSE],
                 matrix(nn$dist, nrow = nrow(m)))
})

test_that("nnToUwot refuses input it cannot interpret", {
    nn <- hnswKNN(m, k = 15)

    # already self-referential: adding another self column would push a real
    # neighbour out of the window uwot reads
    expect_error(nnToUwot(nnToUwot(nn)), "must carry `id` and `dist`")
    already <- list(id = cbind(seq_len(nrow(m)), nn$id),
                    dist = cbind(0, nn$dist))
    expect_error(nnToUwot(already), "already lists each observation")

    # unsorted distances give wrong weights and no error from uwot
    unsorted <- nn
    unsorted$dist <- unsorted$dist[, rev(seq_len(ncol(unsorted$dist)))]
    expect_error(nnToUwot(unsorted), "sorted ascending")

    expect_error(nnToUwot(list(id = 1:5, dist = 1:5)),
                 "must carry `id` and `dist`")
})

test_that("nnToUwot output is accepted by uwot", {
    skip_if_not_installed("uwot")
    nn <- hnswKNN(m, k = 15)
    emb <- uwot::umap2(m, nn_method = nnToUwot(nn), n_epochs = 10,
                       verbose = FALSE)
    expect_identical(dim(emb), c(nrow(m), 2L))
    expect_false(anyNA(emb))
})
