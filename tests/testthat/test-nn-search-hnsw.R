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

test_that("the default build is reproducible", {
    # n_threads_build = 1 is the default precisely so repeated calls agree:
    # a multithreaded build varies with insertion order.
    expect_identical(hnswKNN(m, k = 15)$id, hnswKNN(m, k = 15)$id)
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
    expect_identical(kNNNetworkParam()@engine, "dbscan")
    expect_identical(sNNNetworkParam()@engine, "dbscan")
    expect_identical(eval(formals(createNearestNetwork)$engine)[1], "dbscan")
})

test_that("ef and n_threads_build are carried on the params and ignored by dbscan", {
    p <- kNNNetworkParam(k = 10, ef = 300, n_threads_build = 2L)
    expect_identical(p@ef, 300)
    expect_identical(p@n_threads_build, 2L)
    # inert under dbscan rather than an error, so engines can be swapped freely
    expect_no_error(
        createNetwork(m, kNNNetworkParam(k = 10, engine = "dbscan",
            ef = 300, n_threads_build = 2L, output = "data.table"))
    )
})
