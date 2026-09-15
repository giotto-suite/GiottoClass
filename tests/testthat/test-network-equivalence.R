# Equivalence guards for the network-construction rewrites. Each change under
# test is meant to be a no-op on results, so these check against an independent
# reference rather than a stored snapshot: the previous implementation inlined
# below, a brute-force answer, or the other backend.
#
# Construction through the public API lives in test_10_create_network.R;
# accessors, carriers and param classes live in test-networks.R.

# The implementation edge_distances() had before it was vectorized: one
# stats::dist() call per edge, through a 2 x d x E array. Kept here so the
# equivalence claim is checked rather than asserted.
.ref_edge_distances <- function(x, y) {
    a <- array(dim = c(nrow(y), ncol(x), 2))
    a[, , 1] <- x[y$from, ]
    a[, , 2] <- x[y$to, ]
    a <- aperm(a, perm = c(3, 2, 1))
    vapply(seq(dim(a)[3L]),
        function(i) stats::dist(a[, , i]),
        FUN.VALUE = numeric(1L)
    )
}

# Not bit-identical, and the test says so: stats::dist() and
# sqrt(rowSums(...)) accumulate in a different order, so results differ in the
# last ulp (~1e-16 absolute). That is small enough not to matter except for an
# edge sitting exactly on a maximum_distance cutoff, which is the same
# tolerance question the dbscan distances already raise.
test_that("edge_distances agrees with the per-edge implementation to ~1e-15", {
    set.seed(1)
    for (d in 2:3) {
        x <- matrix(runif(400 * d), ncol = d)
        y <- data.table::data.table(
            from = sample(400, 900, TRUE), to = sample(400, 900, TRUE)
        )
        expect_equal(
            edge_distances(x, y), .ref_edge_distances(x, y),
            tolerance = 1e-14, info = paste0(d, "D")
        )
    }
})

test_that("edge_distances handles the degenerate shapes", {
    x <- matrix(c(0, 0, 3, 4, 3, 4), ncol = 2, byrow = TRUE)

    # a single edge
    one <- data.table::data.table(from = 1L, to = 2L)
    expect_identical(edge_distances(x, one), 5)

    # duplicate points -> distance 0, not NA
    dup <- data.table::data.table(from = 2L, to = 3L)
    expect_identical(edge_distances(x, dup), 0)

    # no edges at all
    none <- data.table::data.table(from = integer(), to = integer())
    expect_identical(edge_distances(x, none), numeric(0))

    # 1D coordinates
    x1 <- matrix(c(0, 7), ncol = 1)
    expect_identical(
        edge_distances(x1, data.table::data.table(from = 1L, to = 2L)), 7
    )
})

test_that("edge_distances honours x_node_ids", {
    # this silently ignored x_node_ids and errored on any character-indexed
    # edge table, which is exactly what the argument exists for
    x <- matrix(c(0, 0, 3, 4), ncol = 2, byrow = TRUE)
    ids <- c("a", "b")
    y <- data.table::data.table(from = "a", to = "b")

    expect_identical(edge_distances(x, y, x_node_ids = ids), 5)
    expect_error(edge_distances(x, y), "indexed by node ID")

    # and it must not modify the caller's table by reference
    y2 <- data.table::data.table(from = "a", to = "b")
    invisible(edge_distances(x, y2, x_node_ids = ids))
    expect_type(y2$from, "character")
})

test_that("kNN distances no longer depend on whether a cutoff was asked for", {
    # the maximum_distance branch used to recompute distances from coordinates
    # instead of using the ones dbscan returned; the two must agree
    set.seed(4)
    x <- matrix(runif(300 * 2, 0, 100), ncol = 2)
    a <- GiottoClass:::.net_dt_knn(x, k = 5L, filter = FALSE)
    b <- GiottoClass:::.net_dt_knn(x, k = 5L, filter = FALSE,
        maximum_distance = 1e9
    )
    expect_equal(a$distance, b$distance, tolerance = 1e-12)
    expect_equal(nrow(a), nrow(b))
})

test_that("deldir and geometry produce the same Delaunay graph", {
    skip_if_not_installed("geometry")
    set.seed(9)
    x <- matrix(runif(600 * 2, 0, 100), ncol = 2)
    key <- function(dt) {
        paste0(pmin(dt$from, dt$to), "|", pmax(dt$from, dt$to))
    }
    a <- GiottoClass:::.net_dt_del_deldir(x)$delaunay_network_DT
    b <- GiottoClass:::.net_dt_del_geometry(x)$delaunay_network_DT
    expect_setequal(key(a), key(b))
    expect_equal(nrow(a), nrow(b))
})

# --- radius network --------------------------------------------------------

test_that("the radius network matches a brute-force answer", {
    set.seed(6)
    n <- 250L
    x <- matrix(runif(n * 2, 0, 100), ncol = 2)
    eps <- 12

    dt <- GiottoClass:::.net_dt_radius(x, eps = eps)

    full <- as.matrix(stats::dist(x))
    want <- which(full <= eps & upper.tri(full), arr.ind = TRUE)
    want_key <- sort(paste0(
        pmin(want[, 1], want[, 2]), "|", pmax(want[, 1], want[, 2])
    ))
    got_key <- sort(paste0(pmin(dt$from, dt$to), "|", pmax(dt$from, dt$to)))

    expect_identical(got_key, want_key)
    expect_true(all(dt$distance <= eps + 1e-9))
})

test_that("the radius network is undirected and self-loop free", {
    set.seed(7)
    x <- matrix(runif(200 * 2, 0, 100), ncol = 2)
    dt <- GiottoClass:::.net_dt_radius(x, eps = 15)
    expect_true(all(dt$from < dt$to)) # canonical, one row per pair
    expect_false(any(dt$from == dt$to))
    expect_equal(anyDuplicated(paste0(dt$from, "|", dt$to)), 0L)
})

test_that("minimum_k rescues nodes that eps leaves isolated", {
    # two tight clusters far apart, plus one outlier beyond eps of everything
    x <- rbind(
        matrix(c(0, 0, 1, 0, 0, 1, 1, 1), ncol = 2, byrow = TRUE),
        matrix(c(500, 500), ncol = 2)
    )
    bare <- GiottoClass:::.net_dt_radius(x, eps = 3)
    expect_false(5L %in% c(bare$from, bare$to)) # the outlier has no edges

    rescued <- GiottoClass:::.net_dt_radius(x, eps = 3, minimum_k = 1L)
    expect_true(5L %in% c(rescued$from, rescued$to))
    # and the within-eps edges are all still there
    expect_true(all(
        paste0(bare$from, "|", bare$to) %in%
            paste0(rescued$from, "|", rescued$to)
    ))
})
