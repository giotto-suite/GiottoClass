# edgeStatsParam: the contract against a dense adjacency computation.

.es_fixture <- function(G = 5L, N = 12L, seed = 9L) {
    set.seed(seed)
    m <- Matrix::rsparsematrix(G, N, density = 0.5,
        rand.x = function(n) as.double(rpois(n, 3L) + 1L))
    ids <- paste0("c", seq_len(N))
    dimnames(m) <- list(paste0("g", seq_len(G)), ids)
    from <- c(1:11, 1, 3, 5)
    to <- c(2:12, 7, 9, 11)
    net <- data.frame(from = ids[from], to = ids[to],
                      weight = round(runif(length(from), 0.5, 2), 2))
    list(m = m, net = net, from = from, to = to)
}
.es_dense <- function(m, from, to, w) {
    N <- ncol(m)
    W <- matrix(0, N, N)
    W[cbind(from, to)] <- w
    x <- as.matrix(m)
    deg <- rowSums(W + t(W))
    list(edge_prod = rowSums((x %*% W) * x), deg_sum = as.numeric(x %*% deg),
         deg_sumsq = as.numeric(x^2 %*% deg), sum_deg2 = sum(deg^2))
}

test_that("edge statistics match a dense adjacency computation", {
    f <- .es_fixture()
    for (wt in c(FALSE, TRUE)) {
        w <- if (wt) f$net$weight else rep(1, nrow(f$net))
        got <- analyzeData(f$m, edgeStatsParam(weighted = wt), network = f$net)
        ref <- .es_dense(f$m, f$from, f$to, w)
        x <- as.matrix(f$m)
        expect_equal(got$feats, rownames(f$m))
        expect_equal(got$sum, unname(rowSums(x)))
        expect_equal(got$sum4, unname(rowSums(x^4)))
        expect_equal(got$edge_prod, unname(ref$edge_prod))
        expect_equal(got$deg_sum, unname(ref$deg_sum))
        expect_equal(got$deg_sumsq, unname(ref$deg_sumsq))
        gr <- attr(got, "graph")
        expect_equal(gr$n_cells, ncol(f$m))
        expect_equal(gr$n_edges, nrow(f$net))
        expect_equal(gr$sum_w, sum(w))
        expect_equal(gr$sum_w2, sum(w^2))
        expect_equal(gr$sum_deg2, ref$sum_deg2)
    }
})

test_that("edges with an end outside the data are left out", {
    f <- .es_fixture()
    keep <- 1:8
    sub <- f$m[, keep]
    got <- analyzeData(sub, edgeStatsParam(), network = f$net)
    ok <- f$from %in% keep & f$to %in% keep
    ref <- .es_dense(sub, f$from[ok], f$to[ok], rep(1, sum(ok)))
    expect_equal(got$edge_prod, unname(ref$edge_prod))
    expect_equal(attr(got, "graph")$n_edges, sum(ok))
})

test_that("an igraph network gives the same result", {
    f <- .es_fixture()
    g <- igraph::graph_from_data_frame(f$net, directed = FALSE)
    expect_equal(analyzeData(f$m, edgeStatsParam(weighted = TRUE), network = g),
                 analyzeData(f$m, edgeStatsParam(weighted = TRUE), network = f$net))
    expect_error(analyzeData(f$m, edgeStatsParam(weighted = TRUE),
                             network = f$net[, 1:2]), "no `weight`")
})
