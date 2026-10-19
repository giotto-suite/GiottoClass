# a path a - b - c - d plus an isolated cell e
.ss_x <- function() {
    m <- matrix(c(1, 2, 4, 8, 16,
                  0, 1, 0, 1, 3), nrow = 2, byrow = TRUE,
        dimnames = list(c("g1", "g2"), c("a", "b", "c", "d", "e")))
    Matrix::Matrix(m, sparse = TRUE)
}
.ss_net <- function() {
    data.frame(from = c("a", "b", "c"), to = c("b", "c", "d"),
        weight = c(1, 2, 3))
}

# explicit reference: s_j = (1 - b) x_j + b * weighted mean of neighbours
.ss_ref <- function(x, net, b, weighted = FALSE) {
    x <- as.matrix(x)
    w <- if (weighted) net$weight else rep(1, nrow(net))
    out <- x
    for (j in colnames(x)) {
        nb <- c(net$to[net$from == j], net$from[net$to == j])
        wj <- c(w[net$from == j], w[net$to == j])
        if (length(nb)) {
            out[, j] <- (1 - b) * x[, j] +
                b * (x[, nb, drop = FALSE] %*% wj) / sum(wj)
        }
    }
    out
}

test_that("smoothing averages each cell with its neighbours in both directions", {
    x <- .ss_x()
    got <- processData(x, spatialSmoothParam(smoothing = 0.5),
        network = .ss_net())
    expect_equal(as.matrix(got), .ss_ref(x, .ss_net(), 0.5))
})

test_that("weights, isolated cells and smoothing = 0", {
    x <- .ss_x()
    got <- processData(x, spatialSmoothParam(smoothing = 0.4, weighted = TRUE),
        network = .ss_net())
    expect_equal(as.matrix(got), .ss_ref(x, .ss_net(), 0.4, weighted = TRUE))
    expect_equal(as.matrix(got)[, "e"], as.matrix(x)[, "e"])
    none <- processData(x, spatialSmoothParam(smoothing = 0),
        network = .ss_net())
    expect_equal(as.matrix(none), as.matrix(x))
})

test_that("a pair listed in both directions counts once", {
    x <- .ss_x()
    net <- .ss_net()
    both <- rbind(net, data.frame(from = net$to, to = net$from,
        weight = net$weight))
    expect_equal(
        processData(x, spatialSmoothParam(smoothing = 0.5), network = both),
        processData(x, spatialSmoothParam(smoothing = 0.5), network = net)
    )
})

test_that("the default weight is 1 - 1/median degree, and hops compose", {
    x <- .ss_x()
    # degrees a..d: 1 2 2 1 -> median 1.5
    got <- processData(x, spatialSmoothParam(), network = .ss_net())
    expect_equal(as.matrix(got), .ss_ref(x, .ss_net(), 1 - 1 / 1.5))
    two <- processData(x, spatialSmoothParam(smoothing = 0.5, hops = 2),
        network = .ss_net())
    one <- .ss_ref(x, .ss_net(), 0.5)
    expect_equal(as.matrix(two), .ss_ref(one, .ss_net(), 0.5))
})

test_that("the giotto method writes smoothed values for the chosen features", {
    g <- GiottoData::loadGiottoMini("visium", verbose = FALSE)
    feats <- head(featIDs(g), 5)
    g <- processData(g, spatialSmoothParam(smoothing = 0.5),
        spatial_network_name = "spatial_network", feats = feats)
    sm <- getExpression(g, values = "smoothed", output = "matrix")
    expect_identical(rownames(sm), feats)
    raw <- getExpression(g, values = "normalized", output = "matrix")[feats, ]
    net <- getSpatialNetwork(g, name = "spatial_network", output = "networkDT")
    expect_equal(as.matrix(sm),
        as.matrix(processData(raw, spatialSmoothParam(smoothing = 0.5),
            network = net)))
})
