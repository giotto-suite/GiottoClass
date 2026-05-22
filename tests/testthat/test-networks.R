# Ignore internal usage of deprecated accessors
lifecycle_opt <- getOption("lifecycle_verbosity")
options("lifecycle_verbosity" = "quiet")

# ignore conda
options("giotto.use_conda" = FALSE)


# load data to test
g <- GiottoData::loadGiottoMini("viz")
activeSpatUnit(g) <- "aggregate"


test_that("spatial weight matrix can be created", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    test <- createSpatialWeightMatrix(g, spat_unit = "aggregate", return_gobject = TRUE)
    mat <- getSpatialNetwork(test, spat_unit = "aggregate", name = "kNN_network")@misc$weight_matrix$spat_weights

    expect_true(inherits(mat, c("matrix", "Matrix")))
})


# Cross-implementation parity tests between the gobject wrappers
# (createSpatialNetwork / createNearestNetwork) and the canonical
# createNetwork() were removed when both paths were unified — coverage
# of the underlying behaviours is now in test_10_create_network.R.
# A small integration test per wrapper remains here to catch regressions
# in the wiring between the gobject method and createNetwork.

test_that("createSpatialNetwork(Delaunay) returns gobject with igraph-backed spatialNetworkObj", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    g2 <- createSpatialNetwork(g, method = "Delaunay", verbose = FALSE)
    sn <- getSpatialNetwork(g2, name = "Delaunay_network",
                            output = "spatialNetworkObj")
    expect_s4_class(sn, "spatialNetworkObj")
    expect_true(inherits(sn@network, "igraph"))
    expect_gt(igraph::ecount(sn@network), 0)
})

test_that("createSpatialNetwork(kNN) returns gobject with igraph-backed spatialNetworkObj", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    g2 <- createSpatialNetwork(g, method = "kNN", k = 4, verbose = FALSE)
    sn <- getSpatialNetwork(g2, name = "kNN_network",
                            output = "spatialNetworkObj")
    expect_s4_class(sn, "spatialNetworkObj")
    expect_true(inherits(sn@network, "igraph"))
    expect_gt(igraph::ecount(sn@network), 0)
})

test_that("createNearestNetwork returns gobject with igraph-backed nnNetObj", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    g2 <- createNearestNetwork(g, type = "kNN", dimensions_to_use = 1:10)
    nn <- getNearestNetwork(g2, nn_type = "kNN", name = "kNN.pca",
                            output = "nnNetObj")
    expect_s4_class(nn, "nnNetObj")
    expect_true(inherits(nn@network, "igraph"))
    expect_gt(igraph::ecount(nn@network), 0)
})

options("lifecycle_verbosity" = lifecycle_opt)