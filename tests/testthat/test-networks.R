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


# Backend-aware auto-write on network setters. When the gobject has a
# gsource backend attached and the incoming network is in-mem (igraph),
# setNearestNetwork / setSpatialNetwork route through GiottoDisk to
# disk-back the @network slot as a parquetEdgeStore. Mirrors the
# setExpression / setPolygonInfo pattern.

test_that("setNearestNetwork auto-writes igraph to parquetEdgeStore on backed gobject", {
    skip_if_not_installed("GiottoDisk")
    rlang::local_options(lifecycle_verbosity = "quiet")

    gdir <- file.path(tempdir(), paste0("nn_autowrite_", basename(tempfile())))
    on.exit(unlink(gdir, recursive = TRUE), add = TRUE)
    mat <- matrix(rpois(20 * 50, 2), nrow = 50, ncol = 20,
                  dimnames = list(paste0("g_", 1:50),
                                  paste0("c_", 1:20)))
    gb <- createGiottoObject(expression = mat, backend = gdir)
    expect_false(is.null(gb@source))

    ig <- igraph::sample_gnm(20, 50, directed = FALSE)
    igraph::V(ig)$name <- paste0("c_", seq_len(20))
    nn <- methods::new("nnNetObj", network = ig, nn_type = "sNN",
        name = "sNN.test", spat_unit = "cell", feat_type = "rna",
        provenance = "cell")
    rlang::local_options(giotto.check_valid = FALSE)
    gb <- setNearestNetwork(gb, nn, verbose = FALSE)

    nn_back <- getNearestNetwork(gb, output = "nnNetObj",
        spat_unit = "cell", feat_type = "rna",
        nn_type = "sNN", name = "sNN.test")
    expect_s4_class(nn_back@network, "parquetEdgeStore")
})

test_that("setSpatialNetwork auto-writes igraph to parquetEdgeStore on backed gobject", {
    skip_if_not_installed("GiottoDisk")
    rlang::local_options(lifecycle_verbosity = "quiet")

    gdir <- file.path(tempdir(), paste0("sn_autowrite_", basename(tempfile())))
    on.exit(unlink(gdir, recursive = TRUE), add = TRUE)
    mat <- matrix(rpois(20 * 50, 2), nrow = 50, ncol = 20,
                  dimnames = list(paste0("g_", 1:50),
                                  paste0("c_", 1:20)))
    sl_dt <- data.table::data.table(
        cell_ID = paste0("c_", 1:20),
        sdimx = runif(20), sdimy = runif(20)
    )
    sl <- createSpatLocsObj(coordinates = sl_dt, spat_unit = "cell",
                            provenance = "cell")
    gb <- createGiottoObject(expression = mat, backend = gdir)
    gb <- setSpatialLocations(gb, sl, verbose = FALSE)

    ig <- igraph::sample_gnm(20, 50, directed = FALSE)
    igraph::V(ig)$name <- paste0("c_", seq_len(20))
    sn <- methods::new("spatialNetworkObj", network = ig,
        name = "delaunay.network", spat_unit = "cell", provenance = "cell")
    rlang::local_options(giotto.check_valid = FALSE)
    gb <- setSpatialNetwork(gb, sn, verbose = FALSE)

    sn_back <- getSpatialNetwork(gb, output = "spatialNetworkObj",
        spat_unit = "cell", name = "delaunay.network")
    expect_s4_class(sn_back@network, "parquetEdgeStore")
})

test_that("network setters leave in-mem igraphs untouched on unbacked gobject", {
    skip_if_not_installed("GiottoDisk")
    rlang::local_options(lifecycle_verbosity = "quiet")

    mat <- matrix(rpois(20 * 50, 2), nrow = 50, ncol = 20,
                  dimnames = list(paste0("g_", 1:50),
                                  paste0("c_", 1:20)))
    gb <- createGiottoObject(expression = mat)
    expect_null(gb@source)

    ig <- igraph::sample_gnm(20, 50, directed = FALSE)
    igraph::V(ig)$name <- paste0("c_", seq_len(20))
    nn <- methods::new("nnNetObj", network = ig, nn_type = "sNN",
        name = "sNN.test", spat_unit = "cell", feat_type = "rna",
        provenance = "cell")
    rlang::local_options(giotto.check_valid = FALSE)
    gb <- setNearestNetwork(gb, nn, verbose = FALSE)

    nn_back <- getNearestNetwork(gb, output = "nnNetObj",
        spat_unit = "cell", feat_type = "rna",
        nn_type = "sNN", name = "sNN.test")
    expect_s3_class(nn_back@network, "igraph")  # not promoted
})

options("lifecycle_verbosity" = lifecycle_opt)