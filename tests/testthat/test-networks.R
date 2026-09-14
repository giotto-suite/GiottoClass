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

test_that("network setters plumb @type + @directed correctly to parquetEdgeStore", {
    skip_if_not_installed("GiottoDisk")
    rlang::local_options(lifecycle_verbosity = "quiet")

    gdir <- file.path(tempdir(), paste0("nn_typedir_", basename(tempfile())))
    on.exit(unlink(gdir, recursive = TRUE), add = TRUE)
    cells <- paste0("c_", 1:10)
    mat <- matrix(rpois(10 * 20, 2), nrow = 20, ncol = 10,
                  dimnames = list(paste0("g", 1:20), cells))
    sl_dt <- data.table::data.table(cell_ID = cells,
                                    sdimx = runif(10),
                                    sdimy = runif(10))
    sl <- createSpatLocsObj(coordinates = sl_dt, spat_unit = "cell",
                            provenance = "cell")
    g <- createGiottoObject(expression = mat, backend = gdir)
    g <- setSpatialLocations(g, sl, verbose = FALSE)

    # sNN — undirected
    ig_snn <- igraph::sample_gnm(10, 15, directed = FALSE)
    igraph::V(ig_snn)$name <- cells
    nn_snn <- methods::new("nnNetObj", network = ig_snn, nn_type = "sNN",
        name = "sNN.t", spat_unit = "cell", feat_type = "rna",
        provenance = "cell")
    # kNN — directed
    ig_knn <- igraph::sample_gnm(10, 15, directed = TRUE)
    igraph::V(ig_knn)$name <- cells
    nn_knn <- methods::new("nnNetObj", network = ig_knn, nn_type = "kNN",
        name = "kNN.t", spat_unit = "cell", feat_type = "rna",
        provenance = "cell")
    # spatial — undirected
    ig_sp <- igraph::sample_gnm(10, 15, directed = FALSE)
    igraph::V(ig_sp)$name <- cells
    sn <- methods::new("spatialNetworkObj", network = ig_sp,
        name = "delaunay.t", spat_unit = "cell", provenance = "cell")

    rlang::local_options(giotto.check_valid = FALSE)
    g <- setNearestNetwork(g, nn_snn, verbose = FALSE)
    g <- setNearestNetwork(g, nn_knn, verbose = FALSE)
    g <- setSpatialNetwork(g, sn, verbose = FALSE)

    snn_store <- getNearestNetwork(g, output = "nnNetObj",
        spat_unit = "cell", feat_type = "rna",
        nn_type = "sNN", name = "sNN.t")@network
    expect_equal(snn_store@type, "sNN")
    expect_false(snn_store@directed)

    knn_store <- getNearestNetwork(g, output = "nnNetObj",
        spat_unit = "cell", feat_type = "rna",
        nn_type = "kNN", name = "kNN.t")@network
    expect_equal(knn_store@type, "kNN")
    expect_true(knn_store@directed)

    sn_store <- getSpatialNetwork(g, output = "spatialNetworkObj",
        spat_unit = "cell", name = "delaunay.t")@network
    expect_equal(sn_store@type, "spatial")
    expect_false(sn_store@directed)
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


# spatIDs delegates to dataStore-backed @network -----------------------------
# When @network is a parquetEdgeStore (GiottoDisk), the spatIDs methods on
# nnNetObj / spatialNetworkObj must delegate via dispatch instead of calling
# igraph functions directly.

test_that("spatIDs(nnNetObj) delegates to parquetEdgeStore when @network is one", {
    skip_if_not_installed("GiottoDisk")
    rlang::local_options(lifecycle_verbosity = "quiet")

    gdir <- file.path(tempdir(), paste0("spatids_", basename(tempfile())))
    on.exit(unlink(gdir, recursive = TRUE), add = TRUE)
    src <- GiottoDisk::gDirSource(gdir)

    ig <- igraph::make_graph(c(1, 2, 2, 3, 3, 4, 4, 5), directed = FALSE)
    igraph::V(ig)$name <- letters[1:5]
    igraph::E(ig)$weight <- c(0.9, 0.7, 0.5, 0.3)
    igraph::E(ig)$distance <- 1 / igraph::E(ig)$weight

    pes <- GiottoDisk::sourceWrite(src, ig, type = "sNN")
    nn <- methods::new("nnNetObj", network = pes, nn_type = "sNN",
        name = "sNN.test")

    expect_s4_class(nn@network, "parquetEdgeStore")
    expect_setequal(spatIDs(nn), letters[1:5])
})

test_that("spatIDs(spatialNetworkObj) delegates to parquetEdgeStore when @network is one", {
    skip_if_not_installed("GiottoDisk")
    rlang::local_options(lifecycle_verbosity = "quiet")

    gdir <- file.path(tempdir(), paste0("spatids_sn_", basename(tempfile())))
    on.exit(unlink(gdir, recursive = TRUE), add = TRUE)
    src <- GiottoDisk::gDirSource(gdir)

    ig <- igraph::make_graph(c(1, 2, 2, 3, 3, 4), directed = FALSE)
    igraph::V(ig)$name <- letters[1:4]
    igraph::E(ig)$weight <- c(0.9, 0.7, 0.5)
    igraph::E(ig)$distance <- 1 / igraph::E(ig)$weight

    pes <- GiottoDisk::sourceWrite(src, ig, type = "spatial")
    sn <- methods::new("spatialNetworkObj", network = pes, name = "sn.test")

    expect_s4_class(sn@network, "parquetEdgeStore")
    expect_setequal(spatIDs(sn), letters[1:4])
})


# --- the in-memory igraph path -------------------------------------------
#
# `spatIDs()` read `@network` as the from/to data.table it held before 0.6.0,
# so it returned character(0) for every ordinary network. The disk-backed
# branch was covered and stayed correct; the canonical in-memory path had no
# test at all, which is how that went unnoticed for a release cycle.

.sn_fixture <- function(method = "Delaunay", n = 100L, seed = 7L, ...) {
    rlang::local_options(lifecycle_verbosity = "quiet",
                         .local_envir = parent.frame())
    set.seed(seed)
    locs <- data.table::data.table(
        cell_ID = sprintf("c%03d", seq_len(n)),
        sdimx = runif(n, 0, 500), sdimy = runif(n, 0, 500)
    )
    m <- matrix(rpois(6L * n, 5), nrow = 6L,
                dimnames = list(paste0("g", 1:6), locs$cell_ID))
    gg <- createGiottoObject(expression = m, spatial_locs = locs)
    gg <- createSpatialNetwork(gg, method = method, name = "n1", ...)
    getSpatialNetwork(gg, name = "n1")
}

test_that("spatIDs(spatialNetworkObj) returns the nodes of an in-memory network", {
    sn <- .sn_fixture()
    net <- sn[]
    expect_s3_class(net, "igraph")
    expect_gt(igraph::ecount(net), 0L)

    ids <- spatIDs(sn)
    expect_type(ids, "character")
    expect_equal(length(ids), igraph::vcount(net))
    expect_setequal(ids, names(igraph::V(net)))
    # every endpoint of every edge is among them
    ends <- igraph::as_data_frame(net, what = "edges")
    expect_true(all(c(ends$from, ends$to) %in% ids))
})



# as.igraph ####

test_that("as.igraph returns the graph the @network slot holds", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    g2 <- createSpatialNetwork(g, method = "Delaunay", verbose = FALSE)
    sn <- getSpatialNetwork(g2, name = "Delaunay_network",
                            output = "spatialNetworkObj")
    nn <- getNearestNetwork(g, output = "nnNetObj")

    # an accessor, not a construction -- identity, not merely equality
    expect_identical(igraph::as.igraph(sn), slot(sn, "network"))
    expect_identical(igraph::as.igraph(nn), slot(nn, "network"))
})

test_that("as.igraph re-dispatches when @network is backed", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    # stands in for a GiottoDisk store: any class registering its own
    # as.igraph method. GiottoClass must not need to name the backend.
    setClass("fakeBackedNet", representation(g = "ANY"))
    on.exit(removeClass("fakeBackedNet"), add = TRUE)
    registerS3method("as.igraph", "fakeBackedNet", function(x, ...) x@g,
        envir = asNamespace("igraph"))

    ring <- igraph::make_ring(7)
    g2 <- createSpatialNetwork(g, method = "Delaunay", verbose = FALSE)
    sn <- getSpatialNetwork(g2, name = "Delaunay_network",
                            output = "spatialNetworkObj")
    slot(sn, "network") <- new("fakeBackedNet", g = ring)

    expect_identical(igraph::as.igraph(sn), ring)
})


# carrier dispatch ####
#
# The container methods forward to whatever `@network` holds rather than
# testing for it, so a carrier is reached by registering a method on it. These
# cover the contract GiottoDisk's parquetEdgeStore relies on, without needing
# GiottoDisk installed -- the store-backed tests above skip whenever it is not.

# Stands in for a store: any class that is not an igraph and brings its own
# spatIDs method. Declared at file level because setMethod() resolves the
# class name against the generic's namespace, and a class created inside a
# test frame is not visible there.
setClass("fakeIdNet", representation(ids = "character"))
setMethod("spatIDs", "fakeIdNet", function(x, ...) x@ids)

test_that("spatIDs() reads an igraph carrier directly", {
    ring <- igraph::make_ring(4)
    igraph::V(ring)$name <- letters[1:4]

    expect_setequal(spatIDs(ring), letters[1:4])
    # isolated vertices are nodes of the graph and are reported
    expect_length(spatIDs(igraph::add_vertices(ring, 1, name = "e")), 5L)
})

test_that("spatIDs() on the containers forwards to the carrier", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    sn <- .sn_fixture()
    nn <- getNearestNetwork(g, output = "nnNetObj")

    expect_identical(spatIDs(sn), spatIDs(slot(sn, "network")))
    expect_identical(spatIDs(nn), spatIDs(slot(nn, "network")))
})

test_that("spatIDs() re-dispatches when @network is backed", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    sn <- .sn_fixture()
    slot(sn, "network") <- new("fakeIdNet", ids = c("x", "y", "z"))
    expect_identical(spatIDs(sn), c("x", "y", "z"))

    nn <- getNearestNetwork(g, output = "nnNetObj")
    slot(nn, "network") <- new("fakeIdNet", ids = c("x", "y"))
    expect_identical(spatIDs(nn), c("x", "y"))
})


# as.data.table ####

test_that("as.data.table returns the edge table of an in-memory network", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    sn <- .sn_fixture()
    nn <- getNearestNetwork(g, output = "nnNetObj")

    for (obj in list(sn, nn)) {
        dt <- data.table::as.data.table(obj)
        expect_s3_class(dt, "data.table")
        expect_true(all(c("from", "to") %in% names(dt)))
        expect_equal(nrow(dt), igraph::ecount(slot(obj, "network")))
    }
})

test_that("as.data.table reads @unfiltered, which holds a bare carrier", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    # createSpatialNetwork() leaves @unfiltered NULL; the slot is filled by
    # createSpatNetObj(), the legacy migration and the Seurat conversion. It
    # holds the graph directly rather than a subobject, so it cannot go
    # through as.data.table() -- hence the carrier-level reader.
    full <- igraph::make_ring(5)
    igraph::V(full)$name <- letters[1:5]
    trimmed <- igraph::delete_edges(full, igraph::E(full)[1])

    sn <- createSpatNetObj(network = trimmed, unfiltered = full,
                           name = "unf.test")
    unf <- slot(sn, "unfiltered")
    expect_s3_class(unf, "igraph")

    dt <- GiottoClass:::.network_as_dt(unf)
    expect_s3_class(dt, "data.table")
    expect_true(all(c("from", "to") %in% names(dt)))
    # the unfiltered graph is a superset of the filtered one
    expect_equal(nrow(dt), igraph::ecount(full))
    expect_gt(nrow(dt), nrow(data.table::as.data.table(sn)))
})

test_that("as.data.table re-dispatches when @network is backed", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    setClass("fakeDtNet", representation(dt = "ANY"))
    on.exit(removeClass("fakeDtNet"), add = TRUE)
    edges <- data.table::data.table(from = c("a", "b"), to = c("b", "c"))
    registerS3method("as.data.table", "fakeDtNet", function(x, ...) x@dt,
        envir = asNamespace("data.table"))

    sn <- .sn_fixture()
    slot(sn, "network") <- new("fakeDtNet", dt = edges)

    expect_identical(data.table::as.data.table(sn), edges)
})


options("lifecycle_verbosity" = lifecycle_opt)

# networkParam $ / $<- ####
#
# The param families are list-backed: state lives in @param and is reached with
# `$`, with .DollarNames driving autocomplete. networkParam was the one family
# that declared typed slots instead, so `$` returned nothing.

test_that("networkParam params are reachable with $", {
    p <- kNNNetworkParam(k = 30)
    expect_identical(p$k, 30L)
    expect_identical(p$engine, "dbscan")
    expect_identical(p$output, "auto")
    expect_null(p$not_a_param)
})

test_that("networkParam params are settable with $<-", {
    p <- kNNNetworkParam(k = 30)
    p$k <- 10L
    expect_identical(p$k, 10L)
    # extras land alongside, the way the other param families behave
    p$custom <- "x"
    expect_identical(p$custom, "x")
})

test_that(".DollarNames lists every param for autocomplete", {
    # maximum_distance defaults to NULL and so is not in @param, but it is a
    # param the class takes and completes on anyway
    expect_setequal(
        .DollarNames(kNNNetworkParam()),
        c("k", "filter", "maximum_distance", "minimum_k", "weight_fun",
          "include_weight", "include_distance", "output", "engine", "ef",
          "n_threads_build")
    )
    expect_false("maximum_distance" %in% names(kNNNetworkParam()@param))

    # params set beyond the signature are unioned in
    p <- kNNNetworkParam()
    p$custom <- 1
    expect_true(all(c("k", "custom") %in% .DollarNames(p)))
})

test_that(".DollarNames whitelists have not drifted from the constructors", {
    # The whitelists in methods-extract.R are maintained by hand. Built with
    # every param set to a non-NULL value, @param holds exactly the params the
    # constructor sets -- so the two should agree exactly. Catches both a param
    # added to a constructor and never whitelisted, and a stale entry left
    # behind after one is removed.
    params <- list(
        kNNNetworkParam(maximum_distance = 20),
        sNNNetworkParam(),
        delaunayNetworkParam()
    )
    for (p in params) {
        expect_setequal(.DollarNames(p), names(p@param))
    }
    expect_true(all(
        c("method", "options", "Y", "j", "S") %in%
            .DollarNames(delaunayNetworkParam())
    ))
    expect_true(all(
        c("top_shared", "minimum_shared") %in% .DollarNames(sNNNetworkParam())
    ))
})

test_that("a NULL param reads back as NULL", {
    # Assigning NULL drops the entry, as in the other param families. The read
    # is the same either way -- an absent name and a stored NULL both give
    # NULL -- so `maximum_distance = NULL` ("no cutoff") round-trips.
    p <- kNNNetworkParam(k = 30)
    expect_null(p$maximum_distance)

    p$maximum_distance <- 20
    expect_identical(p$maximum_distance, 20)
    p$maximum_distance <- NULL
    expect_null(p$maximum_distance)
})

test_that("constructors validate what the slot types used to catch", {
    expect_error(kNNNetworkParam(k = "banana"), "count")
    expect_error(kNNNetworkParam(k = 30, filter = "yes"), "flag")
    expect_error(sNNNetworkParam(top_shared = -1), ">= 0")
    expect_error(delaunayNetworkParam(maximum_distance = "nonsense"),
                 "maximum_distance")
    # "auto" and NULL remain valid for delaunay
    expect_s4_class(delaunayNetworkParam(maximum_distance = "auto"),
                    "delaunayNetworkParam")
    expect_s4_class(delaunayNetworkParam(maximum_distance = NULL),
                    "delaunayNetworkParam")
})

test_that("networks build identically through the list-backed params", {
    set.seed(1)
    m <- cbind(runif(200, 0, 100), runif(200, 0, 100))
    rownames(m) <- sprintf("c%03d", seq_len(200))

    knn <- createNetwork(m, kNNNetworkParam(k = 6))
    expect_true(all(c("from", "to", "weight", "distance") %in% names(knn)))
    expect_equal(nrow(knn), 1200L)

    snn <- createNetwork(m, sNNNetworkParam(k = 6))
    expect_true("shared" %in% names(snn))

    del <- createNetwork(m, delaunayNetworkParam())
    expect_gt(nrow(del), 0L)
})
