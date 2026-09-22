# A spatial network stores edges between named cells and nothing else, so
# everything about the cells an edge runs between is attached rather than read
# back. Both annotations are the same operation on different sources, and both
# are optional -- these pin down that they stay independent.

skip_if_no_mini <- function() skip_if_not_installed("GiottoData")

.ann_mini <- function() {
    suppressMessages(GiottoData::loadGiottoMini("vizgen", verbose = FALSE))
}

.ann <- function(g, ...) {
    suppressMessages(annotateSpatialNetwork(g, spat_unit = "aggregate",
        spatial_network_name = "Delaunay_network", ...))
}


test_that("the two annotations are independent", {
    skip_if_no_mini()
    g <- .ann_mini()
    coords <- c("sdimx_begin", "sdimy_begin", "sdimx_end", "sdimy_end")
    labels <- c("from_cell_type", "to_cell_type", "type_int", "from_to",
        "unified_int")

    geom <- .ann(g)
    lab <- .ann(g, cluster_column = "leiden_clus", coordinates = FALSE)
    both <- .ann(g, cluster_column = "leiden_clus")

    expect_true(all(coords %in% names(geom)))
    expect_false(any(labels %in% names(geom)))

    expect_true(all(labels %in% names(lab)))
    expect_false(any(coords %in% names(lab)))

    expect_true(all(c(coords, labels) %in% names(both)))
    # an annotation widens, it never lengthens
    expect_identical(nrow(geom), nrow(lab))
    expect_identical(nrow(lab), nrow(both))
})

test_that("a label may come from any slot spatValues searches", {
    skip_if_no_mini()
    # The point of resolving through spatValues rather than reading cell
    # metadata: an expression feature annotates an edge as well as a cluster.
    g <- .ann_mini()
    feat <- rownames(getExpression(g, spat_unit = "aggregate",
        output = "matrix"))[[1L]]
    out <- .ann(g, cluster_column = feat, coordinates = FALSE)
    expect_true(all(c("from_cell_type", "to_cell_type") %in% names(out)))
    expect_false(anyNA(out$from_cell_type))
})

test_that("coordinates are read live, so a transform is carried along", {
    skip_if_no_mini()
    g <- .ann_mini()
    a <- .ann(g)
    b <- .ann(spatShift(g, dx = 1000))
    expect_identical(nrow(a), nrow(b))
    expect_equal(b$sdimx_begin - a$sdimx_begin, rep(1000, nrow(a)))
    expect_equal(a$sdimy_begin, b$sdimy_begin)
})

test_that("an edge with an endpoint absent from the locations is dropped", {
    edges <- data.table::data.table(
        from = c("a", "b", "c"), to = c("b", "c", "a"),
        distance = 1, weight = 1)
    locs <- data.table::data.table(
        cell_ID = c("a", "b"), sdimx = c(0, 1), sdimy = c(0, 1))

    out <- GiottoClass:::.attach_edge_coords(edges, locs)
    expect_identical(nrow(out), 1L)          # only a--b has both ends
    expect_setequal(c(out$from, out$to), c("a", "b"))
})

test_that("a cell that keeps no edge simply has no row", {
    # Not a defect to correct: an edge table has no way to carry an isolated
    # node, which is why a consumer takes its node set from the locations.
    edges <- data.table::data.table(from = "a", to = "b", distance = 1,
        weight = 1)
    locs <- data.table::data.table(cell_ID = c("a", "b", "lonely"),
        sdimx = c(0, 1, 2), sdimy = c(0, 1, 2))

    out <- GiottoClass:::.attach_edge_coords(edges, locs)
    expect_identical(nrow(out), 1L)
    expect_false("lonely" %in% c(out$from, out$to))
})

test_that("a third dimension is carried through when present", {
    edges <- data.table::data.table(from = "a", to = "b", distance = 1,
        weight = 1)
    locs <- data.table::data.table(cell_ID = c("a", "b"), sdimx = c(0, 1),
        sdimy = c(0, 1), sdimz = c(0, 2))

    out <- GiottoClass:::.attach_edge_coords(edges, locs)
    expect_true(all(c("sdimz_begin", "sdimz_end") %in% names(out)))
    expect_equal(out$sdimz_end - out$sdimz_begin, 2)
})

test_that("a network refuses to plot itself but plots against locations", {
    skip_if_no_mini()
    g <- .ann_mini()
    sn <- getSpatialNetwork(g, spat_unit = "aggregate",
        name = "Delaunay_network", output = "spatialNetworkObj")
    sl <- getSpatialLocations(g, spat_unit = "aggregate",
        output = "spatLocsObj")

    expect_error(plot(sn), "cannot be drawn on its own")

    pdf(NULL)
    on.exit(dev.off(), add = TRUE)
    expect_no_error(plot(sn, sl))
})
