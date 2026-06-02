# Tests for the analyzeData dispatch on labelProportionsParam.
#
# Canonical shape: param dispatches on the data class.
#   analyzeData(giotto, labelProportionsParam)   # sugar router
#   analyzeData(igraph, labelProportionsParam)   # in-mem network primary
#
# calculateLabelProportions() now wraps the giotto dispatch path.

options("giotto.use_conda" = FALSE)
options("lifecycle_verbosity" = "quiet")

g <- GiottoData::loadGiottoMini("viz")
activeSpatUnit(g) <- "aggregate"


test_that("labelProportionsParam factory captures all 3 group_methods", {
    p1 <- labelProportionsParam(labels = "leiden_clus",
        group_method = "table",
        groups = data.frame(grp = "all", cell_ID = head(spatIDs(g))),
        column_cell_id = "cell_ID")
    expect_s4_class(p1, "labelProportionsParam")
    expect_identical(p1$group_method, "table")

    p2 <- labelProportionsParam(labels = "leiden_clus",
        group_method = "spatialnetwork",
        spatial_network_name = "Delaunay_network",
        weights = TRUE, alpha = 0.5)
    expect_identical(p2$group_method, "spatialnetwork")
    expect_identical(p2$alpha, 0.5)

    p3 <- labelProportionsParam(labels = "leiden_clus",
        group_method = "polygon",
        spat_info = "aggregate")
    expect_identical(p3$group_method, "polygon")
    expect_identical(p3$spat_info, "aggregate")
})


test_that("analyzeData(giotto, ..) routes spatialnetwork to igraph primary method", {
    p <- labelProportionsParam(labels = "leiden_clus",
        group_method = "spatialnetwork",
        spatial_network_name = "Delaunay_network")
    enr <- analyzeData(g, p,
        spat_unit = "aggregate", output = "spatEnrObj")
    expect_s4_class(enr, "spatEnrObj")
})


test_that("analyzeData(igraph, labelProportionsParam) consumes labels DT and returns wide DT", {
    p <- labelProportionsParam(labels = "leiden_clus",
        group_method = "spatialnetwork",
        spatial_network_name = "Delaunay_network")
    sn <- getSpatialNetwork(g, spat_unit = "aggregate",
        name = "Delaunay_network", output = "spatialNetworkObj")
    labs <- spatValues(g, feats = "leiden_clus",
        spat_unit = "aggregate", verbose = FALSE)
    res <- analyzeData(sn[], p, labels = labs)
    expect_s3_class(res, "data.table")
    expect_true("group" %in% colnames(res))
    # rows are groups, additional cols are label categories
    expect_gt(ncol(res), 1L)
})


test_that("analyzeData(igraph, ..) errors without labels", {
    p <- labelProportionsParam(labels = "leiden_clus",
        group_method = "spatialnetwork",
        spatial_network_name = "Delaunay_network")
    sn <- getSpatialNetwork(g, spat_unit = "aggregate",
        name = "Delaunay_network", output = "spatialNetworkObj")
    expect_error(analyzeData(sn[], p), "labels.*required")
})


test_that("calculateLabelProportions wrapper still produces equivalent output", {
    p <- labelProportionsParam(labels = "leiden_clus",
        group_method = "spatialnetwork",
        spatial_network_name = "Delaunay_network")
    direct <- analyzeData(g, p, spat_unit = "aggregate", output = "data.table")
    wrapped <- calculateLabelProportions(g,
        labels = "leiden_clus",
        group_method = "spatialnetwork",
        spatial_network_name = "Delaunay_network",
        spat_unit = "aggregate", output = "data.table")
    expect_equal(direct, wrapped)
})


test_that("table group_method stays inline and returns data.table", {
    rels <- data.frame(
        grp = rep(LETTERS[1:5], length.out = ncol(g)),
        cell_ID = colnames(g)
    )
    res <- calculateLabelProportions(g,
        labels = "leiden_clus",
        group_method = "table",
        groups = rels, column_cell_id = "cell_ID",
        spat_unit = "aggregate", output = "data.table")
    expect_s3_class(res, "data.table")
})
