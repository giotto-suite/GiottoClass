# Spatial network construction on a giottoMulti.
#
# Scope: the per-child dispatch in createSpatialNetwork(), and adr/0006 --
# artifact generators take no view and no sample selector, and read their
# job size from the space.

rlang::local_options(lifecycle_verbosity = "quiet")
options("giotto.use_conda" = FALSE)

# Spatial locations, polygons and expression only. The mini's dim reductions
# and NN networks are keyed by the original cell IDs and this fixture renames
# them, which would make every construction warn about coord names not found
# in gobject IDs -- noise that would mask a real validator complaint.
.netfix_giotto <- function() {
    g <- updateGiottoObject(
        GiottoData::loadGiottoMini("visium", verbose = FALSE))
    g@dimension_reduction <- NULL
    g@nn_network <- NULL
    g@spatial_network <- NULL
    g@spatial_enrichment <- NULL
    g
}

# Two children whose cells carry distinct local IDs, so a cross-sample edge
# is distinguishable from a within-sample one by prefix alone.
.netfix_multi <- function() {
    g1 <- .netfix_giotto()
    g2 <- .netfix_giotto()
    new_ids <- paste0("s2_", pDataDT(g2)$cell_ID)
    g2@cell_metadata$cell$rna@metaDT$cell_ID <- new_ids
    sl <- g2@spatial_locs$cell$raw
    sl@coordinates$cell_ID <- new_ids
    g2@spatial_locs$cell$raw <- sl
    sv <- g2@spatial_info$cell@spatVector
    sv$poly_ID <- new_ids
    g2@spatial_info$cell@spatVector <- sv
    g2@spatial_info$cell@unique_ID_cache <- new_ids
    e <- g2@expression$cell$rna$raw
    colnames(e@exprMat) <- new_ids
    g2@expression$cell$rna$raw <- e
    g2@cell_ID$cell <- new_ids
    createGiottoMulti(list(a = g1, b = g2))
}

# Edge frame of a child's network, as a comparable data.frame.
.net_edges <- function(g, name) {
    sn <- getSpatialNetwork(g, name = name, output = "spatialNetworkObj")
    e <- igraph::as_data_frame(sn@network, what = "edges")
    e <- e[order(e$from, e$to), c("from", "to"), drop = FALSE]
    rownames(e) <- NULL
    e
}


# every formal reaches the child ####

# This is the test the hand-listed per-child forward could not survive. It
# forwarded 19 formals by name; upstream added `radius` to the signature and
# the list was not updated, and because the merge was textually clean nothing
# flagged it. `radius` bound at the container and was dropped on the way down,
# so the child raised "needs a `radius`" naming an argument the user supplied.
#
# Driving it off the FORMALS rather than off a fixed list is the point: adding
# a formal to createSpatialNetwork() must not require editing a forward list
# anywhere, and the only maintenance here is adding a case when a new formal
# needs one.
test_that("every createSpatialNetwork() formal reaches each child", {
    mg <- .netfix_multi()
    cases <- list(
        radius = list(method = "radius", radius = 400),
        k = list(method = "kNN", k = 7),
        maximum_distance_knn = list(
            method = "kNN", k = 20, maximum_distance_knn = 300),
        minimum_k = list(method = "kNN", k = 4, minimum_k = 2),
        delaunay_method = list(
            method = "Delaunay", delaunay_method = "delaunayn_geometry"),
        maximum_distance_delaunay = list(
            method = "Delaunay", maximum_distance_delaunay = 400),
        name = list(method = "kNN", k = 5, name = "custom_net")
    )

    for (case in names(cases)) {
        args <- cases[[case]]
        nm <- args$name %null% paste0(
            switch(args$method, kNN = "kNN", Delaunay = "Delaunay",
                radius = "radius"), "_network")

        got <- do.call(createSpatialNetwork, c(list(gobject = mg), args))
        for (s in names(mg@objects)) {
            want <- do.call(createSpatialNetwork,
                c(list(gobject = mg@objects[[s]]), args))
            expect_identical(
                .net_edges(got@objects[[s]], nm), .net_edges(want, nm),
                info = sprintf("formal `%s`, sample `%s`", case, s)
            )
        }
    }
})


# adr/0006 — no sample selector on an artifact generator ####

test_that("createSpatialNetwork takes no sample selector", {
    mg <- .netfix_multi()
    gmultiGroup(mg, "pair") <- c("a", "b")

    # `space` is a coordinate frame now. A sample name, or a group name, is
    # not one -- and the error has to say which mistake was made, or the
    # next reader reinstates the selector.
    expect_error(createSpatialNetwork(mg, space = "a"), "not a coordinate frame")
    expect_error(createSpatialNetwork(mg, space = "pair"),
        "not a coordinate frame")

    # ":all:" meant "all children" here and "all keys of a nesting axis"
    # everywhere else in the package. Retired outright; `space = NULL` is
    # the spelling for all samples.
    expect_error(createSpatialNetwork(mg, space = ":all:"))
})

test_that("readers keep the sample selector the writers lose", {
    # The asymmetry adr/0006 turns on: narrowing a returned value is
    # reversible, narrowing a slot is not.
    mg <- .netfix_multi()
    gmultiGroup(mg, "pair") <- c("a", "b")
    expect_identical(
        getCellMetadata(mg, samples = "pair", output = "data.table"),
        getCellMetadata(mg, samples = c("a", "b"), output = "data.table")
    )
})

test_that("artifact generators carry neither a view nor a sample selector", {
    # `object =` is deliberately NOT in the forbidden set: on the setters it
    # is a single-valued write target resolved by .gm_set_target(), not a
    # selector. See adr/0006 Consequences.
    gens <- c(
        "createSpatialNetwork", "createSpatialDelaunayNetwork",
        "createSpatialKNNnetwork", "createSpatialFeaturesKNNnetwork",
        "createSpatialWeightMatrix", "createSpatialGrid",
        "createSpatialDefaultGrid", "createNearestNetwork",
        "addNetworkLayout", "createMetafeats", "addCellMetadata",
        "addFeatMetadata", "addSpatialCentroidLocations",
        "calculateOverlapRaster", "calculateOverlapSerial",
        "calculateOverlapParallel", "overlapToMatrixMultiPoly"
    )
    for (f in gens) {
        fm <- names(formals(get(f, envir = asNamespace("GiottoClass"))))
        expect_false("view" %in% fm, info = paste(f, "- see adr/0006"))
        expect_false(any(c("samples", "sample") %in% fm),
            info = paste(f, "- see adr/0006"))
    }
})


# the per-sample path is unchanged ####

test_that("createSpatialNetwork(mg) builds in every child", {
    mg <- .netfix_multi()
    out <- createSpatialNetwork(mg, method = "kNN", k = 5)
    for (s in names(mg@objects)) {
        want <- createSpatialNetwork(mg@objects[[s]], method = "kNN", k = 5)
        expect_identical(
            .net_edges(out@objects[[s]], "kNN_network"),
            .net_edges(want, "kNN_network")
        )
    }
})
