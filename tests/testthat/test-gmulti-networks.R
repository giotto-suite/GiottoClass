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

    # `space` is a coordinate frame now, so neither a sample name nor a
    # group name is one. Rejecting both is the contract -- an artifact
    # generator takes no sample selection, whatever it is spelled.
    expect_error(createSpatialNetwork(mg, space = "a"),
        "not a registered space")
    expect_error(createSpatialNetwork(mg, space = "pair"),
        "not a registered space")
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


# fused cross-sample locations ####

# .gm_fused_spatlocs() is what a combined-frame network will be built on:
# one coordinate table spanning samples, IDs already in the joint
# `sample::id` vocabulary. The crop carrier is the other consumer, and it
# converts the same object to points.
test_that("fused spatlocs span samples in one object with global IDs", {
    mg <- .netfix_multi()
    sl <- .gm_fused_spatlocs(mg, space = NULL, coordinator = NULL)

    expect_s4_class(sl, "spatLocsObj")
    ids <- sl[]$cell_ID
    expect_true(all(grepl("^(a|b)::", ids)))
    # both samples present, and nothing lost in the fold
    expect_setequal(unique(sub("::.*$", "", ids)), c("a", "b"))
    expect_identical(
        length(ids),
        nrow(mg@objects$a@spatial_locs$cell$raw[]) +
            nrow(mg@objects$b@spatial_locs$cell$raw[])
    )
    expect_false(anyDuplicated(ids) > 0L)
})

test_that("fused spatlocs narrow to named samples, and say so when they cannot", {
    mg <- .netfix_multi()
    one <- .gm_fused_spatlocs(mg, space = NULL, coordinator = NULL,
        samples = "a")
    expect_true(all(grepl("^a::", one[]$cell_ID)))

    # A group is a sample name here -- this is a reader, so adr/0006 permits
    # the selector.
    gmultiGroup(mg, "pair") <- c("a", "b")
    both <- .gm_fused_spatlocs(mg, space = NULL, coordinator = NULL,
        samples = "pair")
    expect_identical(
        sort(both[]$cell_ID),
        sort(.gm_fused_spatlocs(mg, NULL, NULL)[]$cell_ID)
    )

    # Silently folding fewer children than asked for is the failure mode
    # this cannot afford.
    expect_error(
        .gm_fused_spatlocs(mg, NULL, NULL, samples = "nope"),
        "unknown sample"
    )
})


# a frame names, and changes, the artifact ####

# `space =` is a sanctioned exception to "do not build artifacts from
# temporary settings": a frame genuinely changes the network, so the
# artifact has to say which frame it came from. It does so twice -- by
# taking the frame's name, and by recording it in @parameters.
test_that("a frame prefixes the default name; native naming is unchanged", {
    g <- .netfix_giotto()
    g <- rescale(g, fx = 2, fy = 2, space = "scaled2x")
    nms <- function(x) list_spatial_networks_names(x, spat_unit = "cell")

    # The method stays readable off either name; only the prefix differs.
    for (m in c("kNN", "Delaunay")) {
        expect_true(paste0(m, "_network") %in%
            nms(createSpatialNetwork(g, method = m)),
            info = m)
        expect_true(paste0("scaled2x_", m, "_network") %in%
            nms(createSpatialNetwork(g, method = m, space = "scaled2x")),
            info = m)
    }
    expect_true("scaled2x_radius_network" %in%
        nms(createSpatialNetwork(g, method = "radius", radius = 300,
            space = "scaled2x")))

    # an explicit name is taken exactly as given, frame or no frame
    expect_true("mine" %in% nms(createSpatialNetwork(g, method = "kNN",
        k = 5, space = "scaled2x", name = "mine")))
})

test_that("the native frame has no name, so omitting it is the only spelling", {
    # There used to be a `":default:"` sentinel that had to produce the same
    # artifact as omitting `space`, or the two spellings of one request
    # would write to different names. The sentinel is gone: `space = NULL`
    # IS the native frame and there is no second way to say it.
    g <- .netfix_giotto()
    nms <- function(x) list_spatial_networks_names(x, spat_unit = "cell")

    bare <- createSpatialNetwork(g, method = "kNN", k = 5)
    expect_true("kNN_network" %in% nms(bare))
    # no frame in the name, and none recorded
    sn <- getSpatialNetwork(bare, name = "kNN_network",
        output = "spatialNetworkObj")
    expect_true(is.na(sn@parameters$space))

    # and the old sentinel is now just an unregistered name like any other
    expect_error(createSpatialNetwork(g, method = "kNN", k = 5,
        space = ":default:"), "not a registered space")
})

test_that("the frame is recorded in @parameters, not @provenance", {
    g <- .netfix_giotto()
    g <- rescale(g, fx = 2, fy = 2, space = "scaled2x")
    sn <- getSpatialNetwork(
        createSpatialNetwork(g, method = "kNN", k = 5, space = "scaled2x"),
        name = "scaled2x_kNN_network", output = "spatialNetworkObj")

    expect_identical(sn@parameters$space, "scaled2x")
    # @provenance answers "which spat_units were aggregated to make this",
    # a different question -- and two of its consumers assume an atomic
    # value, so a list there would break show() and garble the manifest.
    expect_false(is.list(sn@provenance))

    native <- getSpatialNetwork(
        createSpatialNetwork(g, method = "kNN", k = 5),
        name = "kNN_network", output = "spatialNetworkObj")
    expect_true(is.na(native@parameters$space))
})

test_that("a non-isometric frame actually changes the network", {
    # The whole reason the frame has to be recorded. Under a 2x rescale
    # distances double, so a fixed cutoff keeps fewer edges. `spin` /
    # `flip` / `spatShift` are isometries and would leave this identical --
    # which is why the name does not try to detect rigidity.
    g <- .netfix_giotto()
    g <- rescale(g, fx = 2, fy = 2, space = "scaled2x")
    ecount <- function(x, nm) {
        igraph::ecount(getSpatialNetwork(x, name = nm,
            output = "spatialNetworkObj")@network)
    }
    native <- createSpatialNetwork(g, method = "kNN", k = 8,
        maximum_distance_knn = 300)
    framed <- createSpatialNetwork(g, method = "kNN", k = 8,
        maximum_distance_knn = 300, space = "scaled2x")
    expect_gt(ecount(native, "kNN_network"),
        ecount(framed, "scaled2x_kNN_network"))
})


# the joint @spatial_network slot ####

# Build a cross-sample network the way step 9's writer will, and place it in
# the joint slot by hand. Exercising the maintenance before the writer exists
# is the point: these helpers are wired into `[`, `names<-` and the
# invalidation paths, and a slot that nothing writes to yet is exactly the
# kind that gets missed at one of them.
# A cross-sample network, named for the frame it was built in and written
# to the multi's own slot -- `setSpatialNetwork()` with no `object =` means
# "this is a multi-level artifact", which is where it has to go: no child's
# slot can hold an edge whose endpoints are in different samples.
.netfix_joint <- function(mg, frame = "atlas") {
    sl <- .gm_fused_spatlocs(mg, space = NULL, coordinator = NULL)
    sn <- .spatial_network_from_locs(sl,
        param = kNNNetworkParam(k = 5, filter = TRUE, output = "igraph"),
        method = "kNN", parameters = list(k = 5),
        name = paste0(frame, "_kNN_network"), spat_unit = "cell")
    setSpatialNetwork(mg, sn, spat_unit = "cell",
        name = paste0(frame, "_kNN_network"))
}

# Both hops go through public surface: the getter finds the multi-level
# network by name, and spatIDs() forwards to whatever carries the graph
# (PR #400), so this reads the same on a backed network whose `@network` is
# a store. Nothing here knows the slot's nesting -- if it did, re-keying the
# slot would break every test instead of one accessor.
.joint_ids <- function(mg, frame = "atlas") {
    spatIDs(getSpatialNetwork(mg, name = paste0(frame, "_kNN_network")))
}

test_that("giottoMulti declares @spatial_network, empty by default", {
    expect_true("spatial_network" %in% slotNames("giottoMulti"))
    expect_null(new("giottoMulti")@spatial_network)
})

test_that("a joint network spans samples with global IDs", {
    mg <- .netfix_joint(.netfix_multi())
    ids <- .joint_ids(mg)
    expect_true(all(grepl("^(a|b)::", ids)))
    expect_setequal(unique(sub("::.*$", "", ids)), c("a", "b"))
})

test_that("`[` prunes the joint network to surviving samples", {
    mg <- .netfix_joint(.netfix_multi())
    sub <- mg["a"]
    ids <- .joint_ids(sub)
    expect_true(all(grepl("^a::", ids)))
    expect_false(any(grepl("^b::", ids)))
})

test_that("`names<-` rewrites joint network vertex prefixes", {
    mg <- .netfix_joint(.netfix_multi())
    before <- .joint_ids(mg)
    names(mg) <- c("x", "y")
    after <- .joint_ids(mg)
    expect_setequal(unique(sub("::.*$", "", after)), c("x", "y"))
    # a rename moves every vertex, it does not drop any
    expect_identical(length(after), length(before))
})

test_that("adding a sample warns that the joint network no longer covers it", {
    mg <- .netfix_joint(.netfix_multi())
    expect_warning(mg[["c"]] <- .netfix_giotto(), "spatial_network")
})

test_that("list_spatial_networks refuses a giottoMulti", {
    # Its walk assumes giotto's `spat_unit -> name`; on the multi's
    # `frame -> spat_unit -> name` it would report frame names in the
    # spat_unit column rather than failing.
    mg <- .netfix_joint(.netfix_multi())
    expect_error(list_spatial_networks(mg), "not yet supported")
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

# the combined build path ####
#
# A combinedSpace lays its samples in one coordinate system, so a job over
# it is ONE job. Before this, every frame was planned per-sample: a
# combinedSpace built N independent networks in a shared frame and missed
# exactly the cross-sample edges it exists for.

# Lay `b` beside `a` with an overlap, so cross-sample edges must exist.
# The combinedSpace is DECLARED, not recorded into existence: recording onto
# an unused name gives a perSampleSpace. Saying two samples share a
# coordinate system is the claim this fixture makes, and it is the claim
# that turns N per-child jobs into one job at the parent.
.netfix_atlas <- function(mg) {
    e <- ext(mg@objects$a@spatial_locs$cell$raw)
    giottoSpace(mg, "atlas") <- combinedSpace(name = "atlas")
    mg <- spatShift(mg, dx = (e[2] - e[1]) * 0.9, space = "atlas",
        samples = "b")
    # `a` is in the layout by not moving — a member step says so
    spatShift(mg, dx = 0, space = "atlas", samples = "a")
}

test_that("a combinedSpace builds ONE network spanning its members", {
    mg <- .netfix_atlas(.netfix_multi())
    expect_s4_class(giottoSpace(mg, "atlas"), "combinedSpace")

    out <- createSpatialNetwork(mg, method = "kNN", k = 8, space = "atlas")

    # one artifact at the multi level, children untouched
    expect_identical(out@objects, mg@objects)
    expect_identical(names(out@spatial_network$cell), "atlas_kNN_network")

    sn <- getSpatialNetwork(out, name = "atlas_kNN_network")
    v <- names(igraph::V(sn@network))
    expect_true(all(grepl("::", v)))
    expect_identical(length(v), length(spatIDs(mg)))
})

test_that("the combined network actually has cross-sample edges", {
    # Without this the feature can pass while doing nothing: N per-sample
    # networks stacked into one object would satisfy every check above.
    mg <- .netfix_atlas(.netfix_multi())
    out <- createSpatialNetwork(mg, method = "kNN", k = 8, space = "atlas")
    el <- igraph::as_edgelist(
        getSpatialNetwork(out, name = "atlas_kNN_network")@network)
    cross <- sum(sub("::.*", "", el[, 1]) != sub("::.*", "", el[, 2]))
    expect_gt(cross, 0L)
})

test_that("a combinedSpace can return the network; per-sample cannot", {
    mg <- .netfix_atlas(.netfix_multi())
    # one object, so the answer is unambiguous
    expect_s4_class(
        createSpatialNetwork(mg, method = "kNN", k = 8, space = "atlas",
            return_gobject = FALSE),
        "spatialNetworkObj")
    # N objects, so there is no single one to hand back
    expect_error(
        createSpatialNetwork(mg, method = "kNN", k = 8,
            return_gobject = FALSE),
        "return_gobject = TRUE")
})

test_that("a combinedSpace naming an absent sample is refused", {
    mg <- .netfix_atlas(.netfix_multi())
    expect_error(
        createSpatialNetwork(mg["a"], method = "kNN", k = 8, space = "atlas"),
        "absent from the object")
})

test_that("the wrappers build on a giottoMulti instead of dying", {
    # both died with `incorrect number of dimensions`: they went straight to
    # the shared builder, where getSpatialLocations(mg) handed back a list
    mg <- .netfix_multi()
    expect_s4_class(createSpatialKNNnetwork(mg, k = 5), "giottoMulti")
    expect_s4_class(
        createSpatialDelaunayNetwork(mg, verbose = FALSE), "giottoMulti")
})

test_that("every door records the frame in the default name", {
    # the wrappers applied a frame and did not name for it, so a framed
    # build silently overwrote the native one under the same key
    g <- .netfix_giotto()
    g <- rescale(g, fx = 2, fy = 2, space = "scaled2x")

    knn <- createSpatialKNNnetwork(g, k = 5, space = "scaled2x")
    expect_true("scaled2x_knn_network" %in% names(knn@spatial_network$cell))
    expect_false("knn_network" %in% names(knn@spatial_network$cell))

    del <- createSpatialDelaunayNetwork(g, space = "scaled2x", verbose = FALSE)
    expect_true(
        "scaled2x_Delaunay_network" %in% names(del@spatial_network$cell))

    # and each door keeps its own default spelling when unframed
    expect_true("knn_network" %in%
        names(createSpatialKNNnetwork(g, k = 5)@spatial_network$cell))
    expect_true("kNN_network" %in% names(
        createSpatialNetwork(g, method = "kNN", k = 5)@spatial_network$cell))
})

test_that("a perSampleSpace builds one network per child, in the frame", {
    mg <- .netfix_multi()
    # declaration-only: recording onto an unused name would declare a
    # perSampleSpace anyway, so declare the intent (adr/0006)
    giottoSpace(mg, "upright") <- perSampleSpace("upright")
    mg <- spatShift(mg, dx = 5000, space = "upright", samples = "b")
    expect_s4_class(giottoSpace(mg, "upright"), "perSampleSpace")

    out <- createSpatialNetwork(mg, method = "kNN", k = 5, space = "upright")

    # N artifacts on the children, nothing at the multi level
    expect_length(out@spatial_network, 0L)
    for (nm in c("a", "b")) {
        expect_true("upright_kNN_network" %in%
            names(out@objects[[nm]]@spatial_network$cell))
    }

    # `.csn_forward()` used to strip `space` on the way to each child, so a
    # framed per-sample build silently ran in the native frame. The child
    # cannot resolve the parent's frame by name, so it is handed `space[nm]`.
    p <- getSpatialNetwork(out@objects$b,
        name = "upright_kNN_network")@parameters
    expect_identical(p$space, "upright")
    # recorded once: the per-sample path re-enters the builder per child, so
    # an appended record would land twice
    expect_identical(sum(names(p) == "space"), 1L)
})
