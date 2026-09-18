# Tests for giottoView + giottoSpace classes, the resolver engine, and
# the JIT view/space integration in getters.
#
# silence deprecated internal functions
rlang::local_options(lifecycle_verbosity = "quiet")
options("giotto.use_conda" = FALSE)

# There is no constructor: a view is created by recording a step onto a
# name. An EMPTY recipe therefore has no call that produces it, and is only
# interesting to tests asserting that an empty recipe resolves to identity.
# Build the plain `as.list()` form, which also exercises the setter's
# coercion and validator.
.empty_view <- function() {
    list(steps = list())
}

# fixture — visium mini with leiden clusters in metadata
.fixture_giotto <- function() {
    g <- GiottoData::loadGiottoMini("visium", verbose = FALSE)
    updateGiottoObject(g)
}

# fixture — two-sample giottoMulti with each child's cells under distinct
# local IDs (s2_* in the second child) so global IDs are unambiguous.
.fixture_gmulti <- function() {
    g1 <- .fixture_giotto()
    g2 <- .fixture_giotto()
    cm2 <- pDataDT(g2)
    new_ids <- paste0("s2_", cm2$cell_ID)
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


# --- giottoView class ------------------------------------------------------

test_that("recording onto an unused name creates the view", {
    g <- giotto()
    expect_length(giottoViews(g), 0L)
    g <- subset(g, cluster == "A", view = "v")
    expect_identical(giottoViews(g), "v")
    v <- giottoView(g, "v")
    expect_s4_class(v, "giottoView")
    expect_length(v, 1L)
    expect_identical(names(v), "filter")
})

test_that("the containers are classes; the steps are plain lists", {
    # Q7 put the serialization guarantees on the STEPS, which is why the
    # container can be a class without giving them up.
    expect_s4_class(giottoView(
        subset(giotto(), x > 0, view = "v"), "v"), "giottoView")
    expect_s4_class(giottoSpace(
        spatShift(giotto(), dx = 1, space = "s"), "s"), "giottoSpace")
})

test_that("subset() records a filter step", {
    g <- subset(giotto(), cluster == "A", view = "v")
    steps <- giottoView(g, "v")@steps
    expect_length(steps, 1L)
    expect_identical(steps[[1L]]$type, "filter")
    expect_identical(steps[[1L]]$predicate, 'cluster == "A"')
})

test_that("subset() records the scope args spatValues will receive", {
    g <- subset(giotto(), x > 0, spat_unit = "cell", feat_type = "rna",
        view = "v")
    sa <- giottoView(g, "v")@steps[[1L]]$scope_args
    expect_identical(sa$spat_unit, "cell")
    expect_identical(sa$feat_type, "rna")
})

test_that("scope args are limited to what spatValues accepts", {
    # `spatValues()` pulls named values out of slots; it has no notion of
    # negation or of an id vector, so recording one would only fail later,
    # inside spatValues, far from this call.
    expect_error(subset(giotto(), x > 0, view = "v", nonsense = 1),
        "cannot record")
})

test_that("negate folds into the predicate rather than becoming a field", {
    # matches the eager path, which does `sub_s <- call(\"!\", sub_s)`
    g <- subset(giotto(), cluster == "A", negate = TRUE, view = "v")
    step <- giottoView(g, "v")@steps[[1L]]
    expect_identical(step$predicate, '!cluster == "A"')
    expect_null(step$negate)
    # `!` binds looser than `==` in R, so the deparsed form re-parses with
    # the negation still outside the comparison
    expect_identical(str2lang(step$predicate),
        quote(!cluster == "A"))
})

test_that("crop() records a crop step", {
    g <- crop(giotto(), c(0, 100, 0, 100), view = "v")
    steps <- giottoView(g, "v")@steps
    expect_length(steps, 1L)
    expect_identical(steps[[1L]]$type, "crop")
    # Q7: numeric extents are normalized to WKT at record time, so the
    # terra (xmin, xmax, ymin, ymax) convention is applied exactly once
    expect_type(steps[[1L]]$region, "character")
    expect_equal(terra::ext(terra::vect(steps[[1L]]$region))[],
        terra::ext(c(0, 100, 0, 100))[])
    expect_identical(steps[[1L]]$relation, "intersects")
})

test_that("crop() with custom relation records it", {
    g <- crop(giotto(), c(0, 100, 0, 100), relation = "within", view = "v")
    expect_identical(giottoView(g, "v")@steps[[1L]]$relation, "within")
})

test_that("crop() with polygon region works", {
    poly <- terra::vect(rbind(
        c(4000, -5000), c(5500, -5000),
        c(5500, -3500), c(4000, -3500),
        c(4000, -5000)
    ), type = "polygons")
    g <- crop(giotto(), poly, view = "v")
    step <- giottoView(g, "v")@steps[[1L]]
    expect_identical(step$type, "crop")
    # SpatVector polygon regions are normalized to WKT at ingest so the
    # recipe is serializable (no live C++ pointer). See methods-view.R
    # .normalize_crop_region.
    expect_type(step$region, "character")
})

test_that("materialize with polygon crop narrows by region", {
    g <- .fixture_giotto()
    # build a polygon equivalent to a known extent
    poly <- terra::vect(rbind(
        c(4000, -5000), c(5500, -5000),
        c(5500, -3500), c(4000, -3500),
        c(4000, -5000)
    ), type = "polygons")
    # expected: cells whose centroid is within the polygon's AABB
    sl <- getSpatialLocations(g, output = "data.table")
    expected <- sum(sl$sdimx >= 4000 & sl$sdimx <= 5500 &
                    sl$sdimy >= -5000 & sl$sdimy <= -3500)

    g <- crop(g, poly, view = "poly_crop")
    g2 <- materialize(g, "poly_crop")
    expect_equal(nrow(pDataDT(g2)), expected)
})

test_that("selectSamples() records a samples step", {
    g <- selectSamples(giotto(), "a", "b", view = "v")
    steps <- giottoView(g, "v")@steps
    expect_length(steps, 1L)
    expect_identical(steps[[1L]]$type, "samples")
    expect_identical(steps[[1L]]$samples, c("a", "b"))
})

test_that("steps append in call order onto one name", {
    g <- giotto()
    g <- subset(g, x > 0, view = "v")
    g <- crop(g, c(0, 100, 0, 100), view = "v")
    g <- selectSamples(g, "a", view = "v")
    steps <- giottoView(g, "v")@steps
    expect_length(steps, 3L)
    expect_identical(vapply(steps, function(s) s$type, character(1L)),
        c("filter", "crop", "samples"))
})


# --- space recipes ---------------------------------------------------------

test_that("recording onto an unused name creates the space", {
    g <- spatShift(giotto(), dx = 10, space = "s")
    expect_identical(giottoSpaces(g), "s")
    s <- giottoSpace(g, "s")
    expect_s4_class(s, "giottoSpace")
    expect_identical(s@name, "s")
    # naming an unused space declares a PER-SAMPLE one. The kind decides
    # job size, and only the per-sample size round-trips: it writes one
    # artifact per child, the shape reading per child returns.
    expect_s4_class(s, "perSampleSpace")
    # a plain giotto has no child names, so the recipe mentions nobody and
    # the step is unscoped, which is how it reaches the one sample there is
    expect_identical(names(s), character())
    expect_length(s, 1L)
    expect_identical(s[[NA_character_]][[1L]]$op, "spatShift")
    expect_null(s@steps[[1L]]$samples)
})

test_that("transform generics record onto a named space", {
    M <- diag(c(1, 1, 1))
    g <- giotto()
    g <- spin(g, 30, space = "s")
    g <- affine(g, M, space = "s")
    g <- spatShift(g, dx = 10, space = "s")
    steps <- giottoSpace(g, "s")@steps
    expect_length(steps, 3L)
    expect_identical(vapply(steps, function(x) x$op, character(1L)),
        c("spin", "affine", "spatShift"))
})

test_that("spin/affine record (0,0) anchor by default", {
    g <- spin(giotto(), 45, space = "s")
    args <- giottoSpace(g, "s")[[1L]]$args
    expect_equal(args$x0, 0)
    expect_equal(args$y0, 0)
})

test_that("user-supplied anchor overrides default", {
    g <- spin(giotto(), 45, x0 = 100, y0 = 200, space = "s")
    args <- giottoSpace(g, "s")[[1L]]$args
    expect_equal(args$x0, 100)
    expect_equal(args$y0, 200)
})


# --- samples= is the gmulti-only replacement for `+` -----------------------

test_that("samples= keys transforms per child, replacing `+`", {
    mg <- .fixture_gmulti()
    mg <- spin(mg, 30, space = "atlas", samples = "a")
    mg <- spatShift(mg, dx = 8000, space = "atlas", samples = "b")
    s <- giottoSpace(mg, "atlas")
    expect_identical(names(s), c("a", "b"))
    expect_length(s[["a"]], 1L)
    expect_length(s[["b"]], 1L)
    expect_identical(s[["a"]][[1L]]$op, "spin")
    expect_identical(s[["b"]][[1L]]$op, "spatShift")
})

test_that("recording twice against one sample concatenates in order", {
    mg <- .fixture_gmulti()
    mg <- spin(mg, 30, space = "atlas", samples = "a")
    mg <- spatShift(mg, dx = 10, space = "atlas", samples = "a")
    steps <- giottoSpace(mg, "atlas")[["a"]]
    expect_length(steps, 2L)
    expect_identical(vapply(steps, function(x) x$op, character(1L)),
        c("spin", "spatShift"))
})

test_that("samples= accepts several children at once", {
    mg <- .fixture_gmulti()
    mg <- spin(mg, 30, space = "atlas", samples = c("a", "b"))
    s <- giottoSpace(mg, "atlas")
    # one step, scoped to both -- not one step per sample
    expect_length(s, 1L)
    expect_identical(s[[1L]]$samples, c("a", "b"))
    expect_length(s[["a"]], 1L)
    expect_length(s[["b"]], 1L)
})

test_that("a broadcast step reaches a sample named only afterwards", {
    # the reason scope is a property of the STEP rather than of a
    # per-sample list: appending to "every key recorded so far" can never
    # reach a key that appears later, so the same recipe would replay
    # differently depending on the order samples happened to be named
    mg <- .fixture_gmulti()
    mg <- spin(mg, 30, space = "atlas")
    mg <- spatShift(mg, dx = 8000, space = "atlas", samples = "b")
    mg <- spin(mg, 10, space = "atlas")
    s <- giottoSpace(mg, "atlas")
    expect_identical(vapply(s[["b"]], function(x) x$op, character(1L)),
        c("spin", "spatShift", "spin"))
    expect_identical(vapply(s[["a"]], function(x) x$op, character(1L)),
        c("spin", "spin"))
    # membership is a declaration, not an inference from who a broadcast
    # happened to touch: only "b" was ever named, so only "b" is declared
    expect_identical(names(s), "b")
})

test_that("scope is stated per call, not inherited from build order", {
    # This is the bug `+` had: `.space_record()` appended to every sample
    # keyed so far, so the meaning of a transform depended on how much of
    # the recipe had been merged before it. Recording `a` then `b` must
    # leave `a` with exactly its own step.
    mg <- .fixture_gmulti()
    mg <- spin(mg, 30, space = "atlas", samples = "a")
    mg <- spatShift(mg, dx = 10, space = "atlas", samples = "b")
    s <- giottoSpace(mg, "atlas")
    expect_length(s[["a"]], 1L)
    expect_identical(s[["a"]][[1L]]$op, "spin")
})

test_that("a gmulti transform requires a space, and samples= is gmulti-only", {
    mg <- .fixture_gmulti()
    expect_error(spatShift(mg, dx = 1), "requires `space =")
    # a plain giotto has no children to scope to
    expect_error(spatShift(giotto(), dx = 1, space = "s", samples = "a"),
        "unused argument")
})


# --- Accessors -------------------------------------------------------------

test_that("the setter copies a recipe between objects", {
    # The only remaining job of the setter now that recording creates
    # views: move one from object to object, or drop it.
    g1 <- subset(giotto(), cluster == "A", view = "tumor")
    g2 <- giotto()
    giottoView(g2, "tumor") <- giottoView(g1, "tumor")
    expect_identical(giottoViews(g2), "tumor")
    expect_identical(giottoView(g2, "tumor"), giottoView(g1, "tumor"))
})

test_that("giottoView(g, name) <- NULL removes", {
    g <- giotto()
    g <- subset(g, x > 0, view = "a")
    g <- subset(g, x > 0, view = "b")
    giottoView(g, "a") <- NULL
    expect_identical(giottoViews(g), "b")
})

test_that("the setter validates a hand-built recipe", {
    g <- giotto()
    expect_error({
        giottoView(g, "bad") <- list(steps = list(), typo = 1)
    }, "unknown field")
    expect_error({
        giottoView(g, "bad") <- list(steps = list(list(type = "nope")))
    }, "unknown type")
    expect_error({
        giottoSpace(g, "bad") <- list(s = list(steps = list()))
    }, "`kind` element")
    expect_error({
        giottoSpace(g, "bad") <- list(s = list(kind = "sideways"))
    }, "unknown kind")
})

test_that("giottoSpace accessor and lookup", {
    g <- spin(giotto(), 30, space = "atlas")
    expect_identical(giottoSpaces(g), "atlas")
    out <- giottoSpace(g, "atlas")
    expect_s4_class(out, "giottoSpace")
    expect_identical(out@name, "atlas")
})

test_that("missing slotted name errors clearly", {
    g <- giotto()
    expect_error(giottoView(g, "missing"), "no view named")
    expect_error(giottoSpace(g, "missing"), "not a registered space")
})

test_that("the getter owns the space-name check, so every caller gets it", {
    # folded into `giottoSpace()` because every caller that checks goes on
    # to fetch, so a consumer cannot do one without the other.
    mg <- .fixture_gmulti()
    mg <- spin(mg, 30, space = "atlas", samples = "a")

    expect_error(giottoSpace(mg, "nope"), "not a registered space")
    # the writer that used to carry its own copy of this check now
    # inherits it from the getter
    expect_error(createSpatialNetwork(mg, space = "nope"),
        "not a registered space")

    # the native frame has no name to resolve -- `space = NULL` is it
    expect_error(giottoSpace(mg, ":default:"), "not a registered space")
    expect_identical(giottoSpace(mg, "atlas")@name, "atlas")
})


# --- Migration on updateGiottoObject --------------------------------------

test_that("updateGiottoObject() adds @view and @spaces for pre-0.7.0", {
    g <- giotto()
    g@versions$gclass <- "0.6.0"
    g <- updateGiottoObject(g)
    expect_true(methods::.hasSlot(g, "view"))
    expect_true(methods::.hasSlot(g, "spaces"))
    expect_null(g@view)
    expect_null(g@spaces)
})

test_that("save/load round-trip preserves @view and @spaces", {
    g <- giotto()
    g <- subset(g, x > 0, view = "demo")
    g <- spin(g, 45, space = "tilted")

    td <- tempfile("gv-")
    on.exit(unlink(file.path(dirname(td), basename(td)), recursive = TRUE),
        add = TRUE)

    saveGiotto(g, foldername = basename(td), dir = dirname(td),
        verbose = FALSE, overwrite = TRUE)
    g2 <- loadGiotto(file.path(dirname(td), basename(td)), verbose = FALSE)

    expect_identical(giottoViews(g2), "demo")
    expect_identical(giottoSpaces(g2), "tilted")
})


# --- Coordinator: dataTableCoordinator ---------------------------------------

test_that("dataTableCoordinator() constructs", {
    p <- dataTableCoordinator()
    expect_s4_class(p, "dataTableCoordinator")
    expect_s4_class(p, "viewCoordinator")
})

test_that(".default_view_coordinator returns dataTableCoordinator for in-memory", {
    g <- giotto()
    p <- GiottoClass:::.default_view_coordinator(g)
    expect_s4_class(p, "dataTableCoordinator")
})

test_that("prepareIds() for dataTableCoordinator is identity", {
    ids <- c("a", "b", "c")
    expect_identical(prepareIds(dataTableCoordinator(), ids), ids)
})

test_that("defaultViewCoordinator() defaults to dataTableCoordinator for any source", {
    # ANY signature method
    p <- defaultViewCoordinator(NULL)  # NULL is technically ANY
    # NULL source path goes through .default_view_coordinator's null check
    # before reaching the generic; here we just verify the ANY method
    # returns dataTableCoordinator for an unknown source class
    expect_s4_class(defaultViewCoordinator(structure(list(),
        class = "_unknown_source_class_")), "dataTableCoordinator")
})

test_that("defaultViewCoordinator() S4 dispatch is registrable from downstream", {
    # Simulate GiottoDisk-style registration: define a fake source class
    # and add a method returning a different coordinator. After cleanup,
    # the method is removed so other tests aren't affected.
    setClass("_test_fake_source_", representation = "list",
        where = globalenv())
    on.exit(removeClass("_test_fake_source_", where = globalenv()),
        add = TRUE)

    fake_src <- new("_test_fake_source_")
    # default (no method registered) returns dataTableCoordinator
    expect_s4_class(defaultViewCoordinator(fake_src), "dataTableCoordinator")

    # register a method
    setMethod("defaultViewCoordinator",
        signature(source = "_test_fake_source_"),
        function(source, ...) {
            # use the existing dataTableCoordinator subclass as a stand-in
            new("dataTableCoordinator", misc = list(marker = "downstream"))
        },
        where = globalenv())
    on.exit(removeMethod("defaultViewCoordinator", "_test_fake_source_",
        where = globalenv()), add = TRUE)

    p <- defaultViewCoordinator(fake_src)
    expect_s4_class(p, "dataTableCoordinator")
    expect_identical(p@misc$marker, "downstream")
})


# --- materialize() end-to-end on visium mini ------------------------------

test_that("materialize() with empty view returns equivalent gobject", {
    g <- .fixture_giotto()
    giottoView(g, "empty") <- .empty_view()
    g2 <- materialize(g, "empty")
    expect_equal(nrow(pDataDT(g2)), nrow(pDataDT(g)))
    expect_equal(
        nrow(getSpatialLocations(g2, output = "data.table")),
        nrow(getSpatialLocations(g, output = "data.table"))
    )
})

test_that("materialize() narrows tabular slots by subset predicate", {
    g <- .fixture_giotto()
    n_total <- length(spatIDs(g))
    n_target <- sum(pDataDT(g)$leiden_clus == "1")

    g <- subset(g, leiden_clus == "1", view = "c1")
    g2 <- materialize(g, "c1")

    expect_lt(nrow(pDataDT(g2)), n_total)
    expect_equal(nrow(pDataDT(g2)), n_target)
    expect_equal(ncol(getExpression(g2, output = "matrix")), n_target)
})

test_that("materialize() narrows spatial slots via cell_ID cascade", {
    g <- .fixture_giotto()
    g <- subset(g, leiden_clus == "1", view = "c1")
    g2 <- materialize(g, "c1")

    n_filter <- nrow(pDataDT(g2))
    expect_equal(
        nrow(getSpatialLocations(g2, output = "data.table")), n_filter)
    expect_equal(
        length(spatIDs(getPolygonInfo(g2, return_giottoPolygon = TRUE))),
        n_filter)
})

test_that("materialize() with %in% and env-resident value works (NSE)", {
    g <- .fixture_giotto()
    targets <- c("1", "2")
    g <- subset(g, leiden_clus %in% targets, view = "c12")
    g2 <- materialize(g, "c12")
    expected <- sum(pDataDT(g)$leiden_clus %in% targets)
    expect_equal(nrow(pDataDT(g2)), expected)
})

test_that("materialize() with expression-column predicate routes via spatValues", {
    g <- .fixture_giotto()
    # pick a gene known to be in the panel by literal name to avoid NSE
    gene <- "Gfap"
    skip_if_not(gene %in% rownames(getExpression(g, output = "matrix")),
        sprintf("gene %s not in panel", gene))

    expected <- sum(getExpression(g, output = "matrix")[gene, ] > 0)
    g <- subset(g, Gfap > 0, view = "gfap_pos")
    g2 <- materialize(g, "gfap_pos")
    expect_equal(nrow(pDataDT(g2)), expected)
})

test_that("materialize() with crop narrows via spatLocs extent", {
    g <- .fixture_giotto()
    sl <- getSpatialLocations(g, output = "data.table")
    ext <- c(4000, 5500, -5000, -3500)
    expected <- sum(sl$sdimx >= ext[1L] & sl$sdimx <= ext[2L] &
                    sl$sdimy >= ext[3L] & sl$sdimy <= ext[4L])

    g <- crop(g, ext, view = "ext_crop")
    g2 <- materialize(g, "ext_crop")
    expect_equal(nrow(pDataDT(g2)), expected)
    expect_equal(
        nrow(getSpatialLocations(g2, output = "data.table")), expected)
})

test_that("materialize() with space transforms spatial coords only", {
    g <- .fixture_giotto()
    g <- spin(g, 30, space = "tilted")
    giottoView(g, "empty") <- .empty_view()

    g2 <- materialize(g, "empty", space = "tilted")
    sl_native <- getSpatialLocations(g, output = "data.table")
    sl_tilted <- getSpatialLocations(g2, output = "data.table")

    expect_equal(nrow(sl_tilted), nrow(sl_native))
    expect_false(isTRUE(all.equal(sl_tilted$sdimx, sl_native$sdimx)))
    # tabular slots unchanged in row count
    expect_equal(nrow(pDataDT(g2)), nrow(pDataDT(g)))
})

test_that("materialize() with filter + space combines both", {
    g <- .fixture_giotto()
    g <- spin(g, 30, space = "tilted")
    g <- subset(g, leiden_clus %in% c("1", "2"), view = "c12")

    g2 <- materialize(g, "c12", space = "tilted")

    expected <- sum(pDataDT(g)$leiden_clus %in% c("1", "2"))
    sl_tilted <- getSpatialLocations(g2, output = "data.table")
    sl_native <- getSpatialLocations(g, output = "data.table")
    expect_equal(nrow(sl_tilted), expected)
    expect_false(isTRUE(all.equal(sl_tilted$sdimx, sl_native$sdimx[1:expected])))
})


# --- JIT getter integration -----------------------------------------------

test_that("getCellMetadata respects view = name", {
    g <- .fixture_giotto()
    g <- subset(g, leiden_clus == "1", view = "x")
    n_base <- nrow(getCellMetadata(g, output = "data.table"))
    n_view <- nrow(getCellMetadata(g, view = "x", output = "data.table"))
    expect_lt(n_view, n_base)
    expect_equal(n_view, sum(pDataDT(g)$leiden_clus == "1"))
})

test_that("getCellMetadata rejects ad-hoc view objects (character-only contract)", {
    g <- .fixture_giotto()
    g <- subset(g, leiden_clus == "2", view = "tmp")
    v <- giottoView(g, "tmp")
    expect_error(
        getCellMetadata(g, view = v, output = "data.table"),
        "Must be of type 'string'"
    )
})

test_that("getExpression view narrows columns", {
    g <- .fixture_giotto()
    g <- subset(g, leiden_clus == "1", view = "x")
    n_base <- ncol(getExpression(g, output = "matrix"))
    n_view <- ncol(getExpression(g, view = "x", output = "matrix"))
    expect_lt(n_view, n_base)
})

test_that("getSpatialLocations view + space combined", {
    g <- .fixture_giotto()
    g <- subset(g, leiden_clus == "1", view = "x")
    g <- spin(g, 30, space = "tilted")

    sl_base <- getSpatialLocations(g, output = "data.table")
    sl_v <- getSpatialLocations(g, view = "x", output = "data.table")
    sl_s <- getSpatialLocations(g, space = "tilted", output = "data.table")
    sl_vs <- getSpatialLocations(g, view = "x", space = "tilted",
        output = "data.table")

    expect_equal(nrow(sl_v), sum(pDataDT(g)$leiden_clus == "1"))
    expect_false(isTRUE(all.equal(sl_s$sdimx, sl_base$sdimx)))
    expect_equal(nrow(sl_vs), nrow(sl_v))
    # rotated x for the view+space combo differs from the unrotated view
    expect_false(isTRUE(all.equal(sl_vs$sdimx, sl_v$sdimx)))
})

test_that("getPolygonInfo view narrows; both output forms agree", {
    g <- .fixture_giotto()
    g <- subset(g, leiden_clus == "1", view = "x")
    gp_full <- getPolygonInfo(g, view = "x", return_giottoPolygon = TRUE)
    sv_full <- getPolygonInfo(g, view = "x")  # SpatVector default
    expect_equal(length(spatIDs(gp_full)), nrow(sv_full))
})

test_that("getFeatureMetadata view is no-op (feat-keyed)", {
    g <- .fixture_giotto()
    g <- subset(g, leiden_clus == "1", view = "x")
    n_base <- nrow(getFeatureMetadata(g, output = "data.table"))
    n_view <- nrow(getFeatureMetadata(g, view = "x", output = "data.table"))
    expect_equal(n_view, n_base)
})

test_that("getter without view/space returns unchanged baseline", {
    g <- .fixture_giotto()
    g <- subset(g, leiden_clus == "1", view = "x")
    expect_equal(
        nrow(getCellMetadata(g, output = "data.table")),
        length(spatIDs(g))
    )
})


# --- Resolver cache --------------------------------------------------------

test_that(".cached_surviving_cell_ids memoises within a cache env", {
    g <- .fixture_giotto()
    g <- subset(g, leiden_clus == "1", view = "tmp")
    v <- giottoView(g, "tmp")
    cache <- GiottoClass:::.new_resolver_cache()

    a <- GiottoClass:::.cached_surviving_cell_ids(g, v,
        dataTableCoordinator(), cache)
    expect_true(exists("surviving_ids", envir = cache))
    b <- GiottoClass:::.cached_surviving_cell_ids(g, v,
        dataTableCoordinator(), cache)
    expect_identical(a, b)
})

test_that(".cached_surviving_cell_ids with NULL cache works", {
    g <- .fixture_giotto()
    g <- subset(g, leiden_clus == "1", view = "tmp")
    v <- giottoView(g, "tmp")
    ids <- GiottoClass:::.cached_surviving_cell_ids(g, v,
        dataTableCoordinator(), NULL)
    expect_type(ids, "character")
    expect_equal(length(ids), sum(pDataDT(g)$leiden_clus == "1"))
})


# --- spatValues with view = ----------------------------------------------

test_that("spatValues view = name narrows returned rows", {
    g <- .fixture_giotto()
    g <- subset(g, leiden_clus == "1", view = "tumor")
    sv <- spatValues(g, feats = "leiden_clus", view = "tumor")
    expect_equal(nrow(sv), sum(pDataDT(g)$leiden_clus == "1"))
    expect_true(all(sv$leiden_clus == "1"))
})

test_that("spatValues view = rejects ad-hoc giottoView (character-only contract)", {
    g <- .fixture_giotto()
    g <- subset(g, leiden_clus %in% c("2", "3"), view = "tmp")
    v <- giottoView(g, "tmp")
    expect_error(
        spatValues(g, feats = "leiden_clus", view = v),
        "Must be of type 'string'"
    )
})

test_that("spatValues view = NULL is identity (matches raw)", {
    g <- .fixture_giotto()
    raw <- spatValues(g, feats = "leiden_clus")
    same <- spatValues(g, feats = "leiden_clus", view = NULL)
    expect_identical(raw, same)
})

test_that("spatValues empty view recipe (slotted) returns same as raw", {
    g <- .fixture_giotto()
    giottoView(g, "empty") <- .empty_view()
    raw <- spatValues(g, feats = "leiden_clus")
    via_empty <- spatValues(g, feats = "leiden_clus", view = "empty")
    expect_equal(nrow(via_empty), nrow(raw))
})

test_that("spatValues view = ... matches getCellMetadata view = ... narrowing", {
    g <- .fixture_giotto()
    g <- subset(g, leiden_clus == "1", view = "x")
    sv <- spatValues(g, feats = "leiden_clus", view = "x")
    cm <- getCellMetadata(g, view = "x", output = "data.table")
    # both paths produce the same cell_ID set
    expect_setequal(sv$cell_ID, cm$cell_ID)
})

test_that("spatValues view = ... is consistent with materialize -> spatValues raw", {
    g <- .fixture_giotto()
    g <- subset(g, leiden_clus == "1", view = "c1")
    direct <- spatValues(g, feats = "leiden_clus", view = "c1")
    g_m <- materialize(g, "c1")
    via_materialize <- spatValues(g_m, feats = "leiden_clus")
    # direct narrowing should match the materialized-then-raw path
    expect_setequal(direct$cell_ID, via_materialize$cell_ID)
})

test_that("spatValues view that filters to zero cells returns empty data.table", {
    g <- .fixture_giotto()
    g <- subset(g, leiden_clus == "_nonexistent_cluster_", view = "none")
    sv <- spatValues(g, feats = "leiden_clus", view = "none")
    expect_equal(nrow(sv), 0L)
    expect_true("cell_ID" %in% colnames(sv))
})

test_that("spatValues view = composed predicate AND-narrows correctly", {
    g <- .fixture_giotto()
    # two subset steps chained — both should apply (intersection semantics)
    g <- subset(g, leiden_clus %in% c("1", "2"), view = "composed")
    g <- subset(g, total_expr > median(total_expr), view = "composed")
    sv <- spatValues(g, feats = "leiden_clus", view = "composed")
    cm <- pDataDT(g)
    n_expected <- sum(
        cm$leiden_clus %in% c("1", "2") &
            cm$total_expr > median(cm$total_expr))
    expect_equal(nrow(sv), n_expected)
})

test_that("spatValues view = on giottoMulti narrows joint output", {
    mg <- .fixture_gmulti()
    sv_raw <- spatValues(mg, feats = "leiden_clus")
    mg <- subset(mg, leiden_clus == "1", view = "c1")
    sv_v <- spatValues(mg, feats = "leiden_clus", view = "c1")
    expect_lt(nrow(sv_v), nrow(sv_raw))
    expect_true(all(sv_v$leiden_clus == "1"))
    # global cell_ID format still present
    expect_true(any(grepl("^a::", sv_v$cell_ID)) ||
                any(grepl("^b::", sv_v$cell_ID)))
})

test_that("spatValues view re-entry guard prevents recursion", {
    # Set the option as if we're mid-resolution; an outer spatValues
    # call with view should drop the view arg rather than recurse.
    g <- .fixture_giotto()
    g <- subset(g, leiden_clus == "1", view = "x")
    options(giotto.spatValues_view_active = TRUE)
    on.exit(options(giotto.spatValues_view_active = FALSE), add = TRUE)
    sv_v <- spatValues(g, feats = "leiden_clus", view = "x")
    sv_raw <- spatValues(g, feats = "leiden_clus")
    # with guard active, view= is dropped; result matches raw
    expect_equal(nrow(sv_v), nrow(sv_raw))
})

test_that("spatValues space = NULL is no-op (currently accepted but not value-transforming)", {
    g <- .fixture_giotto()
    g <- spin(g, 30, space = "tilted")
    sv_native <- spatValues(g, feats = "leiden_clus")
    sv_space <- spatValues(g, feats = "leiden_clus", space = "tilted")
    # space does NOT transform value columns (leiden_clus is a label);
    # rows and values are unchanged
    expect_equal(nrow(sv_native), nrow(sv_space))
    expect_setequal(sv_native$leiden_clus, sv_space$leiden_clus)
})


# --- gmulti dispatch ------------------------------------------------------

test_that("giottoView accessors work on giottoMulti via gAny", {
    mg <- .fixture_gmulti()
    g <- giotto()
    g <- subset(g, leiden_clus == "1", view = "tmp")
    v <- giottoView(g, "tmp")
    giottoView(mg, "tumor") <- v
    expect_identical(giottoViews(mg), "tumor")
    out <- giottoView(mg, "tumor")
    expect_s4_class(out, "giottoView")
})

test_that("giottoSpace accessors work on giottoMulti via gAny", {
    mg <- .fixture_gmulti()
    mg <- spin(mg, 30, space = "atlas", samples = "a")
    mg <- spin(mg, 45, space = "atlas", samples = "b")
    expect_identical(giottoSpaces(mg), "atlas")
    out <- giottoSpace(mg, "atlas")
    expect_identical(names(out), c("a", "b"))
    expect_length(out, 2L)
})

test_that("`[` scopes a space to a named child", {
    mg <- .fixture_gmulti()
    mg <- spin(mg, 30, space = "atlas", samples = "a")
    mg <- spin(mg, 45, space = "atlas", samples = "b")
    s <- giottoSpace(mg, "atlas")

    sa <- s["a"]
    expect_s4_class(sa, "giottoSpace")
    expect_identical(names(sa), "a")
    expect_length(s[["a"]], 1L)
    expect_equal(s[["a"]][[1L]]$op, "spin")
    expect_equal(s[["a"]][[1L]]$args$angle, 30)
    expect_equal(s[["b"]][[1L]]$args$angle, 45)
})

test_that("an unscoped step answers for every sample, named or not", {
    s <- giottoSpace(spin(giotto(), 15, space = "s"), "s")
    expect_equal(s[["any_sample_name"]][[1L]]$args$angle, 15)
    # and NA -- no sample identity at all -- resolves the same way
    expect_equal(s[[NA_character_]][[1L]]$args$angle, 15)
})

test_that("`[j]` resolves for j and drops the scopes it has spent", {
    # the {GiottoDisk} resolver seam: it scopes with `space[samp]` and
    # then reads back with `[[NA_character_]]`, by which point the
    # handle no longer knows which sample it holds. Dropping the scopes is
    # what makes that second read correct rather than empty.
    mg <- .fixture_gmulti()
    mg <- spin(mg, 30, space = "atlas")
    mg <- spatShift(mg, dx = 10, space = "atlas", samples = "b")
    s <- giottoSpace(mg, "atlas")

    expect_identical(vapply(s["b"][[NA_character_]],
        function(x) x$op, character(1L)), c("spin", "spatShift"))
    expect_identical(vapply(s["a"][[NA_character_]],
        function(x) x$op, character(1L)), "spin")
    expect_identical(names(s["b"]), "b")

    # with nothing scoped, every step survives narrowing -- rescoped to
    # the sample asked for, rather than dropped or stripped
    flat <- giottoSpace(spin(giotto(), 15, space = "s"), "s")
    expect_length(flat["a"], length(flat))
    expect_identical(flat["a"]@steps[[1L]]$op, "spin")
    expect_identical(flat["a"]@steps[[1L]]$samples, "a")
    expect_identical(flat["anything"]@steps[[1L]]$samples, "anything")
    # ... and narrowing to nobody is refused
    expect_error(flat[NA_character_], "cannot narrow to NA")
})

test_that("recording onto an unused name declares a per-sample space", {
    # `samples =` does NOT decide the kind -- scoping says which samples
    # move, not whether they interact -- so neither call gets a combined one
    mg <- .fixture_gmulti()
    expect_s4_class(giottoSpace(spatShift(mg, dx = 5, space = "p"), "p"),
        "perSampleSpace")
    expect_s4_class(giottoSpace(spin(mg, 5, space = "q", samples = "a"), "q"),
        "perSampleSpace")
    # membership starts empty and grows by key. Seeding it from the
    # object's children would make it a restatement of names(@objects),
    # and then neither the slot nor its growth would say anything.
    expect_identical(names(giottoSpace(
        spatShift(mg, dx = 5, space = "p"), "p")), character())
    grown <- spin(mg, 5, space = "q", samples = "a")
    expect_identical(names(giottoSpace(grown, "q")), "a")
    grown <- spatShift(grown, dx = 1, space = "q", samples = "b")
    expect_identical(names(giottoSpace(grown, "q")), c("a", "b"))
})

test_that("a perSampleSpace is declaration-only, then per-sample editable", {
    # the one kind `space = "<name>"` will not create, because it is the
    # rarer intent. Once declared it takes scoped steps like any other:
    # two sections each rotated upright is per-sample, since nothing about
    # that puts them in a shared coordinate system.
    mg <- .fixture_gmulti()
    giottoSpace(mg, "upright") <- perSampleSpace()
    mg <- spin(mg, 30, space = "upright", samples = "a")
    mg <- spin(mg, 45, space = "upright", samples = "b")
    mg <- spatShift(mg, dx = 1, space = "upright")

    sp <- giottoSpace(mg, "upright")
    expect_s4_class(sp, "perSampleSpace")
    expect_equal(sp[["a"]][[1L]]$args$angle, 30)
    expect_equal(sp[["b"]][[1L]]$args$angle, 45)
    # the broadcast step reaches both
    expect_length(sp[["a"]], 2L)
    expect_length(sp[["b"]], 2L)
})

test_that("names() reports the samples the recipe mentions", {
    mg <- .fixture_gmulti()
    mg <- spin(mg, 30, space = "atlas", samples = c("a", "b"))
    expect_identical(names(giottoSpace(mg, "atlas")), c("a", "b"))

    # a perSampleSpace mentions only what is scoped. That list is not its
    # COVERAGE -- an unscoped step reaches samples named nowhere -- which
    # is why a consumer sizing a job reads the object, not the space.
    giottoSpace(mg, "each") <- perSampleSpace()
    expect_identical(names(giottoSpace(mg, "each")), character())
    mg <- spin(mg, 10, space = "each", samples = "a")
    mg <- spatShift(mg, dx = 1, space = "each")
    expect_identical(names(giottoSpace(mg, "each")), "a")
    expect_length(giottoSpace(mg, "each")[["b"]], 1L)   # b still covered
})

test_that("membership can be declared for a member that never moves", {
    mg <- .fixture_gmulti()
    giottoSpace(mg, "atlas") <- combinedSpace(c("a", "b"))
    mg <- spatShift(mg, dx = 9000, space = "atlas", samples = "b")
    expect_identical(names(giottoSpace(mg, "atlas")), c("a", "b"))
    expect_identical(giottoSpace(mg, "atlas")[["a"]], list())
})

test_that("a step cannot scope to a non-member, by construction", {
    # membership is DERIVED from the steps, so naming a sample in one makes
    # it a member. There is no second slot to fall out of step with the
    # recipe, which is why this needs no validity check.
    sp <- combinedSpace("a", name = "x")
    sp <- spin(sp, 1, samples = "zzz")
    expect_true(validObject(sp))
    expect_identical(names(sp), c("a", "zzz"))
})

test_that("a membership step declares without transforming", {
    sp <- combinedSpace(c("a", "b"), name = "atlas")
    expect_identical(names(sp), c("a", "b"))
    # it is not applied: `[[` hands back things the resolver will do.call()
    expect_identical(sp[["a"]], list())
    expect_identical(sp[["b"]], list())
    expect_identical(vapply(sp@steps, function(s) s$type,
        character(1L)), "member")

    # and it survives narrowing, so a scoped handle still knows its member
    sp <- spatShift(sp, dx = 9000, samples = "b")
    expect_identical(names(sp["b"]), "b")
    expect_identical(names(sp["a"]), "a")
    expect_length(sp["a"][[NA_character_]], 0L)
    expect_length(sp["b"][[NA_character_]], 1L)
})

test_that("`+` refuses a merge that would have no job size", {
    mg <- .fixture_gmulti()
    giottoSpace(mg, "atlas") <- combinedSpace(name = "atlas")
    mg <- spin(mg, 30, space = "atlas", samples = "a")
    giottoSpace(mg, "each") <- perSampleSpace()
    expect_error(giottoSpace(mg, "atlas") + giottoSpace(mg, "each"),
        "cannot compose a")
    # and two spaces that are not the same space. Same KIND, or the
    # mixed-kind check above fires first and this asserts nothing.
    giottoSpace(mg, "other") <- combinedSpace(name = "other")
    mg <- spin(mg, 10, space = "other", samples = "b")
    expect_error(giottoSpace(mg, "atlas") + giottoSpace(mg, "other"),
        "cannot compose spaces")
})

test_that("there is no name for the native frame", {
    # A sentinel name would be a second spelling of a value R already has,
    # which every consumer would then have to know meant the same thing.
    # And a transform cannot be applied to the native frame anyway -- the
    # result would not be native -- so the name could only ever have stood
    # for an empty recipe.
    g <- giotto()
    expect_length(giottoSpaces(g), 0L)
    expect_error(giottoSpace(g, ":default:"), "not a registered space")

    # `space = NULL` is the native frame, and on a plain giotto a transform
    # with no space is eager rather than recorded
    expect_s4_class(spin(g, 30), "giotto")
    expect_length(giottoSpaces(spin(g, 30)), 0L)
})

test_that("resolving for a sample with nothing scoped to it is empty", {
    mg <- .fixture_gmulti()
    mg <- spin(mg, 15, space = "atlas", samples = "a")
    s <- giottoSpace(mg, "atlas")
    expect_identical(s[["b"]], list())
    # ... and NA resolves to the sole mentioned sample, which is the rule
    # the `sp["a"][[NA_character_]]` seam relies on
    expect_length(s[[NA_character_]], 1L)
})

test_that("materialize on giottoMulti narrows children via selectSamples", {
    mg <- .fixture_gmulti()
    mg <- selectSamples(mg, "a", view = "only_a")
    out <- materialize(mg, "only_a")
    expect_identical(names(out@objects), "a")
})

test_that("materialize on giottoMulti applies view per-child", {
    mg <- .fixture_gmulti()
    mg <- subset(mg, leiden_clus == "1", view = "c1")
    out <- materialize(mg, "c1")
    expect_named(out@objects, c("a", "b"))
    # each child has been narrowed
    n_a <- nrow(pDataDT(out@objects$a))
    n_b <- nrow(pDataDT(out@objects$b))
    expect_lt(n_a, length(spatIDs(mg@objects$a)))
    expect_lt(n_b, length(spatIDs(mg@objects$b)))
})

test_that("materialize on giottoMulti scopes space per-child", {
    mg <- .fixture_gmulti()
    mg <- spin(mg, 30, space = "atlas", samples = "a")
    mg <- spin(mg, 45, space = "atlas", samples = "b")
    giottoView(mg, "empty") <- .empty_view()
    out <- materialize(mg, "empty", space = "atlas")

    sl_a_native <- getSpatialLocations(mg@objects$a, output = "data.table")
    sl_b_native <- getSpatialLocations(mg@objects$b, output = "data.table")
    sl_a_post <- getSpatialLocations(out@objects$a, output = "data.table")
    sl_b_post <- getSpatialLocations(out@objects$b, output = "data.table")

    # both children transformed — but by different angles
    expect_false(isTRUE(all.equal(sl_a_post$sdimx, sl_a_native$sdimx)))
    expect_false(isTRUE(all.equal(sl_b_post$sdimx, sl_b_native$sdimx)))
    # different angles → ratios of transformed-to-native should differ
    # (we don't compute the exact rotation expectation here, just that
    # the two children's transforms are not identical)
    expect_false(isTRUE(all.equal(
        sl_a_post$sdimx - sl_a_native$sdimx,
        sl_b_post$sdimx - sl_b_native$sdimx)))
})

test_that("spatValues on giottoMulti finds features in joint cell_metadata", {
    mg <- .fixture_gmulti()
    sv <- spatValues(mg, feats = "leiden_clus")
    expect_equal(nrow(sv), sum(lengths(lapply(mg@objects, spatIDs))))
    expect_true(all(c("cell_ID", "leiden_clus") %in% colnames(sv)))
    # global cell_IDs use the sample::local_id format
    expect_true(any(grepl("^a::", sv$cell_ID)))
    expect_true(any(grepl("^b::", sv$cell_ID)))
})

test_that("spatValues on giottoMulti finds features in joint expression", {
    mg <- .fixture_gmulti()
    gene <- rownames(getExpression(mg, output = "matrix"))[1L]
    sv <- spatValues(mg, feats = gene)
    expect_equal(nrow(sv), sum(lengths(lapply(mg@objects, spatIDs))))
    expect_true(gene %in% colnames(sv))
})

test_that("materialize on giottoMulti narrows joint shared slots", {
    mg <- .fixture_gmulti()
    mg <- subset(mg, leiden_clus == "1", view = "c1")
    n_total <- nrow(pDataDT(mg))
    n_target <- sum(pDataDT(mg)$leiden_clus == "1")

    out <- materialize(mg, "c1")

    # joint cell_metadata narrowed
    expect_equal(nrow(pDataDT(out)), n_target)
    expect_lt(nrow(pDataDT(out)), n_total)
    # joint expression narrowed in column count
    expect_equal(ncol(getExpression(out, output = "matrix")), n_target)
})

test_that("materialize via slotted view name dispatches on multi", {
    mg <- .fixture_gmulti()
    mg <- selectSamples(mg, "a", view = "x")
    out <- materialize(mg, "x")
    expect_identical(names(out@objects), "a")
})


# Q7 — recipes are plain data and survive serialization ####
#
# The whole reason steps are tagged lists rather than S4: a recipe has to
# survive saveRDS and reach a parallel worker. Before Q7 that claim was
# false — viewFilter carried an environment and viewCrop could hold a
# terra pointer.

test_that("steps and recipes are plain tagged lists, not S4", {
    g <- giotto()
    g <- subset(g, cluster == "A", view = "v")
    g <- crop(g, c(0, 10, 0, 10), view = "v")
    g <- selectSamples(g, "a", view = "v")
    v <- giottoView(g, "v")
    # Q7 put the guarantees on the STEPS: no closure, no external
    # pointer. The container is a class; that changes nothing here.
    for (s in v@steps) {
        expect_type(s, "list")
        expect_false(isS4(s))
        expect_true(is.character(s$type))
    }
    expect_identical(
        vapply(v@steps, function(s) s$type, character(1L)),
        c("filter", "crop", "samples"))

    s <- giottoSpace(spin(giotto(), 30, space = "s"), "s")
    step <- s[[NA_character_]][[1L]]
    expect_type(step, "list")
    expect_false(isS4(step))
    expect_identical(step$type, "transform")
})

test_that("a filter step carries no environment; the predicate is a string", {
    target <- "A"
    g <- giotto()
    g <- subset(g, cluster == target, view = "tmp")
    v <- giottoView(g, "tmp")
    step <- v@steps[[1L]]
    expect_null(step$env)
    expect_type(step$predicate, "character")
    # the VALUE of target was substituted in at record time, so the recipe
    # does not change when the binding does
    expect_identical(step$predicate, 'cluster == "A"')
    target <- "B"
    expect_identical(v@steps[[1L]]$predicate, 'cluster == "A"')
})

test_that("a view recipe round-trips through saveRDS and still resolves", {
    g <- .fixture_giotto()
    g <- subset(g, leiden_clus == 1, view = "tmp")
    v <- giottoView(g, "tmp")
    f <- tempfile(fileext = ".rds")
    on.exit(unlink(f), add = TRUE)
    saveRDS(v, f)
    v2 <- readRDS(f)
    expect_identical(v@steps, v2@steps)

    # and the deserialized recipe resolves to the same cells
    giottoView(g, "a") <- v
    giottoView(g, "b") <- v2
    expect_identical(
        pDataDT(materialize(g, "a"))$cell_ID,
        pDataDT(materialize(g, "b"))$cell_ID)
})

test_that("a space recipe round-trips through saveRDS and still resolves", {
    g <- .fixture_giotto()
    s <- giottoSpace(spatShift(giotto(), dx = 100, space = "s"), "s")
    f <- tempfile(fileext = ".rds")
    on.exit(unlink(f), add = TRUE)
    saveRDS(s, f)
    s2 <- readRDS(f)
    expect_identical(s[[1L]], s2[[1L]])

    giottoSpace(g, "s2") <- s2
    sl <- getSpatialLocations(g, output = "data.table")
    sl_shift <- getSpatialLocations(g, space = "s2", output = "data.table")
    expect_equal(sl_shift$sdimx, sl$sdimx + 100)
})

test_that("a recorded crop region is WKT, and terra objects do not leak in", {
    poly <- terra::vect(rbind(
        c(0, 0), c(10, 0), c(10, 10), c(0, 10), c(0, 0)
    ), type = "polygons")
    g <- giotto()
    g <- crop(g, poly, view = "tmp")
    v <- giottoView(g, "tmp")
    region <- v@steps[[1L]]$region
    expect_type(region, "character")
    expect_match(region, "^POLYGON")
    # serializable: a SpatVector would not survive this
    f <- tempfile(fileext = ".rds")
    on.exit(unlink(f), add = TRUE)
    saveRDS(v, f)
    expect_identical(readRDS(f)@steps[[1L]]$region, region)
})

test_that("WKT round-trip does not shift a crop boundary", {
    # PLAN Q7 flagged emitted-WKT precision as unverified. Check that a
    # numeric extent survives numeric -> WKT -> SpatVector -> extent.
    exts <- list(
        c(0, 100, 0, 100),
        c(-5000.5, -3499.25, 1234.125, 5678.0625),
        c(1e-8, 2e-8, 3e-8, 4e-8),
        c(1e7, 1e7 + 0.001, -1e7, -1e7 + 0.001)
    )
    for (e in exts) {
        wkt <- GiottoClass:::.normalize_crop_region(e)
        got <- terra::ext(terra::vect(wkt))[]
        expect_equal(unname(got), e, tolerance = 0,
            info = paste("extent:", paste(e, collapse = ", ")))
    }
})

test_that("crop accepts WKT directly as the canonical entry", {
    wkt <- "POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0))"
    g <- giotto()
    g <- crop(g, wkt, view = "tmp")
    v <- giottoView(g, "tmp")
    expect_identical(v@steps[[1L]]$region, wkt)
    expect_error(crop(giotto(), view = "v", "not wkt at all"), "not valid WKT")
})

test_that("multi-feature crop regions are unioned into one geometry", {
    p1 <- terra::vect(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)),
        type = "polygons")
    p2 <- terra::vect(rbind(c(5, 5), c(6, 5), c(6, 6), c(5, 6), c(5, 5)),
        type = "polygons")
    both <- rbind(p1, p2)
    expect_equal(nrow(both), 2)
    g <- giotto()
    g <- crop(g, both, view = "tmp")
    v <- giottoView(g, "tmp")
    expect_length(v@steps[[1L]]$region, 1L)
    # the union's extent spans both parts
    expect_equal(unname(terra::ext(terra::vect(v@steps[[1L]]$region))[]),
        c(0, 6, 0, 6))
})

test_that("the inline cap rejects an oversized query set", {
    withr::local_options(list(giotto.view_crop_inline_max = 2L))
    wkts <- rep("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))", 3L)
    expect_error(crop(giotto(), view = "v", wkts), "inline cap")
})

test_that("transform args are whitelisted to serializable types", {
    # a terra object as a transform arg is exactly what breaks saveRDS
    sv <- terra::vect(rbind(c(0, 0), c(1, 1)), type = "points")
    expect_error(spatShift(giotto(), space = "s", dx = sv), "cannot be recorded")
    # atomic vectors, numeric matrices and affine2d are accepted
    expect_no_error(spatShift(giotto(), space = "s", dx = 1, dy = 2))
    expect_no_error(affine(giotto(), space = "s", matrix(c(1, 0, 0, 1), nrow = 2)))
    expect_no_error(flip(giotto(), space = "s", direction = "vertical"))
})

test_that("an unknown step type is rejected at record and validate time", {
    expect_error(GiottoClass:::.validate_view_step(list(type = "nope")),
        "unknown type")
    expect_error(GiottoClass:::.validate_space_step(
        list(type = "transform", op = "teleport", args = list())),
        "unknown transform")
    # a malformed hand-built recipe is caught by the recipe validator,
    # which the setter runs now that there is no class validity function
    v <- list(steps = list(list(type = "filter",
        predicate = "cluster ==", scope_args = list())))
    expect_error(GiottoClass:::.validate_view(v), "does not parse")
    gg <- giotto()
    expect_error({ giottoView(gg, "bad") <- v }, "does not parse")
})

.relate_regions <- list(
    rect = terra::vect(terra::ext(c(0, 10, 0, 10))),
    tri = terra::vect(rbind(c(0, 0), c(10, 0), c(5, 10), c(0, 0)),
        type = "polygons"),
    empty = terra::vect(terra::ext(c(100, 110, 100, 110))),
    all = terra::vect(terra::ext(c(-100, 100, -100, 100)))
)

.relate_pts <- function() {
    as.points(createSpatLocsObj(data.table::data.table(
        cell_ID = c("a", "b", "c", "d"),
        sdimx = c(1, 9, 5, 50), sdimy = c(1, 9, 1, 50))))
}

test_that("a rectangular region takes the AABB path; a polygon does not", {
    expect_true(GiottoClass:::.region_is_rect(.relate_regions$rect))
    expect_false(GiottoClass:::.region_is_rect(.relate_regions$tri))
    # both shapes go through the same primitive and must agree with terra
    pts <- .relate_pts()
    for (nm in names(.relate_regions)) {
        region <- .relate_regions[[nm]]
        expect_setequal(
            spatRelate(pts, region, relation = "intersects")$cell_ID,
            pts$cell_ID[terra::is.related(pts, region, "intersects")]
        )
    }
})

test_that("disjoint is the exact complement of intersects", {
    pts <- .relate_pts()
    all_ids <- pts$cell_ID
    for (nm in names(.relate_regions)) {
        region <- .relate_regions[[nm]]
        hit <- spatRelate(pts, region, relation = "intersects")$cell_ID
        miss <- spatRelate(pts, region, relation = "disjoint")$cell_ID
        expect_setequal(c(hit, miss), all_ids)
        expect_length(intersect(hit, miss), 0L)
        # and it is what terra would have said
        expect_setequal(miss,
            all_ids[terra::is.related(pts, region, "disjoint")])
    }
})

test_that("other predicates agree with terra on both region shapes", {
    pts <- .relate_pts()
    # left: the sf/sedona spelling a recipe records. right: terra's.
    rels <- c(within = "within", touches = "touches", covered_by = "coveredby")
    for (rel in names(rels)) {
        for (nm in c("rect", "tri")) {
            region <- .relate_regions[[nm]]
            expect_setequal(
                spatRelate(pts, region, relation = rel)$cell_ID,
                pts$cell_ID[terra::is.related(pts, region, rels[[rel]])]
            )
        }
    }
})


# crop: the geometry choice is declared, not inferred ####
#
# `geom` says what represents a cell when the predicate runs — its centroid
# or its polygon. Declared on the step so a saved recipe states which
# question it asks, and so the backed resolvers can route on the same field
# instead of on target storage kind.

.box_at_centre <- function(g, half = 1500) {
    sl <- getSpatialLocations(g, output = "data.table")
    c(mean(range(sl$sdimx)) - half, mean(range(sl$sdimx)) + half,
      mean(range(sl$sdimy)) - half, mean(range(sl$sdimy)) + half)
}

test_that("geom defaults to centroid and is recorded on the step", {
    g <- giotto()
    g <- crop(g, c(0, 10, 0, 10), view = "tmp")
    v <- giottoView(g, "tmp")
    expect_identical(v@steps[[1L]]$geom, "centroid")
    v2 <- giottoView(crop(giotto(), c(0, 10, 0, 10), geom = "poly",
        view = "v"), "v")
    expect_identical(v2@steps[[1L]]$geom, "poly")
})

test_that("the centroid-capable relations record geom = centroid", {
    # measured against terra, not assumed: a point either falls in the
    # region or not (intersects/disjoint), is strictly interior (within),
    # or is on the boundary (touches). All four are well defined.
    for (r in c("intersects", "disjoint", "within", "touches")) {
        g <- crop(giotto(), c(0, 10, 0, 10), relation = r, view = "tmp")
        v <- giottoView(g, "tmp")
        expect_identical(v@steps[[1L]]$geom, "centroid", info = r)
    }
})

test_that("the poly-only relations warn and record geom = poly", {
    # these are always FALSE against a point, so asking for them on a
    # centroid is an empty result rather than an approximation
    for (r in c("contains", "covers", "overlaps", "crosses")) {
        expect_warning(
            v <- giottoView(crop(giotto(), c(0, 10, 0, 10),
                relation = r, view = "v"), "v"),
            "always FALSE", info = r)
        expect_identical(v@steps[[1L]]$geom, "poly", info = r)
    }
    # declaring poly explicitly is silent
    expect_no_warning(
        crop(giotto(), view = "v", c(0, 10, 0, 10), relation = "contains",
            geom = "poly"))
})

test_that("an unavailable relation or geom is rejected", {
    expect_error(crop(giotto(), view = "v", c(0, 10, 0, 10),
        relation = "nonsense"), "not available")
    expect_error(crop(giotto(), view = "v", c(0, 10, 0, 10), geom = "blah"))
})

test_that("crop(g, view = ) inherits the vocabulary and the promotion", {
    g <- .fixture_giotto()
    expect_error(crop(g, c(0, 10, 0, 10), relation = "nonsense",
        view = "v"), "not available")
    # nothing recorded on the failed call
    expect_false("v" %in% giottoViews(g))

    expect_warning(g2 <- crop(g, c(0, 10, 0, 10), relation = "contains",
        view = "v"), "always FALSE")
    expect_identical(giottoView(g2, "v")@steps[[1L]]$geom, "poly")
})

test_that("intersects and disjoint partition the cell set", {
    # also the guard on the disjoint no-AABB fix: survivors lie OUTSIDE
    # the region, so a bbox pre-filter would drop exactly them. This test
    # fails against the pre-fix code.
    g <- .fixture_giotto()
    box <- .box_at_centre(g)
    g <- crop(g, box, relation = "intersects", view = "i")
    g <- crop(g, box, relation = "disjoint", view = "d")
    n_i <- length(pDataDT(materialize(g, "i"))$cell_ID)
    n_d <- length(pDataDT(materialize(g, "d"))$cell_ID)
    expect_identical(n_i + n_d, nrow(getSpatialLocations(g,
        output = "data.table")))
    expect_gt(n_i, 0L)
    expect_gt(n_d, 0L)
})

test_that("geom changes the answer at a fixed relation", {
    # the point of the parameter: same question, two representations.
    # `within` on a centroid asks "is the centroid inside"; on the polygon
    # it asks "is the whole cell inside", which is strictly stricter.
    g <- .fixture_giotto()
    box <- .box_at_centre(g)
    g <- crop(g, box, relation = "within", geom = "centroid", view = "w_c")
    g <- crop(g, box, relation = "within", geom = "poly", view = "w_p")
    cells_c <- pDataDT(materialize(g, "w_c"))$cell_ID
    cells_p <- pDataDT(materialize(g, "w_p"))$cell_ID
    expect_lt(length(cells_p), length(cells_c))
    expect_true(all(cells_p %in% cells_c))
})

test_that("geom = poly with no polygon source errors, naming the remedy", {
    m <- matrix(0, nrow = 2, ncol = 3,
        dimnames = list(c("f1", "f2"), c("c1", "c2", "c3")))
    g <- createGiottoObject(expression = m, verbose = FALSE,
        spatial_locs = data.frame(cell_ID = c("c1", "c2", "c3"),
            sdimx = 1:3, sdimy = 1:3))
    expect_null(g@spatial_info)

    g <- crop(g, c(0, 10, 0, 10), geom = "poly", view = "p")
    expect_error(materialize(g, "p"), "no polygon source")
    expect_error(materialize(g, "p"), "geom = \"centroid\"")

    # the centroid arm resolves on the same object
    g <- crop(g, c(0, 10, 0, 10), view = "c")
    expect_length(pDataDT(materialize(g, "c"))$cell_ID, 3L)
})

test_that("one recipe narrows every cell-keyed slot identically", {
    # IMPLEMENTATION_viewspace.md section 4: one usage layer per predicate
    g <- .fixture_giotto()
    box <- .box_at_centre(g)
    g <- crop(g, box, relation = "within", geom = "poly", view = "w")

    from_meta <- sort(getCellMetadata(g, view = "w",
        output = "data.table")$cell_ID)
    from_expr <- sort(colnames(getExpression(g, view = "w",
        output = "matrix")))
    from_locs <- sort(getSpatialLocations(g, view = "w",
        output = "data.table")$cell_ID)
    expect_identical(from_meta, from_expr)
    expect_identical(from_meta, from_locs)
})

test_that("the resolver cache is memoization only, never routing", {
    g <- .fixture_giotto()
    box <- .box_at_centre(g)
    g <- crop(g, box, relation = "within", geom = "poly", view = "tmp")
    v <- giottoView(g, "tmp")
    co <- dataTableCoordinator()

    no_cache <- GiottoClass:::.cached_surviving_cell_ids(g, v, co, NULL)
    cache <- GiottoClass:::.new_resolver_cache()
    with_cache <- GiottoClass:::.cached_surviving_cell_ids(g, v, co, cache)
    again <- GiottoClass:::.cached_surviving_cell_ids(g, v, co, cache)
    expect_identical(no_cache, with_cache)
    expect_identical(with_cache, again)
})

test_that("a hand-poked incoherent step fails validity", {
    g <- giotto()
    g <- crop(g, c(0, 10, 0, 10), view = "tmp")
    v <- giottoView(g, "tmp")
    # a recipe is a plain list, so nothing stops a field being poked; the
    # validator is what catches it, on demand or at the setter
    v@steps[[1L]]$relation <- "contains"
    expect_error(GiottoClass:::.validate_view(v), "cannot be evaluated")
    expect_error({ giottoView(g, "poked") <- v }, "cannot be evaluated")
})

test_that("geom survives the saveRDS round-trip", {
    v <- giottoView(crop(giotto(), c(0, 10, 0, 10), relation = "within",
        geom = "poly", view = "v"), "v")
    f <- tempfile(fileext = ".rds")
    on.exit(unlink(f), add = TRUE)
    saveRDS(v, f)
    expect_identical(readRDS(f)@steps, v@steps)
})

test_that("samples = NULL broadcasts over the keys already recorded", {
    # Replaces A8's `+`-vs-pipe non-commutativity rule. That rule existed
    # because `+` merged key sets and a later pipe hit whatever had been
    # merged so far, so the same expression meant different things
    # depending on build order. An omitted `samples` is now a property of
    # the step itself, so it reaches every sample regardless of order.
    mg <- .fixture_gmulti()
    mg <- spin(mg, 30, space = "atlas", samples = "a")
    mg <- spin(mg, 45, space = "atlas", samples = "b")
    mg <- spatShift(mg, dx = 100, space = "atlas")   # no samples = broadcast
    s <- giottoSpace(mg, "atlas")
    expect_length(s[["a"]], 2L)
    expect_length(s[["b"]], 2L)
    expect_identical(s[["a"]][[2L]]$op, "spatShift")
    expect_identical(s[["b"]][[2L]]$op, "spatShift")
})

test_that("a mistyped sample name is rejected at record time", {
    # `.space_record()` creates whatever key it is handed, so an unchecked
    # typo would record a chain no child ever resolves against.
    mg <- .fixture_gmulti()
    expect_error(spin(mg, 30, space = "atlas", samples = "typo"),
        "unknown sample")
})


# --- the handle API: access, append, export --------------------------------
#
# Q7's guarantees live on the STEPS, so the containers are free to be
# classes. What the classes buy is a surface for the three things a recipe
# has to support: reading it, adding to it, and getting the plain form back
# out. These test that surface rather than the storage shape.

.demo_view <- function() {
    g <- giotto()
    g <- subset(g, cluster == "A", view = "v")
    g <- crop(g, c(0, 10, 0, 10), relation = "within", view = "v")
    g <- selectSamples(g, "a", "b", view = "v")
    giottoView(g, "v")
}

.demo_space <- function() {
    mg <- .fixture_gmulti()
    # a cross-sample layout is declared, not recorded into being
    giottoSpace(mg, "atlas") <- combinedSpace(name = "atlas")
    mg <- spin(mg, 30, space = "atlas", samples = "a")
    mg <- spatShift(mg, dx = 100, space = "atlas", samples = "b")
    giottoSpace(mg, "atlas")
}

test_that("giottoView `[` preserves the class and stays appendable", {
    v <- .demo_view()
    expect_length(v, 3L)
    expect_identical(names(v), c("filter", "crop", "samples"))

    expect_s4_class(v[2L], "giottoView")
    expect_identical(names(v[2L]), "crop")
    expect_identical(names(v[-2L]), c("filter", "samples"))
    expect_identical(names(v[c(1L, 3L)]), c("filter", "samples"))
    # class-preserving means a subset can still be built on
    expect_length(crop(v[1L], c(0, 5, 0, 5)), 2L)
})

test_that("giottoView `[[` extracts the step; `[i, j]` one attribute", {
    v <- .demo_view()
    step <- v[[2L]]
    expect_type(step, "list")
    expect_false(isS4(step))
    expect_identical(step$type, "crop")
    expect_identical(v[2L, "relation"], "within")
    expect_identical(v[1L, "type"], "filter")
})

test_that("giottoView `[[<-` edits and drops steps, and validates", {
    v <- .demo_view()
    v[[2L]] <- NULL
    expect_identical(names(v), c("filter", "samples"))

    v2 <- .demo_view()
    step <- v2[[2L]]
    step$relation <- "intersects"
    v2[[2L]] <- step
    expect_identical(v2[2L, "relation"], "intersects")
    # a hand-poked step that cannot answer its own relation is rejected
    bad <- v2[[2L]]
    bad$relation <- "contains"
    bad$geom <- "centroid"
    expect_error({ v2[[2L]] <- bad }, "cannot be evaluated")
})

test_that("giottoSpace `[` / `[[` index by sample, or by step position", {
    sp <- .demo_space()
    expect_identical(names(sp), c("a", "b"))   # samples, not the space name
    expect_identical(sp@name, "atlas")
    expect_length(sp, 2L)                      # step count

    # character -> sample: the ordered step list that applies to it
    expect_identical(sp[["a"]][[1L]]$op, "spin")
    expect_identical(sp[["b"]][[1L]]$op, "spatShift")
    expect_identical(sp[["nobody"]], list())

    # numeric -> step position, raw and scopes intact
    expect_identical(sp[[1L]]$op, "spin")
    expect_identical(sp[[1L]]$samples, "a")
    expect_length(sp[2L], 1L)
    expect_identical(sp[2L]@steps[[1L]]$op, "spatShift")

    # `[` narrows and stays appendable
    scoped <- sp["a"]
    expect_s4_class(scoped, "giottoSpace")
    expect_identical(names(scoped), "a")
    expect_length(spin(scoped, 45)[["a"]], 2L)

    # a second index is the old frame-then-sample form
    expect_error(sp[["atlas", "a"]], "indexed on one axis")
    expect_error(sp["atlas", "a"], "indexed on one axis")
})

test_that("`giottoSpace(g)` with no name gives the whole collection", {
    g <- spin(giotto(), 30, space = "one")
    g <- spatShift(g, dx = 5, space = "two")
    all_sp <- giottoSpace(g)
    # a handle holds one frame, so a collection is a list of handles
    expect_type(all_sp, "list")
    expect_setequal(names(all_sp), c("one", "two"))
    expect_s4_class(all_sp[["one"]], "giottoSpace")
    expect_identical(all_sp[["one"]]@name, "one")
})

test_that("`+` concatenates view steps and merges space frames", {
    v1 <- .demo_view()[1L]
    v2 <- .demo_view()[2L]
    expect_identical(names(v1 + v2), c("filter", "crop"))

    # merging two whole recipes keeps every scope, so each sample still
    # replays only what was recorded for it
    merged <- .demo_space() + .demo_space()
    expect_identical(names(merged), c("a", "b"))
    expect_length(merged, 4L)
    expect_length(merged[["a"]], 2L)
    expect_length(merged[["b"]], 2L)
})

test_that("narrowing rescopes, so split-then-merge reconstructs", {
    # `[i]` narrows without erasing: a surviving step is rescoped to `i`
    # rather than stripped of its scope. Erasing instead -- which is what
    # this did before -- made every step apply to everyone, so merging two
    # narrowings handed each sample the other's transforms.
    sp <- .demo_space()
    merged <- sp["a"] + sp["b"]
    expect_identical(names(merged), c("a", "b"))
    expect_length(merged[["a"]], 1L)
    expect_identical(merged[["a"]][[1L]]$op, "spin")
    expect_length(merged[["b"]], 1L)
    expect_identical(merged[["b"]][[1L]]$op, "spatShift")

    # a narrowed handle still reads back through the two-call seam, which
    # is what the erasure was there to serve
    expect_identical(sp["b"][[NA_character_]][[1L]]$op, "spatShift")

    # merging the same scope twice concatenates rather than overwrites
    twice <- sp["a"] + sp["a"]
    expect_length(twice[["a"]], 2L)
})

test_that("a crop step records the frame its region was read in", {
    g <- spatShift(giotto(), dx = 1, space = "atlas")
    g <- crop(g, c(0, 10, 0, 10), view = "v", space = "atlas")
    g <- crop(g, c(0, 5, 0, 5), view = "v")
    expect_identical(giottoView(g, "v")[1L, "space"], "atlas")
    expect_true(is.na(giottoView(g, "v")[2L, "space"]))
})

test_that("`+` composes views naming different frames", {
    # the frame is per step, so concatenation cannot reinterpret either
    # side and there is nothing to reconcile
    g <- spatShift(giotto(), dx = 1, space = "one")
    g <- spatShift(g, dx = 2, space = "two")
    g <- crop(g, c(0, 10, 0, 10), view = "v", space = "one")
    g <- crop(g, c(0, 10, 0, 10), view = "w", space = "two")
    both <- giottoView(g, "v") + giottoView(g, "w")
    expect_length(both, 2L)
    expect_identical(unlist(both[, "space"]), c("one", "two"))
})

test_that("builder verbs on a recipe record what the gobject route records", {
    # the point of moving the verbs onto the classes is that there is one
    # construction path, so the two surfaces must produce identical objects
    g <- crop(giotto(), c(0, 10, 0, 10), relation = "within", view = "v")
    direct <- crop(new("giottoView"), c(0, 10, 0, 10), relation = "within")
    expect_identical(giottoView(g, "v")@steps, direct@steps)

    g2 <- selectSamples(giotto(), "a", "b", view = "v")
    expect_identical(giottoView(g2, "v")@steps,
        selectSamples(new("giottoView"), "a", "b")@steps)

    g3 <- spin(giotto(), 30, space = "s")
    direct_sp <- spin(perSampleSpace(name = "s"), 30)
    expect_identical(giottoSpace(g3, "s"), direct_sp)
})

test_that("as.list() is the export seam and round-trips losslessly", {
    v <- .demo_view()
    lv <- as.list(v)
    expect_type(lv, "list")
    expect_named(lv, "steps")

    g <- giotto()
    giottoView(g, "from_obj") <- v
    giottoView(g, "from_list") <- lv
    expect_identical(giottoView(g, "from_obj"), giottoView(g, "from_list"))

    sp <- .demo_space()
    lsp <- as.list(sp)
    expect_named(lsp, "atlas")
    expect_named(lsp$atlas, c("kind", "steps"))
    mg <- .fixture_gmulti()
    giottoSpace(mg, "atlas") <- lsp
    expect_identical(giottoSpace(mg, "atlas"), sp)

    # the kind is NAMED in the export rather than inferred: both kinds hold
    # the same slots now, so there is no shape left to infer it from
    ps <- perSampleSpace("flat")
    ps <- spin(ps, 30)
    giottoSpace(mg, "flat") <- as.list(ps)
    expect_s4_class(giottoSpace(mg, "flat"), "perSampleSpace")
    expect_identical(giottoSpace(mg, "flat"), ps)
})

test_that("the recipe classes carry no closure and no external pointer", {
    # Q7's guarantee, restated against the class container: a recipe has to
    # survive saveRDS and reach a worker, and that is a property of the
    # steps, which the class only holds.
    .no_live_refs <- function(x) {
        if (is.function(x)) return(FALSE)
        if (inherits(x, "externalptr") ||
            identical(typeof(x), "externalptr")) return(FALSE)
        if (is.environment(x)) return(FALSE)
        if (is.list(x)) return(all(vapply(x, .no_live_refs, logical(1L))))
        TRUE
    }
    expect_true(.no_live_refs(.demo_view()@steps))
    expect_true(.no_live_refs(.demo_space()@steps))
})

test_that("validObject rejects a hand-poked recipe", {
    v <- .demo_view()
    v@steps[[1L]]$predicate <- "cluster =="
    expect_error(validObject(v), "does not parse")

    sp <- .demo_space()
    sp@steps[[1L]]$op <- "teleport"
    expect_error(validObject(sp), "unknown transform")
})

test_that("show() prints without error for both recipes", {
    expect_output(show(.demo_view()), "giottoView")
    expect_output(show(.demo_space()), "combinedSpace")
    expect_output(show(spin(perSampleSpace("s"), 30)), "perSampleSpace")
    # a member step has no `op`, so the summary line must not read one
    expect_output(show(combinedSpace(c("a", "b"), name = "atlas")),
        "member\\{a,b\\}")
})


test_that("crop steps in different frames resolve in their own frames", {
    # The frame is per step, so one view can crop in an alternate frame and
    # then in the native one. Under a view-level field this was an error.
    g <- .fixture_giotto()
    sl <- getSpatialLocations(g, output = "data.table")
    box <- c(mean(range(sl$sdimx)) - 1500, mean(range(sl$sdimx)) + 1500,
             mean(range(sl$sdimy)) - 1500, mean(range(sl$sdimy)) + 1500)
    dx <- 3000
    g <- spatShift(g, dx = dx, space = "shifted")

    # same box, read once in the shifted frame and once natively
    g <- crop(g, box + c(dx, dx, 0, 0), view = "mixed", space = "shifted")
    g <- crop(g, box, view = "mixed")
    v <- giottoView(g, "mixed")
    expect_identical(unlist(v[, "space"]), c("shifted", NA_character_))

    # each step is evaluated in the frame it names, so the two select the
    # same cells and the intersection equals either one alone.
    #
    # `slots =` keeps this on the cell axis: `.apply_crops_geometrically()`
    # clips points and images with the recorded region and no reference to
    # any frame, so a non-native-frame crop reaching them mismatches. That
    # gap predates the frame moving onto the step -- it fires for any view
    # bound to a non-identity space -- and closing it needs the
    # region-reprojection machinery GiottoClass does not have yet.
    g <- crop(g, box, view = "native_only")
    keyed <- c("cell_metadata", "spatial_locs", "expression")
    expect_setequal(
        pDataDT(materialize(g, "mixed", slots = keyed))$cell_ID,
        pDataDT(materialize(g, "native_only", slots = keyed))$cell_ID)
})

# `space =` on giottoMulti getters ####

test_that("space= on a gmulti getter resolves against the multi, not children", {
    mg <- .fixture_gmulti()
    mg <- spatShift(mg, dx = 100, space = "atlas", samples = "b")

    # the space is registered on the parent; children's @spaces are empty.
    # Forwarding the NAME made each child resolve it against its own slot
    # and fail with "'atlas' is not a registered space".
    native <- getSpatialLocations(mg)
    out <- getSpatialLocations(mg, space = "atlas")
    expect_identical(out$b[]$sdimx, native$b[]$sdimx + 100)
    # the step was scoped to `b`, so `a` is untouched
    expect_identical(out$a[]$sdimx, native$a[]$sdimx)
})

test_that("a gmulti getter takes only a space the object owns", {
    mg <- .fixture_gmulti()
    mg <- spatShift(mg, dx = 100, space = "atlas", samples = "b")

    # A handle is how the parent talks to its children, not how a caller
    # talks to the parent. Reading through a detached frame would produce
    # content whose frame name resolves against nothing on this object.
    expect_error(getSpatialLocations(mg, space = giottoSpace(mg, "atlas")),
        "must be the name of a space registered on this object")
})

# a view is evaluated at the gmulti level only ####

test_that("a view resolves once at the parent, in global IDs", {
    mg <- .fixture_gmulti()
    mg <- subset(mg, leiden_clus == 1, view = "v")

    co <- GiottoClass:::.default_view_coordinator(mg)
    keep <- GiottoClass:::.surviving_cell_ids(mg, giottoView(mg, "v"), co)
    # filters read joint metadata and crops read fused coordinates, both
    # keyed by sample::id -- so one evaluation answers for every child
    expect_true(all(grepl("::", keep)))
    expect_gt(length(unique(sub("::.*", "", keep))), 1L)
})

test_that("gmulti getters honour a view, narrowing children by that set", {
    mg <- .fixture_gmulti()
    mg <- subset(mg, leiden_clus == 1, view = "v")
    co <- GiottoClass:::.default_view_coordinator(mg)
    keep <- GiottoClass:::.surviving_cell_ids(mg, giottoView(mg, "v"), co)

    # forwarding the NAME made each child resolve it against its own empty
    # @view and fail with "no view named 'v'"
    out <- getSpatialLocations(mg, view = "v")
    expect_identical(
        sum(vapply(out, function(x) nrow(x[]), integer(1L))),
        length(keep))
    # the parent speaks globals; children come back local
    expect_false(any(grepl("::", out$a[]$cell_ID)))
    # and it narrows: unfiltered is strictly larger
    expect_gt(nrow(getSpatialLocations(mg)$a[]), nrow(out$a[]))

    # cell-keyed polygons take the same set
    polys <- getPolygonInfo(mg, view = "v")
    expect_identical(sum(vapply(polys, nrow, numeric(1L))),
        as.numeric(length(keep)))
})

test_that("a loose giotto still resolves its own view", {
    # parent-only evaluation is a giottoMulti rule; a standalone object is
    # its own scope and is unaffected
    g <- .fixture_giotto()
    g <- subset(g, leiden_clus == 1, view = "gv")
    expect_gt(nrow(getSpatialLocations(g)[]),
        nrow(getSpatialLocations(g, view = "gv")[]))
})

test_that("a gmulti getter takes only a view the object owns", {
    mg <- .fixture_gmulti()
    mg <- subset(mg, leiden_clus == 1, view = "v")
    # same ownership rule as spaces: handles are the internal channel
    expect_error(getSpatialLocations(mg, view = giottoView(mg, "v")),
        "must be the name of a view registered on this object")
})

# combinedSpace membership is closed ####

test_that("a broadcast step on a combinedSpace expands to its members", {
    cs <- combinedSpace(c("a", "b"), name = "atlas")
    cs <- spin(cs, 30)

    # the member names are written INTO the step, so the recipe states its
    # own scope rather than leaving it to be inferred from record order
    expect_identical(cs[[length(cs)]]$samples, c("a", "b"))
    expect_identical(
        GiottoClass:::.as_giotto_space(as.list(cs), name = "atlas")[["a"]],
        cs[["a"]])
})

test_that("a broadcast step does not reach a non-member", {
    # Left unscoped, `[[` handed the step to any name at all, so a sample
    # outside the layout was transformed as if it were in it -- against the
    # documented closed membership of a combinedSpace.
    cs <- combinedSpace(c("a", "b"), name = "atlas")
    cs <- spin(cs, 30)
    cs <- spatShift(cs, dx = 5, samples = "a")

    expect_length(cs[["a"]], 2L)
    expect_length(cs[["b"]], 1L)
    expect_length(cs[["c"]], 0L)
})

test_that("a perSampleSpace broadcast still reaches an unnamed sample", {
    # open membership by design: this is the difference between the kinds,
    # not an oversight shared with combinedSpace
    ps <- perSampleSpace(name = "upright")
    ps <- spin(ps, 30)
    expect_null(ps[[1L]]$samples)
    expect_length(ps[["anyone"]], 1L)
})

test_that("an unscoped step on a memberless combinedSpace is refused", {
    # it would be recorded reaching nobody -- a dead step that looks live
    expect_error(spin(combinedSpace(name = "atlas"), 30),
        "cannot take an unscoped step")
    # both remedies work
    expect_s4_class(spin(combinedSpace(name = "atlas"), 30, samples = "a"),
        "combinedSpace")
    expect_s4_class(spin(combinedSpace(c("a", "b"), name = "atlas"), 30),
        "combinedSpace")
})


# --- combine* forward view/space ------------------------------------------
# These functions are the data-fetch layer GiottoVisuals plots through, so a
# view or space they swallow is a plot silently drawn on the wrong cells or
# in the wrong frame -- no error to notice.

test_that("space applies with no view supplied", {
    # `view` and `space` are independent knobs; materialize dispatched only
    # on view = "character", so naming a frame alone used to fail on
    # dispatch rather than return the frame.
    g <- .fixture_giotto()
    giottoSpace(g, "scaled") <- spin(perSampleSpace("scaled"), 30)

    plain <- suppressWarnings(combineMetadata(g, verbose = FALSE))
    framed <- suppressWarnings(
        combineMetadata(g, space = "scaled", verbose = FALSE))

    expect_identical(nrow(framed), nrow(plain))
    expect_false(isTRUE(all.equal(framed$sdimx, plain$sdimx)))
})

test_that("combineCellData(space=) reaches every child of a gmulti", {
    # the frame is registered on the PARENT only -- a child cannot resolve
    # the name, so this passes only if the parent resolved it first
    mg <- .fixture_gmulti()
    giottoSpace(mg, "scaled") <- spin(perSampleSpace("scaled"), 30)

    plain <- suppressWarnings(combineCellData(mg))
    framed <- suppressWarnings(combineCellData(mg, space = "scaled"))

    expect_named(framed, c("a", "b"))
    for (nm in c("a", "b")) {
        x <- framed[[nm]][[1L]]
        y <- plain[[nm]][[1L]]
        expect_identical(nrow(x), nrow(y))
        # centroids and polygon vertices both moved
        expect_false(isTRUE(all.equal(x$sdimx, y$sdimx)))
        expect_false(isTRUE(all.equal(x$x, y$x)))
    }
})

test_that("combineCellData(view=) narrows every child at the parent", {
    mg <- .fixture_gmulti()
    plain <- suppressWarnings(combineCellData(mg))
    mg <- subset(mg, leiden_clus == 1, view = "sub")
    viewed <- suppressWarnings(combineCellData(mg, view = "sub"))

    for (nm in c("a", "b")) {
        n_view <- length(unique(viewed[[nm]][[1L]]$cell_ID))
        n_all <- length(unique(plain[[nm]][[1L]]$cell_ID))
        expect_lt(n_view, n_all)
        expect_gt(n_view, 0L)
    }
    # both children resolve against the same joint metadata, so they narrow
    # to the same count -- a per-child resolution could not guarantee this
    expect_identical(
        length(unique(viewed$a[[1L]]$cell_ID)),
        length(unique(viewed$b[[1L]]$cell_ID))
    )
})

test_that("combineMetadata(space=) reaches every child of a gmulti", {
    mg <- .fixture_gmulti()
    giottoSpace(mg, "scaled") <- spin(perSampleSpace("scaled"), 30)

    plain <- suppressWarnings(combineMetadata(mg, verbose = FALSE))
    framed <- suppressWarnings(
        combineMetadata(mg, space = "scaled", verbose = FALSE))

    expect_named(framed, c("a", "b"))
    for (nm in c("a", "b")) {
        expect_identical(nrow(framed[[nm]]), nrow(plain[[nm]]))
        expect_false(isTRUE(
            all.equal(framed[[nm]]$sdimx, plain[[nm]]$sdimx)))
    }
})


# --- combineFeatureOverlapData reads any points carrier --------------------

test_that("combineFeatureOverlapData returns the as.data.table point shape", {
    # This site used to call .spatvector_to_dt() directly, which is only the
    # in-memory half of as.data.table() -- so it was the one place in the
    # combine family that a backed points store could not be read through.
    # Pinning the column set here because the swap changes it: terra's geom
    # bookkeeping (geom / part / hole) is gone, and what remains is what the
    # sibling `combineFeatureData()` already returned.
    g <- GiottoData::loadGiottoMini("vizgen", verbose = FALSE)
    g <- updateGiottoObject(g)
    sel <- list(rna = c("Mlc1", "Gfap"))

    ov <- suppressWarnings(
        combineFeatureOverlapData(g, poly_info = "aggregate", sel_feats = sel))
    fd <- suppressWarnings(
        combineFeatureData(g, spat_unit = "aggregate", sel_feats = sel))

    expect_true(all(c("feat_ID", "feat_ID_uniq", "x", "y") %in% names(ov$rna)))
    # the terra-only geometry bookkeeping is no longer emitted
    expect_false(any(c("geom", "part", "hole") %in% names(ov$rna)))
    # and the two branches spatInSituPlotPoints() chooses between now agree
    # on the point columns they share
    shared <- intersect(names(ov$rna), names(fd$rna))
    expect_true(all(c("feat_ID", "feat_ID_uniq", "x", "y") %in% shared))

    # the overlap filter still narrows to overlapped points only
    expect_gt(nrow(ov$rna), 0L)
    expect_true(all(ov$rna$feat_ID %in% sel$rna))
})
