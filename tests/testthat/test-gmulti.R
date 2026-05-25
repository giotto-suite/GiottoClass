# Tests for giottoMulti — sketch-level coverage only.
# Verifies the basic class machinery: construction, introspection, id_map,
# activeObjects accessor, and spatIDs/featIDs dispatch on global IDs.

.mk_minimal <- function(ncell, nfeat) {
    m <- matrix(0, nrow = nfeat, ncol = ncell)
    rownames(m) <- paste0("f", seq_len(nfeat))
    colnames(m) <- paste0("c", seq_len(ncell))
    createGiottoObject(expression = m, verbose = FALSE)
}

test_that("empty giottoMulti constructs and shows", {
    mg <- new("giottoMulti")
    expect_s4_class(mg, "giottoMulti")
    expect_true(is(mg, "gAny"))
    expect_length(mg, 0L)
    expect_null(idMap(mg, "cells"))
    expect_output(show(mg), "giottoMulti")
})

test_that("populated giottoMulti exposes children", {
    g1 <- .mk_minimal(5, 4)
    g2 <- .mk_minimal(3, 4)
    mg <- createGiottoMulti(list(a = g1, b = g2))

    expect_s4_class(mg, "giottoMulti")
    expect_true(is(mg, "gAny"))
    expect_identical(names(mg), c("a", "b"))
    expect_length(mg, 2L)
    expect_s4_class(mg[["a"]], "giotto")
    expect_identical(mg[[1]], mg[["a"]])
})

test_that("id_map namespaces cells globally and leaves feats passthrough", {
    g1 <- .mk_minimal(5, 4)
    g2 <- .mk_minimal(3, 4)
    mg <- createGiottoMulti(list(a = g1, b = g2))

    cells <- idMap(mg, "cells")
    expect_identical(nrow(cells), 8L)
    expect_identical(unique(cells$object), c("a", "b"))
    expect_true(all(grepl("^[ab]::c[0-9]+$", cells$global_id)))

    feats <- idMap(mg, "feats")
    # 4 features per object x 2 objects = 8 rows in long form
    expect_identical(nrow(feats), 8L)
    # but feature names are shared (passthrough), so global = local
    expect_identical(feats$global_id, feats$local_id)
})

test_that("spatIDs returns global by default, local on request, filterable", {
    g1 <- .mk_minimal(5, 4)
    g2 <- .mk_minimal(3, 4)
    mg <- createGiottoMulti(list(a = g1, b = g2))

    expect_identical(
        spatIDs(mg),
        c("a::c1", "a::c2", "a::c3", "a::c4", "a::c5",
          "b::c1", "b::c2", "b::c3")
    )
    expect_identical(spatIDs(mg, local = TRUE),
        c("c1", "c2", "c3", "c4", "c5", "c1", "c2", "c3"))
    expect_identical(spatIDs(mg, object = "a"),
        c("a::c1", "a::c2", "a::c3", "a::c4", "a::c5"))
    expect_identical(spatIDs(mg, object = "a", local = TRUE),
        c("c1", "c2", "c3", "c4", "c5"))
})

test_that("featIDs returns uniques by default", {
    g1 <- .mk_minimal(5, 4)
    g2 <- .mk_minimal(3, 4)
    mg <- createGiottoMulti(list(a = g1, b = g2))

    expect_identical(featIDs(mg), c("f1", "f2", "f3", "f4"))
    expect_identical(length(featIDs(mg, uniques = FALSE)), 8L)
})

test_that("activeObjects get/set works and validates", {
    g1 <- .mk_minimal(5, 4)
    g2 <- .mk_minimal(3, 4)
    mg <- createGiottoMulti(list(a = g1, b = g2))

    # default: all
    expect_identical(activeObjects(mg), c("a", "b"))

    activeObjects(mg) <- "a"
    expect_identical(activeObjects(mg), "a")

    activeObjects(mg) <- NULL
    expect_identical(activeObjects(mg), c("a", "b"))

    expect_error(activeObjects(mg) <- "nope", "unknown object")
})

test_that("[[<- replaces a child", {
    g1 <- .mk_minimal(5, 4)
    g2 <- .mk_minimal(3, 4)
    g3 <- .mk_minimal(2, 4)
    mg <- createGiottoMulti(list(a = g1, b = g2))

    mg[["b"]] <- g3
    expect_identical(mg[["b"]], g3)
    # NOTE: id_map is not eagerly refreshed; documented behavior
})

test_that("gAny inheritance does not change giotto dispatch", {
    g <- .mk_minimal(5, 4)
    expect_true(is(g, "giotto"))
    expect_true(is(g, "gAny"))
    # spatIDs("giotto", ...) still wins
    expect_identical(spatIDs(g), c("c1", "c2", "c3", "c4", "c5"))
})


# Shared-domain accessors promoted to S4 (gAny) ####

test_that("getExpression works identically on giotto via gAny method", {
    g <- .mk_minimal(5, 4)
    e <- getExpression(g)
    expect_s4_class(e, "exprObj")
    expect_identical(dim(e[]), c(4L, 5L))

    em <- getExpression(g, output = "matrix")
    expect_true(inherits(em, c("matrix", "Matrix")))
})

test_that("getExpression on giottoMulti reads from parent's shared slot", {
    g1 <- .mk_minimal(5, 4)
    mg <- createGiottoMulti(list(a = g1))

    # parent slot empty: getExpression falls back to assembling from children
    # (naive concat with sample::id prefix; see .gm_assemble_expression)
    e_derived <- getExpression(mg)
    expect_s4_class(e_derived, "exprObj")
    expect_identical(dim(e_derived[]), c(4L, 5L))
    expect_true(all(colnames(e_derived[]) == paste("a", paste0("c", 1:5), sep = "::")))

    # populate parent's shared slot. Joint slots are keyed on GLOBAL IDs
    # (sample::id), matching @id_map$cells$global_id; the view filter
    # expects this contract. setExpression(mg, joint) is the override path
    # for integration output.
    e1 <- g1@expression$cell$rna$raw
    mat <- e1[]
    colnames(mat) <- paste("a", colnames(mat), sep = "::")
    e1[] <- mat
    mg@expression <- list(cell = list(rna = list(raw = e1)))

    e <- getExpression(mg)
    expect_s4_class(e, "exprObj")
    expect_identical(dim(e[]), c(4L, 5L))
})

test_that("getCellMetadata works identically on giotto via gAny method", {
    g <- .mk_minimal(5, 4)
    cm <- getCellMetadata(g)
    expect_s4_class(cm, "cellMetaObj")
    expect_identical(nrow(cm[]), 5L)
})

test_that("getCellMetadata on giottoMulti reads from parent's shared slot", {
    g1 <- .mk_minimal(5, 4)
    mg <- createGiottoMulti(list(a = g1))

    # populate shared slots with globally-keyed content (sample::id), matching
    # the @id_map view filter contract.
    e1 <- g1@expression$cell$rna$raw
    mat <- e1[]
    colnames(mat) <- paste("a", colnames(mat), sep = "::")
    e1[] <- mat
    mg@expression <- list(cell = list(rna = list(raw = e1)))

    cm1 <- g1@cell_metadata$cell$rna
    dt <- cm1[]
    dt$cell_ID <- paste("a", dt$cell_ID, sep = "::")
    cm1[] <- dt
    mg@cell_metadata <- list(cell = list(rna = cm1))

    cm <- getCellMetadata(mg)
    expect_s4_class(cm, "cellMetaObj")
    expect_identical(nrow(cm[]), 5L)
})


# Spatial-domain accessors — per-child dispatch on giottoMulti ####

test_that("getSpatialLocations on giottoMulti returns named per-child list", {
    g1 <- .mk_minimal(5, 4)
    g2 <- .mk_minimal(3, 4)
    mg <- createGiottoMulti(list(a = g1, b = g2))

    out <- getSpatialLocations(mg)
    expect_type(out, "list")
    expect_identical(names(out), c("a", "b"))
    expect_s4_class(out$a, "spatLocsObj")
    expect_s4_class(out$b, "spatLocsObj")
    expect_identical(nrow(out$a[]), 5L)
    expect_identical(nrow(out$b[]), 3L)
})

test_that("getSpatialLocations honors object= to subset children", {
    g1 <- .mk_minimal(5, 4)
    g2 <- .mk_minimal(3, 4)
    mg <- createGiottoMulti(list(a = g1, b = g2))

    out <- getSpatialLocations(mg, object = "b")
    expect_identical(names(out), "b")
    expect_identical(nrow(out$b[]), 3L)
})

test_that("setSpatialLocations on giottoMulti requires object= and routes", {
    g1 <- .mk_minimal(5, 4)
    g2 <- .mk_minimal(3, 4)
    mg <- createGiottoMulti(list(a = g1, b = g2))
    sl_b <- getSpatialLocations(g2)

    # missing object should error
    expect_error(setSpatialLocations(mg, x = sl_b),
        "must name the child")

    # length > 1 should error
    expect_error(
        setSpatialLocations(mg, x = sl_b, object = c("a", "b")),
        "length 1"
    )

    # round-trip: writing back the child's own spatlocs returns a giottoMulti
    sl_a <- getSpatialLocations(g1)
    mg2 <- setSpatialLocations(mg, x = sl_a, object = "a", verbose = FALSE)
    expect_s4_class(mg2, "giottoMulti")
    out_a <- getSpatialLocations(mg2, object = "a")$a
    expect_identical(nrow(out_a[]), 5L)
})


# setGiotto dispatch on giottoMulti ####

test_that("setGiotto on giottoMulti routes shared subobject to parent", {
    g1 <- .mk_minimal(5, 4)
    mg <- createGiottoMulti(list(a = g1))
    # parent's shared expression slot starts empty
    expect_null(mg@expression)

    # joint expression is keyed on global IDs (sample::id) to match the
    # @id_map view filter contract.
    e <- getExpression(g1)
    mat <- e[]
    colnames(mat) <- paste("a", colnames(mat), sep = "::")
    e[] <- mat

    mg2 <- setGiotto(mg, e, verbose = FALSE)
    expect_s4_class(mg2, "giottoMulti")
    # shared slot now populated, child untouched
    expect_false(is.null(mg2@expression))
    e2 <- getExpression(mg2)
    expect_s4_class(e2, "exprObj")
    expect_identical(dim(e2[]), c(4L, 5L))
})

test_that("set_default_spat_unit/feat_type fall back to @access on giottoMulti", {
    g1 <- .mk_minimal(5, 4)
    mg <- createGiottoMulti(list(a = g1))
    # parent slots are empty; defaults should come from the @access cache
    # populated by the constructor (per-child defaults).
    expect_null(mg@expression)

    su <- set_default_spat_unit(mg)
    ft <- set_default_feat_type(mg, spat_unit = su)
    expect_identical(su, mg@access$spat_unit[[1L]])
    expect_identical(ft, mg@access$feat_type[[1L]])
})

test_that("setGiotto on giottoMulti routes spatial subobject per-child", {
    g1 <- .mk_minimal(5, 4)
    g2 <- .mk_minimal(3, 4)
    mg <- createGiottoMulti(list(a = g1, b = g2))

    sl_a <- getSpatialLocations(g1)
    # without object= the underlying setSpatialLocations errors
    expect_error(setGiotto(mg, sl_a, verbose = FALSE), "must name the child")

    mg2 <- setGiotto(mg, sl_a, object = "a", verbose = FALSE)
    expect_s4_class(mg2, "giottoMulti")
    expect_identical(nrow(getSpatialLocations(mg2, object = "a")$a[]), 5L)
})


# id_map caching: fast-path initialize, rebuildMaps escape hatch ####

test_that("constructor populates id_sig alongside id_map", {
    g1 <- .mk_minimal(5, 4)
    g2 <- .mk_minimal(3, 4)
    mg <- createGiottoMulti(list(a = g1, b = g2))

    # id_sig is a per-child list of cell/feat lengths
    expect_identical(names(mg@id_sig), c("a", "b"))
    expect_identical(mg@id_sig$a$cell, lengths(g1@cell_ID))
    expect_identical(mg@id_sig$a$feat, lengths(g1@feat_ID))
    expect_identical(mg@id_sig$b$cell, lengths(g2@cell_ID))
})

test_that("initialize fast-path: id_map unchanged when children unchanged", {
    g1 <- .mk_minimal(5, 4)
    mg <- createGiottoMulti(list(a = g1))
    before <- mg@id_map

    # mutate id_map to detect whether a rebuild fired
    mg@id_map$cells <- before$cells[1, ]
    mg2 <- initialize(mg)
    # signatures match (children unchanged), so the narrowed id_map is kept
    expect_identical(nrow(mg2@id_map$cells), 1L)
})

test_that("initialize rebuilds id_map when child length signature changes", {
    g1 <- .mk_minimal(5, 4)
    mg <- createGiottoMulti(list(a = g1))
    expect_identical(nrow(mg@id_map$cells), 5L)

    # swap in a child with a different cell count — simulates direct mutation
    g1_smaller <- .mk_minimal(2, 4)
    mg@objects$a <- g1_smaller

    mg2 <- initialize(mg)
    # signature changed → full rebuild
    expect_identical(nrow(mg2@id_map$cells), 2L)
    expect_identical(mg2@id_sig$a$cell, lengths(g1_smaller@cell_ID))
})

test_that("rebuildMaps forces a rebuild even when signatures match", {
    g1 <- .mk_minimal(5, 4)
    mg <- createGiottoMulti(list(a = g1))

    # narrow id_map manually
    mg@id_map$cells <- mg@id_map$cells[1:2, ]
    # signatures still match children's actual lengths, so initialize fast-paths
    expect_identical(nrow(initialize(mg)@id_map$cells), 2L)
    # but rebuildMaps clears @id_sig first → full rebuild
    expect_identical(nrow(rebuildMaps(mg)@id_map$cells), 5L)
})


# subset narrows id_map non-destructively ####

test_that("subset(mg, cells = ...) narrows id_map without touching children", {
    g1 <- .mk_minimal(5, 4)
    g2 <- .mk_minimal(3, 4)
    mg <- createGiottoMulti(list(a = g1, b = g2))

    keep <- c("a::c1", "a::c2", "b::c1")
    mg2 <- subset(mg, cells = keep)

    expect_identical(mg2@id_map$cells$global_id, keep)
    # children intact
    expect_identical(length(spatIDs(mg2@objects$a)), 5L)
    expect_identical(length(spatIDs(mg2@objects$b)), 3L)
    # spatIDs on the multi reflects the narrowed view
    expect_identical(spatIDs(mg2), keep)
})

test_that("subset(mg, features = ...) narrows feat id_map", {
    g1 <- .mk_minimal(5, 4)
    mg <- createGiottoMulti(list(a = g1))

    keep <- c("f1", "f3")
    mg2 <- subset(mg, features = keep)
    expect_identical(sort(unique(mg2@id_map$feats$global_id)), sort(keep))
})

test_that("subset warns on missing globals", {
    g1 <- .mk_minimal(5, 4)
    mg <- createGiottoMulti(list(a = g1))
    expect_warning(
        subset(mg, cells = c("a::c1", "a::nope")),
        "not in id_map"
    )
})

test_that("rebuildMaps restores full view after a subset", {
    g1 <- .mk_minimal(5, 4)
    g2 <- .mk_minimal(3, 4)
    mg <- createGiottoMulti(list(a = g1, b = g2))

    mg2 <- subset(mg, cells = c("a::c1"))
    expect_identical(nrow(mg2@id_map$cells), 1L)

    mg3 <- rebuildMaps(mg2)
    expect_identical(nrow(mg3@id_map$cells), 8L)  # 5 + 3
})


# View filter: getters reflect @id_map without trimming the joint slots ####

test_that("getExpression applies id_map view filter on giottoMulti", {
    g1 <- .mk_minimal(5, 4)
    mg <- createGiottoMulti(list(a = g1))

    # populate joint @expression with globally-keyed colnames
    e <- getExpression(g1)
    mat <- e[]
    colnames(mat) <- paste("a", colnames(mat), sep = "::")
    e[] <- mat
    mg@expression <- list(cell = list(rna = list(raw = e)))

    # full view: all 5 cells
    expect_identical(ncol(getExpression(mg)[]), 5L)

    # subset narrows the view; joint slot stays full
    mg2 <- subset(mg, cells = c("a::c1", "a::c3"))
    expect_identical(ncol(getExpression(mg2)[]), 2L)
    # joint slot itself is untouched
    expect_identical(ncol(mg2@expression$cell$rna$raw[]), 5L)

    # rebuildMaps restores the full view without re-supplying expression
    mg3 <- rebuildMaps(mg2)
    expect_identical(ncol(getExpression(mg3)[]), 5L)
})

test_that("getCellMetadata applies id_map view filter on giottoMulti", {
    g1 <- .mk_minimal(5, 4)
    mg <- createGiottoMulti(list(a = g1))

    cm1 <- g1@cell_metadata$cell$rna
    dt <- cm1[]
    dt$cell_ID <- paste("a", dt$cell_ID, sep = "::")
    cm1[] <- dt
    mg@cell_metadata <- list(cell = list(rna = cm1))

    expect_identical(nrow(getCellMetadata(mg)[]), 5L)
    mg2 <- subset(mg, cells = c("a::c2"))
    expect_identical(nrow(getCellMetadata(mg2)[]), 1L)
    # joint slot untouched
    expect_identical(nrow(mg2@cell_metadata$cell$rna[]), 5L)
})


# Per-child view filter (default-on, unfiltered escape hatch) ####

test_that("getSpatialLocations per-child filters by id_map's local IDs", {
    g1 <- .mk_minimal(5, 4)
    g2 <- .mk_minimal(3, 4)
    mg <- createGiottoMulti(list(a = g1, b = g2))

    # subset narrows globals for child "a" to c1, c3 (locals c1, c3 of a)
    mg2 <- subset(mg, cells = c("a::c1", "a::c3", "b::c1"))

    sl_a <- getSpatialLocations(mg2, object = "a")$a
    expect_s4_class(sl_a, "spatLocsObj")
    expect_identical(sort(sl_a[]$cell_ID), c("c1", "c3"))

    sl_b <- getSpatialLocations(mg2, object = "b")$b
    expect_identical(sl_b[]$cell_ID, "c1")

    # underlying child slot untouched
    expect_identical(length(spatIDs(mg2@objects$a)), 5L)
})

test_that("unfiltered = TRUE returns the child's full content", {
    g1 <- .mk_minimal(5, 4)
    mg <- createGiottoMulti(list(a = g1))
    mg2 <- subset(mg, cells = c("a::c1"))

    # default: narrowed
    sl_filtered <- getSpatialLocations(mg2, object = "a")$a
    expect_identical(nrow(sl_filtered[]), 1L)

    # escape hatch: full child
    sl_full <- getSpatialLocations(mg2, object = "a", unfiltered = TRUE)$a
    expect_identical(nrow(sl_full[]), 5L)
})


# compact: materialize the view, trim joint slots ####

test_that("compact trims joint @expression to the current view", {
    g1 <- .mk_minimal(5, 4)
    mg <- createGiottoMulti(list(a = g1))

    # populate joint @expression with globally-keyed colnames
    e <- getExpression(g1)
    mat <- e[]
    colnames(mat) <- paste("a", colnames(mat), sep = "::")
    e[] <- mat
    mg@expression <- list(cell = list(rna = list(raw = e)))

    mg2 <- subset(mg, cells = c("a::c1", "a::c3"))
    # before compact: joint slot stores full 5 cols, view filter exposes 2
    expect_identical(ncol(mg2@expression$cell$rna$raw[]), 5L)
    expect_identical(ncol(getExpression(mg2)[]), 2L)

    mg3 <- compact(mg2)
    # after compact: joint slot itself is now 2 cols
    expect_identical(ncol(mg3@expression$cell$rna$raw[]), 2L)
    # view filter is a no-op (already trimmed)
    expect_identical(ncol(getExpression(mg3)[]), 2L)
})

test_that("compact trims joint @cell_metadata to the current view", {
    g1 <- .mk_minimal(5, 4)
    mg <- createGiottoMulti(list(a = g1))

    cm1 <- g1@cell_metadata$cell$rna
    dt <- cm1[]
    dt$cell_ID <- paste("a", dt$cell_ID, sep = "::")
    cm1[] <- dt
    mg@cell_metadata <- list(cell = list(rna = cm1))

    mg2 <- subset(mg, cells = c("a::c2"))
    expect_identical(nrow(mg2@cell_metadata$cell$rna[]), 5L)

    mg3 <- compact(mg2)
    expect_identical(nrow(mg3@cell_metadata$cell$rna[]), 1L)
})

test_that("getExpression assembles joint matrix from children when empty", {
    g1 <- .mk_minimal(5, 4)
    g2 <- .mk_minimal(3, 4)
    mg <- createGiottoMulti(list(a = g1, b = g2))

    e <- getExpression(mg)
    expect_s4_class(e, "exprObj")
    # 8 cells = 5 + 3, all globally namespaced; 4 features (intersection)
    expect_identical(dim(e[]), c(4L, 8L))
    expect_identical(colnames(e[]),
        c(paste("a", paste0("c", 1:5), sep = "::"),
          paste("b", paste0("c", 1:3), sep = "::")))
})

test_that("assembled joint expression respects @id_map view filter", {
    g1 <- .mk_minimal(5, 4)
    g2 <- .mk_minimal(3, 4)
    mg <- createGiottoMulti(list(a = g1, b = g2))
    mg2 <- subset(mg, cells = c("a::c1", "b::c2"))

    e <- getExpression(mg2)
    expect_identical(ncol(e[]), 2L)
    expect_identical(colnames(e[]), c("a::c1", "b::c2"))
})

test_that("assembly resolves per-child defaults when nesting args are NULL", {
    # Both children have a `cell` spat_unit; we add an extra spat_unit to
    # `a` and make it the active one so a's default (`extra`) differs from
    # b's default (`cell`). The joint assembly should pull a's `extra` and
    # b's `cell` independently — the global namespace (sample::id)
    # disambiguates either way.
    g1 <- .mk_minimal(5, 4)
    g2 <- .mk_minimal(3, 4)

    # add an "extra" spat_unit to g1 with the same cell IDs
    e_extra <- g1@expression$cell$rna$raw
    spatUnit(e_extra) <- "extra"
    g1@expression$extra <- list(rna = list(raw = e_extra))
    g1@cell_metadata$extra <- g1@cell_metadata$cell
    g1@feat_metadata$extra <- g1@feat_metadata$cell
    g1@cell_ID$extra <- g1@cell_ID$cell
    g1 <- initialize(g1)
    activeSpatUnit(g1) <- "extra"

    mg <- createGiottoMulti(list(a = g1, b = g2))

    # a's default spat_unit is "extra"; b's default is "cell". Per-child
    # resolution should let both contribute.
    e <- getExpression(mg)
    expect_identical(ncol(e[]), 8L)
    expect_true(all(c("a::c1", "b::c1") %in% colnames(e[])))
})

test_that("assembly intersects features across children", {
    g1 <- .mk_minimal(5, 4)
    # Trim g2's feature panel to 3 features (intersect with g1's 4 → 3 features)
    m2 <- matrix(0, nrow = 3, ncol = 3)
    rownames(m2) <- paste0("f", 1:3)
    colnames(m2) <- paste0("c", 1:3)
    g2 <- createGiottoObject(expression = m2, verbose = FALSE)
    mg <- createGiottoMulti(list(a = g1, b = g2))

    e <- getExpression(mg)
    expect_identical(nrow(e[]), 3L)
    expect_identical(sort(rownames(e[])), paste0("f", 1:3))
})

test_that("setExpression on giottoMulti overrides assembly", {
    g1 <- .mk_minimal(5, 4)
    mg <- createGiottoMulti(list(a = g1))

    # custom joint matrix replacing the naive assembly
    custom_e <- getExpression(g1)
    mat <- custom_e[]
    colnames(mat) <- paste("a", colnames(mat), sep = "::")
    # tweak values so we can detect which path returned
    mat[1, 1] <- 999
    custom_e[] <- mat
    mg@expression <- list(cell = list(rna = list(raw = custom_e)))

    e <- getExpression(mg)
    expect_identical(e[][1, 1], 999)
})


test_that("compact leaves children untouched", {
    g1 <- .mk_minimal(5, 4)
    mg <- createGiottoMulti(list(a = g1))

    e <- getExpression(g1)
    mat <- e[]
    colnames(mat) <- paste("a", colnames(mat), sep = "::")
    e[] <- mat
    mg@expression <- list(cell = list(rna = list(raw = e)))

    mg2 <- compact(subset(mg, cells = c("a::c1")))
    # children intact
    expect_identical(length(spatIDs(mg2@objects$a)), 5L)
})


# as(giotto, "giottoMulti") — single-object wrap ####

test_that("as(g, 'giottoMulti') wraps a single giotto with default name", {
    g <- .mk_minimal(5, 4)
    mg <- as(g, "giottoMulti")
    expect_s4_class(mg, "giottoMulti")
    expect_identical(length(mg), 1L)
    expect_identical(names(mg), "sample1")
    # children intact
    expect_identical(length(spatIDs(mg@objects$sample1)), 5L)
})

test_that("wrapped giottoMulti exposes the lazy view layer", {
    g <- .mk_minimal(5, 4)
    mg <- as(g, "giottoMulti")

    # globals are sample1::c{1..5}
    expect_identical(spatIDs(mg),
        paste("sample1", paste0("c", 1:5), sep = "::"))

    # subset narrows non-destructively
    mg2 <- subset(mg, cells = c("sample1::c1", "sample1::c3"))
    expect_identical(spatIDs(mg2), c("sample1::c1", "sample1::c3"))
    expect_identical(length(spatIDs(mg2@objects$sample1)), 5L)

    # rebuildMaps restores
    mg3 <- rebuildMaps(mg2)
    expect_identical(length(spatIDs(mg3)), 5L)
})

test_that("show(mg) surfaces children, view counts, joint slots", {
    g1 <- .mk_minimal(5, 4)
    g2 <- .mk_minimal(3, 4)
    mg <- createGiottoMulti(list(a = g1, b = g2))

    # unfiltered: no "(filtered)" tag
    expect_output(show(mg), "2 child object\\(s\\)")
    expect_output(show(mg), "a: 5 cells, 4 features")
    expect_output(show(mg), "b: 3 cells, 4 features")
    expect_output(show(mg), "view: 8 / 8 cells, 4 / 4 features")
    # no joint slots populated
    expect_failure(expect_output(show(mg), "joint slots:"))

    # after subset: filtered flag appears
    mg2 <- subset(mg, cells = c("a::c1", "b::c1"))
    expect_output(show(mg2), "view: 2 / 8 cells \\(filtered\\)")
})

test_that("show(mg) lists populated joint slots", {
    g1 <- .mk_minimal(5, 4)
    mg <- createGiottoMulti(list(a = g1))

    e <- getExpression(g1)
    mat <- e[]
    colnames(mat) <- paste("a", colnames(mat), sep = "::")
    e[] <- mat
    mg@expression <- list(cell = list(rna = list(raw = e)))

    expect_output(show(mg), "joint slots: expression")
})

test_that("show(mg) adds a 'shared' line when child panels differ", {
    g1 <- .mk_minimal(5, 4)
    # b shares 3 of 4 features with a
    m2 <- matrix(0, nrow = 3, ncol = 3)
    rownames(m2) <- paste0("f", 1:3)
    colnames(m2) <- paste0("c", 1:3)
    g2 <- createGiottoObject(expression = m2, verbose = FALSE)
    mg <- createGiottoMulti(list(a = g1, b = g2))

    out <- capture.output(show(mg))
    expect_true(any(grepl("shared: 3 feature\\(s\\)", out)))

    # matched panels: no shared line
    mg_eq <- createGiottoMulti(list(a = g1, b = g1))
    out_eq <- capture.output(show(mg_eq))
    expect_false(any(grepl("shared:", out_eq)))
})

test_that("mg[i] subsets children, returning a smaller giottoMulti", {
    g1 <- .mk_minimal(5, 4)
    g2 <- .mk_minimal(3, 4)
    g3 <- .mk_minimal(2, 4)
    mg <- createGiottoMulti(list(a = g1, b = g2, c = g3))

    sub_named <- mg[c("a", "c")]
    expect_s4_class(sub_named, "giottoMulti")
    expect_identical(names(sub_named), c("a", "c"))
    expect_identical(spatIDs(sub_named),
        c(paste0("a::c", 1:5), paste0("c::c", 1:2)))

    sub_int <- mg[c(1, 3)]
    expect_identical(names(sub_int), c("a", "c"))

    expect_error(mg["nope"], "unknown child")
})

test_that("names(mg) <- renames children and refreshes id_map / access", {
    g1 <- .mk_minimal(5, 4)
    g2 <- .mk_minimal(3, 4)
    mg <- createGiottoMulti(list(a = g1, b = g2))

    names(mg) <- c("x", "y")
    expect_identical(names(mg), c("x", "y"))
    expect_identical(mg@access$object, c("x", "y"))
    expect_identical(sort(unique(mg@id_map$cells$object)), c("x", "y"))
    expect_identical(spatIDs(mg),
        c(paste0("x::c", 1:5), paste0("y::c", 1:3)))

    expect_error(names(mg) <- c("a", "a"), "unique")
    expect_error(names(mg) <- "a", "length")
})

test_that("names(mg) <- refuses when joint shared slots are populated", {
    g1 <- .mk_minimal(5, 4)
    mg <- createGiottoMulti(list(a = g1))

    # populate joint @expression with globally-keyed colnames
    e <- getExpression(g1)
    mat <- e[]
    colnames(mat) <- paste("a", colnames(mat), sep = "::")
    e[] <- mat
    mg@expression <- list(cell = list(rna = list(raw = e)))

    expect_error(names(mg) <- "renamed", "populated")

    # clearing the joint slot allows the rename
    mg@expression <- NULL
    names(mg) <- "renamed"
    expect_identical(names(mg), "renamed")
})

test_that("pDataDT / fDataDT work on giottoMulti via assembly fallback", {
    g1 <- .mk_minimal(5, 4)
    g2 <- .mk_minimal(3, 4)
    mg <- createGiottoMulti(list(a = g1, b = g2))

    pd <- pDataDT(mg)
    expect_s3_class(pd, "data.table")
    expect_identical(nrow(pd), 8L)
    expect_identical(sort(pd$cell_ID),
        sort(c(paste0("a::c", 1:5), paste0("b::c", 1:3))))

    fd <- fDataDT(mg)
    expect_s3_class(fd, "data.table")
    expect_identical(nrow(fd), 4L)
    expect_identical(sort(fd$feat_ID), paste0("f", 1:4))
})

test_that("pDataDT assembly tags each row with list_ID = child name", {
    g1 <- .mk_minimal(5, 4)
    g2 <- .mk_minimal(3, 4)
    mg <- createGiottoMulti(list(a = g1, b = g2))

    pd <- pDataDT(mg)
    expect_true("list_ID" %in% names(pd))
    # rows tagged consistently with the child they came from
    expect_identical(sum(pd$list_ID == "a"), 5L)
    expect_identical(sum(pd$list_ID == "b"), 3L)
    # list_ID aligns with the cell_ID prefix
    expect_true(all(startsWith(pd$cell_ID[pd$list_ID == "a"], "a::")))
    expect_true(all(startsWith(pd$cell_ID[pd$list_ID == "b"], "b::")))
})

test_that("activeSpatUnit / activeFeatType return per-child vectors", {
    g1 <- .mk_minimal(5, 4)
    g2 <- .mk_minimal(3, 4)
    mg <- createGiottoMulti(list(a = g1, b = g2))

    su <- activeSpatUnit(mg)
    expect_named(su, c("a", "b"))
    expect_true(all(su == "cell"))

    ft <- activeFeatType(mg)
    expect_named(ft, c("a", "b"))
    expect_true(all(ft == "rna"))
})


test_that("show(mg) reports active scope only when narrower than all", {
    g1 <- .mk_minimal(5, 4)
    g2 <- .mk_minimal(3, 4)
    mg <- createGiottoMulti(list(a = g1, b = g2))

    # default active = all → no active line
    expect_failure(expect_output(show(mg), "active:"))

    activeObjects(mg) <- "a"
    expect_output(show(mg), "active: a")
})


test_that("assembled joint expression on wrapped giotto carries globals", {
    g <- .mk_minimal(5, 4)
    mg <- as(g, "giottoMulti")

    e <- getExpression(mg)
    expect_s4_class(e, "exprObj")
    expect_identical(dim(e[]), c(4L, 5L))
    expect_identical(colnames(e[]),
        paste("sample1", paste0("c", 1:5), sep = "::"))
})


# @source slot + acquisition + validation -----------------------------------

test_that("giottoMulti has @source slot defaulting to NULL", {
    g <- .mk_minimal(5, 4)
    mg <- createGiottoMulti(list(a = g))
    expect_true("source" %in% slotNames("giottoMulti"))
    expect_null(mg@source)
})

test_that("multi inherits source from first sourced child", {
    g1 <- .mk_minimal(5, 4)
    g2 <- .mk_minimal(3, 4)
    fake <- structure(list(tag = "src1"), class = "fakeSource")
    g1@source <- fake

    mg <- createGiottoMulti(list(a = g1, b = g2))
    expect_identical(mg@source, fake)
})

test_that("createGiottoMulti accepts explicit source arg", {
    g <- .mk_minimal(5, 4)
    fake <- structure(list(tag = "explicit"), class = "fakeSource")
    mg <- createGiottoMulti(list(a = g), source = fake)
    expect_identical(mg@source, fake)
})

test_that("mixed-class child sources error at construction", {
    g1 <- .mk_minimal(5, 4)
    g2 <- .mk_minimal(3, 4)
    g1@source <- structure(list(), class = "srcA")
    g2@source <- structure(list(), class = "srcB")
    expect_error(
        createGiottoMulti(list(a = g1, b = g2)),
        "different classes"
    )
})

test_that("explicit source class mismatch with children errors", {
    g <- .mk_minimal(5, 4)
    g@source <- structure(list(), class = "srcA")
    expect_error(
        createGiottoMulti(list(a = g),
            source = structure(list(), class = "srcB")),
        "does not match"
    )
})


# overlay model: multi@cell_metadata as annotation layer over children ----

test_that("multi@cell_metadata empty -> pDataDT returns pure assembly", {
    g1 <- .mk_minimal(5, 4)
    g2 <- .mk_minimal(3, 4)
    mg <- createGiottoMulti(list(a = g1, b = g2))
    expect_length(mg@cell_metadata, 0L)
    pd <- pDataDT(mg)
    expect_true("list_ID" %in% names(pd))
    expect_identical(nrow(pd), 8L)
})

test_that("multi-level annotation column overlays children's values", {
    g1 <- .mk_minimal(5, 4)
    g2 <- .mk_minimal(3, 4)
    mg <- createGiottoMulti(list(a = g1, b = g2))

    # Add a multi-level cluster column via setCellMetadata
    base <- pDataDT(mg)
    clusters <- data.table::data.table(
        cell_ID = base$cell_ID,
        cluster = rep(c("X", "Y"), length.out = nrow(base))
    )
    cm <- createCellMetaObj(clusters,
        spat_unit = "cell", feat_type = "rna")
    mg <- setCellMetadata(mg, x = cm, verbose = FALSE)

    pd2 <- pDataDT(mg)
    expect_true("cluster" %in% names(pd2))
    expect_setequal(pd2$cluster, c("X", "Y"))
    # list_ID still present from assembly
    expect_true("list_ID" %in% names(pd2))
})

test_that("partial overlay coverage NA-fills unmentioned cells", {
    g1 <- .mk_minimal(5, 4)
    g2 <- .mk_minimal(3, 4)
    mg <- createGiottoMulti(list(a = g1, b = g2))

    # overlay covers only child b's cells
    base <- pDataDT(mg)
    partial <- data.table::data.table(
        cell_ID = base$cell_ID[base$list_ID == "b"],
        b_only_flag = TRUE
    )
    cm <- createCellMetaObj(partial,
        spat_unit = "cell", feat_type = "rna")
    mg <- setCellMetadata(mg, x = cm, verbose = FALSE)

    pd <- pDataDT(mg)
    expect_true("b_only_flag" %in% names(pd))
    # Cells in child b have value, cells in child a are NA
    expect_true(all(pd$b_only_flag[pd$list_ID == "b"] == TRUE))
    expect_true(all(is.na(pd$b_only_flag[pd$list_ID == "a"])))
})

test_that("child standalone view is untouched by multi-level annotations", {
    g1 <- .mk_minimal(5, 4)
    g2 <- .mk_minimal(3, 4)
    mg <- createGiottoMulti(list(a = g1, b = g2))

    cluster_dt <- data.table::data.table(
        cell_ID = paste0("a::c", 1:5),
        cluster = "X"
    )
    cm <- createCellMetaObj(cluster_dt,
        spat_unit = "cell", feat_type = "rna")
    mg <- setCellMetadata(mg, x = cm, verbose = FALSE)

    # Child accessed standalone — no `cluster` column
    child_a_pd <- pDataDT(mg@objects$a)
    expect_false("cluster" %in% names(child_a_pd))
})
