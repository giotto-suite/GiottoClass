# Diff logic is PURE: manifests in, diff out. These tests build manifests by
# hand and never touch a giotto object, so a change to any accessor cannot
# break them - and they still pass when Giotto is unavailable.

.mf <- function(slots = list(), n_cells = list(cell = 100L),
    n_features = list(rna = 50L), uid = "g-test") {
    structure(
        list(
            schema_version = "0.2.0",
            object = list(uid = uid, class = "giotto"),
            summary = list(n_cells = n_cells, n_features = n_features),
            slots = slots,
            warnings = character()
        ),
        class = c("gmanifest", "list")
    )
}

.expr <- function(name, fp = "aaaa") {
    list(keys = list(spat_unit = "cell", feat_type = "rna", name = name),
        class = "exprObj", dtype = "dgCMatrix", fingerprint = fp)
}

.meta <- function(cols) {
    list(
        keys = list(spat_unit = "cell", feat_type = "rna"),
        class = "cellMetaObj",
        columns = lapply(cols, function(n) {
            list(dtype = "numeric", n_levels = n)
        })
    )
}

.ids <- function(fp) {
    list(cell_ID = list(list(keys = list(spat_unit = "cell"),
        fingerprint = fp)))
}

describe("manifestDiff", {
    it("reports no change for identical manifests", {
        m <- .mf(list(expression = list(.expr("raw"))))
        d <- manifestDiff(m, m)
        expect_false(d$changed)
        expect_identical(d$summary, "no state change")
    })

    it("reports an added leaf by name", {
        b <- .mf(list(expression = list(.expr("raw"))))
        a <- .mf(list(expression = list(.expr("raw"), .expr("normalized"))))
        d <- manifestDiff(b, a)
        expect_true(d$changed)
        expect_match(d$summary, "expression added: normalized")
        expect_named(d$detail$added, "expression.cell.rna.normalized")
    })

    it("reports a removed leaf", {
        b <- .mf(list(expression = list(.expr("raw"), .expr("scaled"))))
        a <- .mf(list(expression = list(.expr("raw"))))
        d <- manifestDiff(b, a)
        expect_match(d$summary, "expression removed: scaled")
        expect_identical(d$detail$removed, "expression.cell.rna.scaled")
    })

    it("names a new metadata column with its level count", {
        b <- .mf(list(cell_metadata = list(
            .meta(list(cell_ID = 100L, total_expr = 100L))
        )))
        a <- .mf(list(cell_metadata = list(
            .meta(list(cell_ID = 100L, total_expr = 100L, leiden_clus = 14L))
        )))
        d <- manifestDiff(b, a)
        expect_match(d$summary, "leiden_clus \\(14 levels\\)")
    })

    it("states a cell count change once and tallies the changed objects", {
        leaves <- function(fp) list(
            expression = list(
                .expr("raw", fp),
                .expr("normalized", fp)
            ),
            spatial_locs = list(
                list(keys = list(spat_unit = "cell", name = "raw"),
                    class = "spatLocsObj", fingerprint = fp)
            )
        )
        d <- manifestDiff(
            .mf(leaves("aaaa"), n_cells = list(cell = 100L)),
            .mf(leaves("bbbb"), n_cells = list(cell = 40L))
        )
        expect_match(d$summary, "cells \\[cell\\]: 100 -> 40")
        expect_match(d$summary, "3 objects changed")
        expect_false(grepl("modified", d$summary))
        # the per-leaf detail is still complete
        expect_length(d$detail$modified, 3L)
    })

    it("still names a non-content change alongside a count change", {
        b <- .mf(list(expression = list(.expr("raw"))),
            n_cells = list(cell = 100L))
        a_rec <- .expr("raw", fp = "bbbb")
        a_rec$dtype <- "parquetExprStore"
        a <- .mf(list(expression = list(a_rec)), n_cells = list(cell = 40L))
        expect_match(manifestDiff(b, a)$summary, "raw modified \\(dtype")
    })

    it("detects content changes that keep every name", {
        b <- .mf(list(expression = list(.expr("raw", fp = "aaaa"))))
        a <- .mf(list(expression = list(.expr("raw", fp = "bbbb"))))
        d <- manifestDiff(b, a)
        expect_true(d$changed)
        expect_match(d$summary, "modified \\(fingerprint\\)")
    })

    it("flags a swapped object identity", {
        b <- .mf(uid = "g-one")
        a <- .mf(uid = "g-two")
        d <- manifestDiff(b, a)
        expect_match(d$summary, "object identity changed")
        expect_identical(d$detail$object$uid$after, "g-two")
    })

    it("treats a NULL before as creation and a NULL after as removal", {
        m <- .mf()
        expect_match(manifestDiff(NULL, m)$summary, "created")
        expect_match(manifestDiff(m, NULL)$summary, "object removed")
    })

    it("states the count once when only the ids moved", {
        b <- .mf(.ids("aaaa"), n_cells = list(cell = 100L))
        a <- .mf(.ids("bbbb"), n_cells = list(cell = 40L))
        d <- manifestDiff(b, a)
        expect_match(d$summary, "cells \\[cell\\]: 100 -> 40")
    })

    it("keys a record by its slot and identity fields", {
        m <- .mf(list(nn_network = list(list(
            keys = list(spat_unit = "cell", feat_type = "rna",
                nn_type = "sNN", name = "sNN.pca"),
            class = "nnNetObj"
        ))))
        flat <- .manifest_flatten(m$slots)
        expect_named(flat, "nn_network.cell.rna.sNN.sNN.pca")
    })

    it("never reports 'no state change' for a leaf it marked modified", {
        # a re-clustering rewrites an existing column in place: same column
        # set, different values. The leaf was modified, so the
        # sentence must say so.
        b <- .mf(list(cell_metadata = list(
            .meta(list(cell_ID = 100L, leiden_clus = 5L))
        )))
        a <- .mf(list(cell_metadata = list(
            .meta(list(cell_ID = 100L, leiden_clus = 14L))
        )))
        d <- manifestDiff(b, a)
        expect_true(d$changed)
        expect_false(identical(d$summary, "no state change"))
        expect_match(d$summary, "leiden_clus \\(5 -> 14 levels\\)")
    })

    it("does not treat a reordered column list as a change", {
        cols <- function(order) {
            list(keys = list(spat_unit = "cell", feat_type = "rna"),
                class = "cellMetaObj",
                columns = stats::setNames(lapply(order, function(cc) {
                    list(dtype = "numeric", n_levels = 5L)
                }), order))
        }
        b <- .mf(list(cell_metadata = list(cols(c("a", "b")))))
        a <- .mf(list(cell_metadata = list(cols(c("b", "a")))))
        expect_false(manifestDiff(b, a)$changed)
    })

    it("reports a content-only change on any leaf type", {
        leaf <- function(fp) list(
            keys = list(spat_unit = "cell", name = "Delaunay_network"),
            class = "spatialNetworkObj", method = "deldir", fingerprint = fp)
        d <- manifestDiff(
            .mf(list(spatial_network = list(leaf("aaaa")))),
            .mf(list(spatial_network = list(leaf("bbbb"))))
        )
        expect_match(d$summary, "modified \\(fingerprint\\)")
    })

    it("keeps the tally free of spurious extra phrases", {
        leaves <- function(fp) list(expression = list(.expr("raw", fp)))
        d <- manifestDiff(
            .mf(leaves("aaaa"), n_cells = list(cell = 100L)),
            .mf(leaves("bbbb"), n_cells = list(cell = 40L))
        )
        expect_match(d$summary, "1 objects changed")
        expect_false(grepl("modified \\(\\)", d$summary))
    })
})

describe("records the diff cannot see into", {
    nofp <- function() list(cell_metadata = list(list(
        keys = list(spat_unit = "cell", feat_type = "rna"),
        class = "cellMetaObj"
    )))

    it("does not claim no change for a record with no fingerprint", {
        m <- .mf(nofp())
        d <- manifestDiff(m, m)
        expect_false(d$changed)
        expect_identical(d$summary, "no detected change (1 item unfingerprinted)")
        expect_identical(d$detail$unfingerprinted, "cell_metadata.cell.rna")
    })

})

describe("level counts in the sentence", {
    # the row count comes from the summary block, as it does for a real
    # manifest
    .meta_n <- function(cols, nrow) {
        structure(list(keys = list(spat_unit = "cell", feat_type = "rna"),
            class = "cellMetaObj",
            columns = lapply(cols, function(n) {
                list(dtype = "numeric", n_levels = n)
            })), nrow = nrow)
    }
    wrap <- function(leaf) {
        .mf(list(cell_metadata = list(unclass_nrow(leaf))),
            n_cells = list(cell = attr(leaf, "nrow")))
    }
    unclass_nrow <- function(leaf) {
        attr(leaf, "nrow") <- NULL
        leaf
    }

    it("keeps the count for a categorical column", {
        d <- manifestDiff(
            wrap(.meta_n(list(cell_ID = 1522L), 1522L)),
            wrap(.meta_n(list(cell_ID = 1522L, leiden_clus = 11L), 1522L))
        )
        expect_match(d$summary, "leiden_clus \\(11 levels\\)")
    })

    it("drops the count for a near-unique column but still reports it", {
        # "total_expr (1428 levels)" over 1522 cells only says "continuous"
        d <- manifestDiff(
            wrap(.meta_n(list(cell_ID = 1522L), 1522L)),
            wrap(.meta_n(list(cell_ID = 1522L, total_expr = 1428L), 1522L))
        )
        expect_match(d$summary, "added: total_expr")
        expect_false(grepl("1428 levels", d$summary))
    })

    it("keeps small counts even when rows are few", {
        d <- manifestDiff(
            wrap(.meta_n(list(cell_ID = 20L), 20L)),
            wrap(.meta_n(list(cell_ID = 20L, hvf = 2L), 20L))
        )
        expect_match(d$summary, "hvf \\(2 levels\\)")
    })

    it("names a re-levelled continuous column without a count", {
        d <- manifestDiff(
            wrap(.meta_n(list(total_expr = 1428L), 1522L)),
            wrap(.meta_n(list(total_expr = 1400L), 1522L))
        )
        expect_match(d$summary, "changed: total_expr")
        expect_false(grepl("levels", d$summary))
    })
})

describe("display names", {
    it("does not truncate a leaf name that contains a dot", {
        # nn_network ids end ".sNN.sNN.pca", so splitting on the last dot
        # reported an sNN.pca network as "pca"
        leaf <- function(fp) list(
            keys = list(spat_unit = "cell", feat_type = "rna",
                nn_type = "sNN", name = "sNN.pca"),
            class = "nnNetObj", fingerprint = fp)
        wrap <- function(l) .mf(list(nn_network = list(l)))
        d <- manifestDiff(wrap(leaf("aaaa")), wrap(leaf("bbbb")))
        expect_match(d$summary, "sNN\\.pca modified")
    })

    it("names an added leaf by its name", {
        after <- .mf(list(expression = list(
            .expr("raw"), .expr("pearson.resid")
        )))
        before <- .mf(list(expression = list(.expr("raw"))))
        expect_match(manifestDiff(before, after)$summary,
            "expression added: pearson\\.resid")
    })

    it("falls back to the last key when a leaf carries no name", {
        b <- .mf(.ids("aaaa"))
        a <- .mf(.ids("bbbb"))
        expect_match(manifestDiff(b, a)$summary, "cell IDs cell")
    })
})
