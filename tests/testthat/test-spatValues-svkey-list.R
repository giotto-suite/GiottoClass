# spatValues(svkey = list(...)): each key is a full location, the results are
# joined on cell_ID, and a value name repeated across keys is prefixed rather
# than refused -- pulling one name from several places is the point.

g <- GiottoData::loadGiottoMini("visium", verbose = FALSE)
f <- featIDs(g)[1]

test_that("a single svkey behaves as before", {
    k <- svkey(f, expression_values = "raw")
    expect_identical(spatValues(g, svkey = k), k@get(g))
})

test_that("the same feature from two expression items, named list", {
    got <- spatValues(g, svkey = list(
        r = svkey(f, expression_values = "raw"),
        n = svkey(f, expression_values = "normalized")
    ))
    expect_named(got, c("cell_ID", paste0("r_", f), paste0("n_", f)))
    raw <- spatValues(g, feats = f, expression_values = "raw")
    nrm <- spatValues(g, feats = f, expression_values = "normalized")
    expect_identical(got[[paste0("r_", f)]], raw[[f]])
    expect_identical(got[[paste0("n_", f)]], nrm[[f]])
})

test_that("an unnamed list prefixes by the key's location", {
    got <- spatValues(g, svkey = list(
        svkey(f, expression_values = "raw"),
        svkey(f, expression_values = "normalized")
    ))
    expect_named(got, c("cell_ID", paste0("raw_", f),
        paste0("normalized_", f)))
})

test_that("multi-feature keys prefix only the names they share", {
    ff <- featIDs(g)[1:3]
    got <- spatValues(g, svkey = list(
        r = svkey(ff[1:2], expression_values = "raw"),
        n = svkey(ff[2:3], expression_values = "normalized")
    ))
    expect_named(got, c("cell_ID", ff[1], paste0("r_", ff[2]),
        paste0("n_", ff[2]), ff[3]))
    raw <- spatValues(g, feats = ff[1:2], expression_values = "raw")
    nrm <- spatValues(g, feats = ff[2:3], expression_values = "normalized")
    expect_identical(got[[ff[1]]], raw[[ff[1]]])
    expect_identical(got[[paste0("r_", ff[2])]], raw[[ff[2]]])
    expect_identical(got[[paste0("n_", ff[2])]], nrm[[ff[2]]])
    expect_identical(got[[ff[3]]], nrm[[ff[3]]])
})

test_that("names that do not collide stay bare, across slots", {
    got <- spatValues(g, svkey = list(
        svkey(f, expression_values = "normalized"),
        svkey("leiden_clus", slot = "cell_metadata")
    ))
    expect_named(got, c("cell_ID", f, "leiden_clus"))
    meta <- spatValues(g, feats = "leiden_clus", slot = "cell_metadata")
    expect_identical(
        got$leiden_clus,
        meta$leiden_clus[match(got$cell_ID, meta$cell_ID)]
    )
})

test_that("the same name in expression and metadata is told apart", {
    cx <- getCellMetadata(g, output = "cellMetaObj")
    cx[][, (f) := "meta"]
    g2 <- setCellMetadata(g, cx, verbose = FALSE, initialize = FALSE)
    got <- spatValues(g2, svkey = list(
        e = svkey(f, expression_values = "raw"),
        m = svkey(f, slot = "cell_metadata")
    ))
    expect_type(got[[paste0("e_", f)]], "double")
    expect_identical(unique(got[[paste0("m_", f)]]), "meta")
})

test_that("keys on different spat_units are refused before any fetch", {
    expect_error(
        spatValues(g, svkey = list(
            svkey(f, spat_unit = "cell"),
            svkey(f, spat_unit = "nucleus")
        )),
        "different spat_units"
    )
})

test_that("identical keys in an unnamed list ask for names", {
    k <- svkey(f, expression_values = "raw")
    expect_error(spatValues(g, svkey = list(k, k)), "Name the list")
    expect_no_error(spatValues(g, svkey = list(a = k, b = k)))
})

test_that("anything but svkeys in the list is an error", {
    expect_error(spatValues(g, svkey = list(svkey(f), "leiden_clus")))
})

test_that("the join is a full join in first-appearance order", {
    a <- data.table::data.table(cell_ID = c("c1", "c2", "c3"), x = 1:3)
    b <- data.table::data.table(cell_ID = c("c3", "c4"), x = c(30L, 40L),
        y = c("p", "q"))
    got <- .sv_join_keyed(list(a, b), labels = c("A", "B"))
    expect_identical(got$cell_ID, c("c1", "c2", "c3", "c4"))
    expect_identical(got$A_x, c(1L, 2L, 3L, NA))
    expect_identical(got$B_x, c(NA, NA, 30L, 40L))
    expect_identical(got$y, c(NA, NA, "p", "q"))
})
