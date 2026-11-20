options("giotto.use_conda" = FALSE)
g <- GiottoData::loadGiottoMini("visium")
nobs <- nrow(pDataDT(g))
nvar <- nrow(fDataDT(g))

test_that("visium can join", {
    # remove to avoid warning about incorrect networks after spatloc edits
    g@spatial_network <- NULL

    j <- joinGiottoObjects(
        gobject_list = list(g, g, g),
        gobject_names = letters[seq_len(3)]
    )

    checkmate::expect_class(j, "giotto")
    expect_equal(nrow(pDataDT(j)), nobs * 3)
    expect_equal(nrow(fDataDT(j)), nvar)

    # TODO check spatial
})

test_that("join warns that multiomics results are dropped", {
    gm <- g
    gm@spatial_network <- NULL
    gm@nn_network <- NULL
    gm <- setMultiomics(gm, x = list(rna = 1), spat_unit = "cell",
        feat_type = "rna_protein", result_name = "w_rna", verbose = FALSE)
    expect_warning(j <- joinGiottoObjects(
        gobject_list = list(gm, gm),
        gobject_names = letters[seq_len(2)]
    ), "multiomics\\s+results")
    expect_null(j@multiomics)
})

test_that("expression and spatloc join", {
    gtest <- suppressWarnings(giotto())
    gtest <- setGiotto(gtest, getExpression(g), verbose = FALSE)
    gtest <- setGiotto(gtest, getSpatialLocations(g), verbose = FALSE)

    j <- joinGiottoObjects(
        gobject_list = list(gtest, gtest, gtest),
        gobject_names = letters[seq_len(3)]
    )

    checkmate::expect_class(j, "giotto")
    expect_equal(nrow(pDataDT(j)), nobs * 3)
    expect_equal(nrow(fDataDT(j)), nvar)

    # TODO check spatial
})

# test_that("poly and spatloc join", {
#     gtest <- suppressWarnings(giotto())
#     gtest <- setGiotto(gtest, getPolygonInfo(g,
#         return_giottoPolygon = TRUE),
#         verbose = FALSE
#     )
#     gtest <- setGiotto(gtest, getSpatialLocations(g), verbose = FALSE)
#
#     j <- joinGiottoObjects(
#         gobject_list = list(gtest, gtest, gtest),
#         gobject_names = letters[seq_len(3)]
#     )
#
#     checkmate::expect_class(j, "giotto")
#     expect_equal(nrow(pDataDT(j)), nobs * 3)
#     expect_equal(nrow(fDataDT(j)), nvar)
#
#     # TODO check spatial
# })
