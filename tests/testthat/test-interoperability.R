# Seurat, AnnData, SpatialData and Bento conversions live in GiottoConnect.

test_that("Giotto to SpatialExperiment Conversion Works", {
    skip_if_not_installed("SpatialExperiment")
    giotto_obj <- GiottoData::loadGiottoMini("visium")
    spe_obj <- giottoToSpatialExperiment(giotto_obj)
    expect_s4_class(spe_obj[[1]], "SpatialExperiment")
})
