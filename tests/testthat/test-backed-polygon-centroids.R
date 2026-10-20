# A polygon set on a backed object is written to a store, and its centroids
# are derived from that store. Terra centroids carried in with it must not stay
# behind: they do not survive a snapshot, and subsetting by poly_ID then fails.

test_that("a backed polygon keeps no terra centroids and subsets after a snapshot", {
    skip_if_not_installed("GiottoDisk")
    proj <- tempfile("backed_poly_")
    on.exit(unlink(proj, recursive = TRUE), add = TRUE)
    src <- suppressMessages(GiottoDisk::sourceCreate(proj, type = "gDirSource"))
    suppressMessages(GiottoDisk::setArtifactDumpDir(src))

    sq <- lapply(seq_len(20), function(i) {
        x0 <- (i - 1) %% 5 * 10
        y0 <- (i - 1) %/% 5 * 10
        cbind(i, 1, c(x0, x0 + 8, x0 + 8, x0, x0), c(y0, y0, y0 + 8, y0 + 8, y0), 0)
    })
    sv <- terra::vect(do.call(rbind, sq), type = "polygons")
    sv$poly_ID <- paste0("c", seq_len(20))
    gp <- createGiottoPolygon(sv, name = "cell", verbose = FALSE)
    gp <- centroids(gp, append_gpolygon = TRUE)
    expect_s4_class(gp@spatVectorCentroids, "SpatVector")

    g <- suppressMessages(createGiottoObject(backend = src))
    g <- setGiotto(g, gp, centroids_to_spatlocs = TRUE, verbose = FALSE)
    p <- getPolygonInfo(g, "cell", return_giottoPolygon = TRUE)
    expect_null(p@spatVectorCentroids)
    expect_equal(nrow(getSpatialLocations(g, spat_unit = "cell",
        output = "data.table")), 20L)

    g <- suppressMessages(saveGiotto(g, name = "snap", verbose = FALSE))
    g2 <- suppressMessages(loadGiotto(proj, name = "snap", verbose = FALSE))
    sub <- subsetGiotto(g2, spat_unit = "cell", cell_ids = paste0("c", 1:5),
        verbose = FALSE)
    expect_setequal(
        spatIDs(getPolygonInfo(sub, "cell", return_giottoPolygon = TRUE)),
        paste0("c", 1:5)
    )
})
