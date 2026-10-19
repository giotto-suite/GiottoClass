
describe("Giotto Object Save/Load", {

    g <- test_data$viz
    empty <- giotto()
    test <- tempdir()

    describe("RDS method", {

        # register cleanup
        on.exit(unlink(file.path(test, "saveGiottoDir"), recursive = TRUE),
                add = TRUE)

        it("it can be saved and loaded", {
            rlang::local_options(lifecycle_verbosity = "quiet")
            saveGiotto(g, dir = test, overwrite = TRUE, verbose = FALSE)
            g2 <<- loadGiotto(file.path(test, "saveGiottoDir"))

            expect_true(methods::validObject(g2))
        })

        it("it can be overwritten and loaded", {
            rlang::local_options(lifecycle_verbosity = "quiet")
            saveGiotto(g2, dir = test, overwrite = TRUE, verbose = FALSE)
            g3 <- loadGiotto(file.path(test, "saveGiottoDir"))

            expect_true(methods::validObject(g3))
        })

    })

    describe("qs2 method", {

        on.exit(unlink(file.path(test, c("qs2Dir", "legacyQs")),
                       recursive = TRUE), add = TRUE)

        it("it can be saved and loaded", {
            skip_if_not_installed("qs2")
            rlang::local_options(lifecycle_verbosity = "quiet")
            saveGiotto(g, dir = test, foldername = "qs2Dir", method = "qs2",
                       overwrite = TRUE, verbose = FALSE)
            expect_true(file.exists(file.path(test, "qs2Dir", "gobject.qs2")))
            g2 <- loadGiotto(file.path(test, "qs2Dir"), verbose = FALSE)
            expect_true(methods::validObject(g2))
            expect_identical(spatIDs(g2), spatIDs(g))
        })

        it("a .qs save is not a recognized format", {
            dir.create(file.path(test, "legacyQs"), showWarnings = FALSE)
            writeBin(as.raw(c(0x0b, 0x0e, 0x0a, 0x0c)),
                     file.path(test, "legacyQs", "gobject.qs"))
            expect_error(loadGiotto(file.path(test, "legacyQs"),
                                    verbose = FALSE), "not a recognized")
        })
    })

    describe("backend-managed project", {

        proj <- file.path(test, "oldLayoutProject")
        on.exit(unlink(proj, recursive = TRUE), add = TRUE)

        it("is upgraded to the backend's current layout and loads", {
            skip_if_not_installed("GiottoDisk", minimum_version = "0.0.0.6")
            # a project as an older backend left it: schema v1, one flat
            # snapshot file
            dir.create(file.path(proj, "giottosave"), recursive = TRUE)
            jsonlite::write_json(list(
                header = list(versions = list(manifest = 1L)),
                content = list()
            ), file.path(proj, "giottodir.json"), auto_unbox = TRUE)
            saveRDS(empty, file.path(proj, "giottosave", "snap.rds"))

            g2 <- loadGiotto(proj, verbose = FALSE)
            expect_s4_class(g2, "giotto")
            s <- GiottoDisk::sourceSchema(proj)
            expect_identical(s$version, s$current)
        })
    })

    describe("zero attribute SpatVector", {

        # register cleanup
        on.exit(unlink(file.path(test, c("zeroattr_poly", "zeroattr_pts",
                                 "badnames")),
                       recursive = TRUE),
                add = TRUE)

        it("polygons round trip without attributes", {
            sv <- terra::vect("POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0))")
            terra::values(sv) <- NULL
            g <- giotto()
            g@spatial_info <- list(cell = new("giottoPolygon",
                                              name = "cell", spatVector = sv))

            saveGiotto(g, dir = test, foldername = "zeroattr_poly",
                       overwrite = TRUE, verbose = FALSE)
            g2 <- loadGiotto(file.path(test, "zeroattr_poly"), verbose = FALSE)
            sv2 <- g2@spatial_info$cell@spatVector

            expect_equal(nrow(sv2), nrow(sv))
            # writing a shapefile with an empty attribute table adds a
            # placeholder "FID" field that must not be read back in
            expect_equal(terra::ncol(sv2), 0L)
            expect_length(names(sv2), 0L)
        })

        it("reports a names file that does not match the attributes", {
            gp <- test_data$gpoly
            g <- setGiotto(giotto(), gp, verbose = FALSE)

            saveGiotto(g, dir = test, foldername = "badnames",
                       overwrite = TRUE, verbose = FALSE)
            # corrupt the saved column names
            nfile <- file.path(test, "badnames", "SpatialInfo",
                               paste0(objName(gp), "_spatInfo_spatVector",
                                      "_names.txt"))
            write.table(c("poly_ID", "extra"), file = nfile,
                        col.names = FALSE, row.names = FALSE)

            expect_error(
                loadGiotto(file.path(test, "badnames"), verbose = FALSE),
                sprintf("2 saved column names for %d attributes",
                        terra::ncol(gp[]))
            )
        })

        it("points round trip without attributes", {
            pv <- terra::vect(cbind(c(1, 2, 3), c(1, 2, 3)), type = "points")
            terra::values(pv) <- NULL
            g <- giotto()
            g@feat_info <- list(rna = new("giottoPoints",
                                          feat_type = "rna", spatVector = pv))

            saveGiotto(g, dir = test, foldername = "zeroattr_pts",
                       overwrite = TRUE, verbose = FALSE)
            g2 <- loadGiotto(file.path(test, "zeroattr_pts"), verbose = FALSE)
            pv2 <- g2@feat_info$rna@spatVector

            expect_equal(nrow(pv2), nrow(pv))
            expect_equal(terra::ncol(pv2), 0L)
            expect_length(names(pv2), 0L)
        })

    })

})
