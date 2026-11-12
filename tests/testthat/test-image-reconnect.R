# reconnect() reopens an image's file after readRDS(). Its intensity range
# and integer flag are display defaults sampled from the pixels; for the same
# file they are kept, not sampled again (a full-resolution read on files
# without usable overviews).

plain <- system.file("extdata", "toy_intensity.tif", package = "GiottoClass")

# `img` through saveRDS() and readRDS(), as a saved object comes back
.round_trip <- function(img) {
    f <- tempfile(fileext = ".rds")
    on.exit(unlink(f), add = TRUE)
    saveRDS(img, f)
    readRDS(f)
}

.no_sampling <- function(env = parent.frame()) {
    local_mocked_bindings(
        .spatraster_sample_values = function(...) stop("sampled the pixels"),
        .env = env
    )
}

describe("reconnecting an image", {
    a <- createGiottoLargeImage(plain, verbose = FALSE)

    it("keeps the stored values for the same file, without sampling", {
        b <- .round_trip(a)
        b@max_intensity <- 12345
        .no_sampling()
        r <- reconnect(b)
        expect_identical(r@max_intensity, 12345)
        expect_identical(r@min_intensity, a@min_intensity)
        expect_identical(r@is_int, a@is_int)
        expect_identical(r@resolution, a@resolution)
        expect_identical(r@extent, a@extent)
        expect_gt(terra::ncol(r@raster_object), 0)
    })

    it("samples when the stored values are missing", {
        b <- .round_trip(a)
        b@max_intensity <- NA_real_
        r <- reconnect(b)
        expect_identical(r@max_intensity, a@max_intensity)
    })

    it("samples a different file again", {
        other <- tempfile(fileext = ".tif")
        on.exit(unlink(other), add = TRUE)
        file.copy(plain, other)
        b <- .round_trip(a)
        b@max_intensity <- 12345
        r <- reconnect(b, path = other)
        expect_identical(r@max_intensity, a@max_intensity)
        # the same file under another spelling is the same file
        b <- .round_trip(a)
        b@max_intensity <- 12345
        .no_sampling()
        r <- reconnect(b, path = file.path(dirname(plain), ".", basename(plain)))
        expect_identical(r@max_intensity, 12345)
    })

    it("keeps them for an affine image too", {
        aff <- spin(a, 30)
        expect_s4_class(aff, "giottoAffineImage")
        b <- .round_trip(aff)
        b@max_intensity <- 12345
        .no_sampling()
        r <- reconnect(b)
        expect_identical(r@max_intensity, 12345)
        expect_equal(r@extent, aff@extent)
    })

    it("still samples a new image", {
        expect_false(is.na(a@max_intensity))
        expect_false(is.null(a@is_int))
    })
})

describe("an affine image", {
    a <- createGiottoLargeImage(plain, verbose = FALSE)
    aff <- spin(a, 30)

    it("holds no copy of itself, and is realized from what it is now", {
        expect_null(aff@funs$realize_magick)
        moved <- spatShift(aff, dx = 1000)
        r <- doDeferred(moved, size = 1e4)
        expect_s4_class(r, "giottoLargeImage")
        expect_equal(ext(r)[], ext(moved)[])
    })

    it("drops the copy an older save carried when reconnected", {
        old <- aff
        old@funs$realize_magick <- local({
            .Object <- aff
            function(filename = NULL, size = 5e5) .Object
        })
        b <- .round_trip(old)
        expect_true(is.function(b@funs$realize_magick))
        expect_null(reconnect(b)@funs$realize_magick)
    })
})
