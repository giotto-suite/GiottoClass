# TIFF directory reading, OME metadata, and the JPEG-2000 VRT path.
#
# `toy_jp2k.ome.tif` is the only fixture that reaches the JPEG-2000 branch.
# The cropped Xenium mini dataset is Deflate-encoded, so without this file
# the whole `.tif_vrt()` path would pass CI while broken. See
# `inst/scripts/make_toy_jp2k.R` for how it was built.

# Decoding JPEG-2000 needs GDAL's OpenJPEG driver. Reading the TIFF directory
# and building the VRT does not, so only the tests that pull pixels are
# skipped when a terra build lacks it -- in which case the reader falls back
# to to_simple_tif() at runtime rather than failing.
skip_if_no_jp2k <- function() {
    testthat::skip_if_not(
        "JP2OpenJPEG" %in% terra::gdal(drivers = TRUE)$name,
        "GDAL has no JP2OpenJPEG driver"
    )
}

jp2k <- system.file("extdata", "toy_jp2k.ome.tif", package = "GiottoClass")
rgb <- system.file("extdata", "toy_jp2k_rgb.tif", package = "GiottoClass")
plain <- system.file("extdata", "toy_intensity.tif", package = "GiottoClass")

describe("tif directory reading", {
    it("reads a classic TIFF header", {
        tg <- GiottoClass:::.tif_read_ifd(plain)
        expect_identical(attr(tg, "endian"), "little")
        expect_gt(tg[[GiottoClass:::.TIF_TAG$width]], 0)
        expect_gt(tg[[GiottoClass:::.TIF_TAG$height]], 0)
    })

    it("reads a BigTIFF header", {
        tg <- GiottoClass:::.tif_read_ifd(jp2k)
        expect_identical(tg[[GiottoClass:::.TIF_TAG$width]], 2048)
        expect_identical(tg[[GiottoClass:::.TIF_TAG$height]], 1024)
        expect_identical(tg[[GiottoClass:::.TIF_TAG$compression]], 34712)
        expect_identical(GiottoClass:::.tif_gdal_datatype(tg), "UInt16")
    })

    it("finds the SubIFD pyramid", {
        tg <- GiottoClass:::.tif_read_ifd(jp2k)
        subs <- tg[[GiottoClass:::.TIF_TAG$subifds]]
        expect_length(subs, 1L)
        lvl <- GiottoClass:::.tif_read_ifd(jp2k, ifd_offset = subs[[1L]])
        expect_identical(lvl[[GiottoClass:::.TIF_TAG$width]], 1024)
        expect_identical(lvl[[GiottoClass:::.TIF_TAG$height]], 512)
    })

    it("discriminates jp2k-tiled from everything else", {
        expect_true(
            GiottoClass:::.tif_is_jp2k_tiled(GiottoClass:::.tif_read_ifd(jp2k))
        )
        expect_true(
            GiottoClass:::.tif_is_jp2k_tiled(GiottoClass:::.tif_read_ifd(rgb))
        )
        expect_false(
            GiottoClass:::.tif_is_jp2k_tiled(GiottoClass:::.tif_read_ifd(plain))
        )
    })

    it("accepts the Aperio jpeg-2000 compression codes, not just 34712", {
        # Xenium writes 34712 (JP2 boxes); Aperio writes 33003/33005 (bare
        # J2K codestreams). GDAL reads both through /vsisubfile.
        expect_setequal(
            GiottoClass:::.TIF_COMPRESSION_JP2K, c(34712L, 33003L, 33005L)
        )
        tg <- GiottoClass:::.tif_read_ifd(rgb)
        expect_identical(tg[[GiottoClass:::.TIF_TAG$compression]], 33005)
        expect_identical(tg[[GiottoClass:::.TIF_TAG$samples]], 3)
        expect_identical(tg[[GiottoClass:::.TIF_TAG$bits]], c(8, 8, 8))
    })

    it("counts pages", {
        expect_length(GiottoClass:::.tif_page_offsets(jp2k), 1L)
    })
})

describe("tif_metadata without python", {
    it("reads the OME-XML from the ImageDescription tag", {
        skip_if_not_installed("xml2")
        x <- GiottoClass:::.tif_description(jp2k)
        expect_true(is.character(x))
        expect_match(x, "<OME")
    })

    it("extracts MapAnnotation key/values", {
        skip_if_not_installed("xml2")
        kv <- tif_metadata(jp2k, output = "kv")
        expect_identical(kv, list(Channel = "DAPI", Purpose = "Nuclear"))
    })

    it("extracts a named node as a data.frame", {
        skip_if_not_installed("xml2")
        ch <- tif_metadata(jp2k, node = "Channel")
        expect_s3_class(ch, "data.frame")
        expect_identical(ch$Name, "DAPI")
    })

    it("does not reach for tifffile", {
        # the python fallback in tif_metadata() is guarded on
        # .tif_metadata_xml() returning NULL. A non-NULL result here is what
        # guarantees reticulate is never touched for an OME-TIFF.
        expect_false(is.null(GiottoClass:::.tif_metadata_xml(jp2k)))
    })

    it("returns NULL for a tif with no XML description", {
        expect_null(GiottoClass:::.tif_metadata_xml(plain))
    })
})

describe("jpeg-2000 VRT", {
    it("is required: terra cannot open the file directly", {
        expect_error(suppressWarnings(terra::rast(jp2k)))
    })

    it("builds and opens", {
        skip_if_no_jp2k()
        v <- GiottoClass:::.tif_vrt(jp2k)
        expect_true(file.exists(v))
        r <- terra::rast(v, noflip = TRUE)
        expect_equal(dim(r), c(1024, 2048, 1))
        expect_identical(terra::datatype(r), "INT2U")
    })

    it("decodes the expected pixels", {
        skip_if_no_jp2k()
        r <- terra::rast(GiottoClass:::.tif_vrt(jp2k), noflip = TRUE)
        v <- as.vector(terra::values(r))
        expect_length(v, 2097152L)
        expect_equal(range(v), c(0, 159))
        expect_equal(sum(as.numeric(v)), 14371435)
    })

    it("wires the pyramid in as an overview", {
        skip_if_no_jp2k()
        v <- GiottoClass:::.tif_vrt(jp2k)
        ovs <- list.files(dirname(v), pattern = "_ov[0-9]+\\.vrt$")
        expect_length(ovs, 1L)
        expect_equal(
            dim(suppressWarnings(
                terra::rast(file.path(dirname(v), ovs[[1L]]))
            )),
            c(512, 1024, 1)
        )
    })

    it("caches per session", {
        expect_identical(
            GiottoClass:::.tif_vrt(jp2k), GiottoClass:::.tif_vrt(jp2k)
        )
    })

    it("declines files it cannot handle", {
        expect_null(GiottoClass:::.tif_vrt(plain))
    })

    it("mosaics a multi-sample (RGB) jpeg-2000 page", {
        skip_if_no_jp2k()
        v <- GiottoClass:::.tif_vrt(rgb)
        expect_false(is.null(v))
        r <- suppressWarnings(terra::rast(v, noflip = TRUE))
        expect_equal(dim(r), c(240, 480, 3))
        expect_identical(terra::datatype(r)[[1L]], "INT1U")
    })

    it("decodes each RGB band to the same values as tifffile", {
        skip_if_no_jp2k()
        # reference values read with python tifffile from this same fixture;
        # a band mix-up or a chunky/planar mistake moves all three
        r <- suppressWarnings(
            terra::rast(GiottoClass:::.tif_vrt(rgb), noflip = TRUE)
        )
        sums <- vapply(seq_len(terra::nlyr(r)), function(b) {
            sum(as.numeric(terra::values(r[[b]])))
        }, numeric(1L))
        expect_equal(sums, c(27937839, 27914769, 27935919))
    })

    it("keeps same-named sources in different directories apart", {
        a <- GiottoClass:::.tif_vrt_id("/one/morphology_focus_0000.ome.tif::1")
        b <- GiottoClass:::.tif_vrt_id("/two/morphology_focus_0000.ome.tif::1")
        expect_false(identical(a, b))
        expect_match(a, "^[0-9a-f]{8}$")
    })
})

describe("image reading routes by capability", {
    it("reads a plain tif directly", {
        r <- GiottoClass:::.create_terra_spatraster(plain)
        expect_s4_class(r, "SpatRaster")
    })

    it("routes jpeg-2000 through the VRT", {
        skip_if_no_jp2k()
        r <- GiottoClass:::.create_terra_spatraster(jp2k)
        expect_s4_class(r, "SpatRaster")
        expect_equal(dim(r), c(1024, 2048, 1))
        expect_match(terra::sources(r), "\\.vrt$")
    })

    it("never needs the python converter for either", {
        skip_if_no_jp2k()
        # to_simple_tif() needs python. Both files must be served by one of
        # the two R rungs, so neither reaches the fallback.
        expect_no_error(GiottoClass:::.create_terra_spatraster(plain))
        expect_no_error(GiottoClass:::.create_terra_spatraster(jp2k))
    })

    it("routes a multi-sample jpeg-2000 file through the VRT", {
        skip_if_no_jp2k()
        r <- GiottoClass:::.create_terra_spatraster(rgb)
        expect_s4_class(r, "SpatRaster")
        expect_equal(dim(r), c(240, 480, 3))
        expect_match(terra::sources(r)[[1L]], "\\.vrt$")
    })

    it("still errors on something unreadable", {
        f <- tempfile(fileext = ".tif")
        writeLines("not a tif", f)
        expect_error(GiottoClass:::.create_terra_spatraster(f))
    })
})

describe("createGiottoLargeImage on jpeg-2000", {
    it("creates a usable image object", {
        skip_if_no_jp2k()
        gimg <- createGiottoLargeImage(jp2k, name = "toy", verbose = FALSE)
        expect_s4_class(gimg, "giottoLargeImage")
        expect_identical(gimg@file_path, jp2k)
        expect_equal(
            unname(as.vector(terra::ext(gimg@raster_object))),
            c(0, 2048, -1024, 0)
        )
    })

    it("survives a reconnect", {
        skip_if_no_jp2k()
        gimg <- createGiottoLargeImage(jp2k, name = "toy", verbose = FALSE)
        expect_no_error(reconnect(gimg))
    })
})
