# Reading .tif files, including formats terra cannot open directly.
#
# Three layers, in dependency order:
#
#   directory   a minimal TIFF / BigTIFF image file directory reader. Answers
#               where each tile lives and what the ImageDescription says,
#               without decoding a pixel.
#   vrt         a GDAL VRT mosaicking JPEG-2000 tiles through /vsisubfile/,
#               so images libtiff has no codec for still read lazily.
#   converters  the user-facing `to_simple_tif()` and `tif_metadata()`.
#
# `.create_terra_spatraster()` in images.R is the consumer: it tries terra
# first, falls back to the VRT here, and only then to `to_simple_tif()`.
# See adr/0005 for why the VRT rather than a conversion.


# directory ####

# byte size per TIFF field type, indexed by the type code (1-18)
.TIF_TYPE_SIZE <- c(
    1, # 1  BYTE
    1, # 2  ASCII
    2, # 3  SHORT
    4, # 4  LONG
    8, # 5  RATIONAL
    1, # 6  SBYTE
    1, # 7  UNDEFINED
    2, # 8  SSHORT
    4, # 9  SLONG
    8, # 10 SRATIONAL
    4, # 11 FLOAT
    8, # 12 DOUBLE
    4, # 13 IFD
    0, # 14 unused
    0, # 15 unused
    8, # 16 LONG8
    8, # 17 SLONG8
    8 # 18 IFD8
)

# TIFF tag numbers used here
.TIF_TAG <- list(
    width = "256",
    height = "257",
    bits = "258",
    compression = "259",
    photometric = "262",
    description = "270",
    strip_offsets = "273",
    samples = "277",
    strip_counts = "279",
    tile_width = "322",
    tile_height = "323",
    tile_offsets = "324",
    tile_counts = "325",
    sample_format = "339",
    subifds = "330"
)

# TIFF compression codes that carry JPEG-2000 payloads.
#   34712  JP2 boxes, as written by 10x Xenium
#   33003  Aperio, raw J2K codestream, RGB
#   33005  Aperio, raw J2K codestream, YCbCr
# GDAL's OpenJPEG driver reads all three through /vsisubfile, boxed or bare.
.TIF_COMPRESSION_JP2K <- c(34712L, 33003L, 33005L)

#' @title Decode an unsigned integer from raw bytes
#' @name .tif_uint
#' @param x raw vector holding exactly the value
#' @param endian "little" or "big"
#' @param size number of bytes
#' @returns numeric scalar
#' @keywords internal
#' @noRd
.tif_uint <- function(x, endian, size) {
    b <- as.numeric(x)
    pw <- seq_len(size) - 1L
    if (endian != "little") pw <- rev(pw)
    sum(b * 256^pw)
}

# vectorised form: split `x` into `n` values of `size` bytes each
.tif_uint_vec <- function(x, endian, size, n) {
    if (n == 0L) {
        return(numeric(0))
    }
    m <- matrix(as.numeric(x), nrow = size, ncol = n)
    pw <- seq_len(size) - 1L
    if (endian != "little") pw <- rev(pw)
    as.vector(crossprod(m, 256^pw))
}

#' @title Read one TIFF image file directory
#' @name .tif_read_ifd
#' @description
#' Parse a single IFD from a TIFF or BigTIFF file. Returns the tag values
#' keyed by tag number as character, with the byte offset of the following
#' IFD attached as the `next_ifd` attribute (0 when this is the last one)
#' and the endianness as `endian`.
#' @param path character. Filepath to the tif
#' @param ifd_offset numeric or NULL. Byte offset of the IFD to read. `NULL`
#' reads the first IFD named in the file header.
#' @returns named list of tag values, with `next_ifd` and `endian` attributes
#' @keywords internal
#' @noRd
.tif_read_ifd <- function(path, ifd_offset = NULL) {
    con <- file(path, "rb")
    on.exit(close(con), add = TRUE)

    bo <- rawToChar(readBin(con, "raw", 2L))
    if (!bo %in% c("II", "MM")) {
        stop("not a TIFF file: ", path, call. = FALSE)
    }
    endian <- if (bo == "II") "little" else "big"

    version <- .tif_uint(readBin(con, "raw", 2L), endian, 2L)
    big <- identical(version, 43)
    if (!big && !identical(version, 42)) {
        stop("unrecognized TIFF version ", version, ": ", path, call. = FALSE)
    }

    # BigTIFF header carries offset size (always 8) and a zero pad
    osize <- if (big) 8L else 4L
    if (big) readBin(con, "raw", 4L)

    if (is.null(ifd_offset)) {
        ifd_offset <- .tif_uint(readBin(con, "raw", osize), endian, osize)
    }

    seek(con, ifd_offset)
    csize <- if (big) 8L else 2L
    n <- .tif_uint(readBin(con, "raw", csize), endian, csize)

    tags <- list()
    for (i in seq_len(n)) {
        tag <- .tif_uint(readBin(con, "raw", 2L), endian, 2L)
        type <- .tif_uint(readBin(con, "raw", 2L), endian, 2L)
        count <- .tif_uint(readBin(con, "raw", osize), endian, osize)
        inline <- readBin(con, "raw", osize)

        size <- if (type >= 1 && type <= 18) .TIF_TYPE_SIZE[[type]] else 0
        if (size == 0) next # unknown or unused type, skip

        total <- size * count
        payload <- if (total <= osize) {
            inline[seq_len(total)]
        } else {
            # value does not fit in the entry, follow the offset and come back
            at <- .tif_uint(inline, endian, osize)
            here <- seek(con)
            seek(con, at)
            out <- readBin(con, "raw", total)
            seek(con, here)
            out
        }

        tags[[as.character(tag)]] <- if (type == 2L) {
            # ASCII, NUL terminated
            rawToChar(payload[payload != as.raw(0L)])
        } else {
            .tif_uint_vec(payload, endian, size, count)
        }
    }

    attr(tags, "next_ifd") <- .tif_uint(
        readBin(con, "raw", osize), endian, osize
    )
    attr(tags, "endian") <- endian
    tags
}

#' @title List the IFD offsets of every page in a tif
#' @name .tif_page_offsets
#' @description
#' Walk the IFD chain and return one byte offset per page. Multi-page files
#' such as the Xenium `morphology.ome.tif` z-stacks carry one full-resolution
#' plane per page.
#' @param path character. Filepath to the tif
#' @param max_pages numeric. Stop after this many pages
#' @returns numeric vector of IFD offsets
#' @keywords internal
#' @noRd
.tif_page_offsets <- function(path, max_pages = 1000L) {
    offs <- numeric(0)
    nxt <- NULL
    repeat {
        tags <- .tif_read_ifd(path, ifd_offset = nxt)
        # the first read resolves the header offset for us, recover it
        offs <- c(offs, if (is.null(nxt)) .tif_first_ifd(path) else nxt)
        nxt <- attr(tags, "next_ifd")
        if (nxt == 0 || length(offs) >= max_pages) break
    }
    offs
}

# byte offset of the first IFD, from the header alone
.tif_first_ifd <- function(path) {
    con <- file(path, "rb")
    on.exit(close(con), add = TRUE)
    bo <- rawToChar(readBin(con, "raw", 2L))
    endian <- if (bo == "II") "little" else "big"
    big <- .tif_uint(readBin(con, "raw", 2L), endian, 2L) == 43
    osize <- if (big) 8L else 4L
    if (big) readBin(con, "raw", 4L)
    .tif_uint(readBin(con, "raw", osize), endian, osize)
}

#' @title Is this page JPEG-2000 compressed and tiled?
#' @name .tif_is_jp2k_tiled
#' @description
#' The combination `.tif_vrt()` can handle. Each JPEG-2000 tile is a complete
#' image stream in its own right -- a JP2 box in Xenium files, a bare J2K
#' codestream in Aperio ones -- so GDAL's `/vsisubfile/` can address them
#' individually;
#' Deflate and LZW tiles are bare codec streams and cannot be addressed that
#' way (GDAL reads those files natively regardless). Strip-based JPEG-2000
#' has not been observed in the wild and is deliberately excluded.
#' @param tags list from [.tif_read_ifd()]
#' @returns logical
#' @keywords internal
#' @noRd
.tif_is_jp2k_tiled <- function(tags) {
    comp <- tags[[.TIF_TAG$compression]]
    if (is.null(comp) || !comp[[1L]] %in% .TIF_COMPRESSION_JP2K) {
        return(FALSE)
    }
    !is.null(tags[[.TIF_TAG$tile_offsets]]) &&
        !is.null(tags[[.TIF_TAG$tile_counts]]) &&
        !is.null(tags[[.TIF_TAG$tile_width]]) &&
        !is.null(tags[[.TIF_TAG$tile_height]])
}

#' @title GDAL datatype name for a tif page
#' @name .tif_gdal_datatype
#' @param tags list from [.tif_read_ifd()]
#' @returns character. A GDAL datatype name, or NULL if unsupported
#' @keywords internal
#' @noRd
.tif_gdal_datatype <- function(tags) {
    bits <- tags[[.TIF_TAG$bits]]
    if (is.null(bits)) {
        return(NULL)
    }
    bits <- bits[[1L]]
    fmt <- tags[[.TIF_TAG$sample_format]]
    fmt <- if (is.null(fmt)) 1L else fmt[[1L]]

    switch(as.character(fmt),
        "1" = switch(as.character(bits),
            "8" = "Byte", "16" = "UInt16", "32" = "UInt32", NULL
        ),
        "2" = switch(as.character(bits),
            "8" = "Int8", "16" = "Int16", "32" = "Int32", NULL
        ),
        "3" = switch(as.character(bits),
            "32" = "Float32", "64" = "Float64", NULL
        ),
        NULL
    )
}

#' @title Read the ImageDescription of a tif page
#' @name .tif_description
#' @description
#' TIFF tag 270. For an OME-TIFF this is the OME-XML document; for a qptiff
#' it is the per-page XML block.
#' @param path character. Filepath to the tif
#' @param page numeric. 1-based page index
#' @returns character scalar, or NULL when the tag is absent
#' @keywords internal
#' @noRd
.tif_description <- function(path, page = 1L) {
    offs <- .tif_page_offsets(path, max_pages = page)
    if (page > length(offs)) {
        stop(sprintf(
            "page %d requested but file has %d page(s)", page, length(offs)
        ), call. = FALSE)
    }
    tags <- .tif_read_ifd(path, ifd_offset = offs[[page]])
    tags[[.TIF_TAG$description]]
}


# vrt ####

# session cache: normalized "<path>::<page>" -> main VRT filepath.
# VRTs live under tempdir() and are rebuilt once per session.
.tif_vrt_cache <- new.env(parent = emptyenv())

#' @title Build a VRT over the JPEG-2000 tiles of a tif page
#' @name .tif_vrt
#' @description
#' Write a GDAL VRT that mosaics the JPEG-2000 tiles of one page of a tif,
#' addressing each tile through `/vsisubfile/`. Any SubIFD pyramid levels are
#' written as sibling VRTs and referenced as overviews. The result is a file
#' `terra::rast()` can open.
#'
#' Returns `NULL` when the page is not something this can handle, so callers
#' can fall through to another strategy.
#' @param path character. Filepath to the tif
#' @param page numeric. 1-based page index
#' @param pyramid logical. Whether to wire SubIFD levels in as overviews
#' @returns character filepath to the VRT, or NULL
#' @keywords internal
#' @noRd
.tif_vrt <- function(path, page = 1L, pyramid = TRUE) {
    path <- normalizePath(path, mustWork = TRUE)
    key <- sprintf("%s::%d", path, page)

    # reuse this session's VRT when it is still on disk
    hit <- .tif_vrt_cache[[key]]
    if (!is.null(hit) && file.exists(hit)) {
        return(hit)
    }

    offs <- .tif_page_offsets(path, max_pages = page)
    if (page > length(offs)) {
        return(NULL)
    }
    tags <- .tif_read_ifd(path, ifd_offset = offs[[page]])

    if (!.tif_is_jp2k_tiled(tags)) {
        return(NULL)
    }
    dtype <- .tif_gdal_datatype(tags)
    if (is.null(dtype)) {
        return(NULL)
    }
    dir <- file.path(tempdir(), "giotto_tif_vrt", .tif_vrt_id(key))
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    stem <- sub("(?i)\\.ome\\.tif{1,2}$|(?i)\\.tif{1,2}$", "",
        basename(path),
        perl = TRUE
    )

    ovs <- character(0)
    subifds <- tags[[.TIF_TAG$subifds]]
    if (isTRUE(pyramid) && !is.null(subifds)) {
        ovs <- vapply(seq_along(subifds), function(j) {
            lvl <- .tif_read_ifd(path, ifd_offset = subifds[[j]])
            out <- file.path(dir, sprintf("%s_ov%d.vrt", stem, j))
            .tif_write_level_vrt(path, lvl, dtype, out)
            basename(out)
        }, character(1L))
    }

    main <- file.path(dir, sprintf("%s.vrt", stem))
    .tif_write_level_vrt(path, tags, dtype, main, overviews = ovs)

    assign(key, main, envir = .tif_vrt_cache)
    main
}

# short, filesystem-safe id for a cache key. Not a cryptographic hash --
# it only has to keep same-named files from different directories apart
# within one session. Modulus stays below 2^31 so every intermediate is
# exactly representable as a double.
.tif_vrt_id <- function(key) {
    m <- 2147483647 # 2^31 - 1
    h <- 0
    for (i in utf8ToInt(key)) {
        h <- (h * 31 + i) %% m
    }
    sprintf("%08x", h)
}

# write one VRT covering a single IFD (full resolution or a pyramid level)
.tif_write_level_vrt <- function(path, tags, dtype, out,
    overviews = character(0)) {
    w <- tags[[.TIF_TAG$width]]
    h <- tags[[.TIF_TAG$height]]
    tw <- tags[[.TIF_TAG$tile_width]]
    th <- tags[[.TIF_TAG$tile_height]]
    offs <- tags[[.TIF_TAG$tile_offsets]]
    lens <- tags[[.TIF_TAG$tile_counts]]
    nx <- ceiling(w / tw)

    k <- seq_along(offs)
    row <- (k - 1L) %/% nx
    col <- (k - 1L) %% nx
    dx <- col * tw
    dy <- row * th
    # right and bottom edge tiles are padded in the file; clip to the image
    sw <- pmin(tw, w - dx)
    sh <- pmin(th, h - dy)

    spp <- tags[[.TIF_TAG$samples]]
    spp <- if (is.null(spp)) 1L else as.integer(spp[[1L]])

    offs_c <- format(offs, scientific = FALSE)
    lens_c <- format(lens, scientific = FALSE)

    # One VRT band per sample. Interleaved (chunky) tiles decode to all of
    # their samples at once, so every band cites the same byte ranges and
    # selects its own SourceBand out of them.
    band_xml <- function(b) {
        src <- sprintf(paste0(
            "<SimpleSource>",
            "<SourceFilename relativeToVRT=\"0\">",
            "/vsisubfile/%s_%s,%s",
            "</SourceFilename>",
            "<SourceBand>%d</SourceBand>",
            "<SrcRect xOff=\"0\" yOff=\"0\" ",
            "xSize=\"%.0f\" ySize=\"%.0f\"/>",
            "<DstRect xOff=\"%.0f\" yOff=\"%.0f\" ",
            "xSize=\"%.0f\" ySize=\"%.0f\"/>",
            "</SimpleSource>"
        ), offs_c, lens_c, path, b, sw, sh, dx, dy, sw, sh)

        ov <- if (length(overviews)) {
            sprintf(paste0(
                "<Overview>",
                "<SourceFilename relativeToVRT=\"1\">%s</SourceFilename>",
                "<SourceBand>%d</SourceBand>",
                "</Overview>"
            ), overviews, b)
        } else {
            character(0)
        }

        c(
            sprintf("<VRTRasterBand dataType=\"%s\" band=\"%d\">", dtype, b),
            src, ov, "</VRTRasterBand>"
        )
    }

    writeLines(c(
        sprintf("<VRTDataset rasterXSize=\"%.0f\" rasterYSize=\"%.0f\">", w, h),
        unlist(lapply(seq_len(spp), band_xml)),
        "</VRTDataset>"
    ), out)

    invisible(out)
}


# converters ####


#' @title Convert Specialized TIF Formats to Basic TIF
#' @name to_simple_tif
#' @description
#' Simple converter from specialized formats to .tif format. Utilizes the python
#' \pkg{tifffile} package. Performs image conversions one page at a time.
#' Wrap this in a for loop or lapply for more than one image or page. Used
#' when image formats are unsupported by terra. This is implementation may
#' change in the future. Currently tested to work with `.ome.tif` and `qptiff`
#' @param input_file character. Filepath to tif to convert
#' @param output_dir character. Output directory (default: "<dir>/tif_exports")
#' @param page integer or NULL. 1-based page index; NULL means first page,
#' a "_%04d" formatted suffix will be added to the output filename.
#' @param overwrite logical. Default = FALSE. Whether to overwrite if the
#' filename already exists.
#' @returns returns the written filepath invisibly
#' @family tif utility functions
#' @export
to_simple_tif <- function(input_file,
    output_dir = file.path(dirname(input_file), "tif_exports"),
    page = NULL,
    overwrite = FALSE) {

    # get tifffile py
    package_check(
        pkg_name = c("tifffile", "imagecodecs"),
        repository = c("pip:tifffile", "pip:imagecodecs")
    )

    py_tif_convert_path <- system.file(
        "python", "tif_convert.py",
        package = "GiottoClass"
    )
    reticulate::source_python(py_tif_convert_path)
    # ensure output directory exists
    if (!checkmate::test_directory_exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
    }

    # tif page
    # Page handling: omitted or NULL -> first page
    if (is.null(page)) {
        page_1based <- 1L
    } else {
        checkmate::assert_integerish(
            page, lower = 1,
            any.missing = FALSE, len = 1)
        page_1based <- as.integer(page)
    }
    page_0based <- page_1based - 1L
    fname_page  <- sprintf("_page%04d", page_1based)

    # Case-insensitive extension handling (.ome.tif/.ome.tiff/.qptiff/.tif/.tiff)
    in_base <- basename(input_file)
    if (grepl("(?i)\\.ome\\.tif{1,2}$", in_base, perl = TRUE)) {
        fext_pattern <- "(?i)\\.ome\\.tif{1,2}$"
    } else if (grepl("(?i)\\.qptiff$", in_base, perl = TRUE)) {
        fext_pattern <- "(?i)\\.qptiff$"
    } else if (grepl("(?i)\\.tif{1,2}$", in_base, perl = TRUE)) {
        fext_pattern <- "(?i)\\.tif{1,2}$"
    } else {
        stop("Unrecognized TIF extension: ", in_base, call. = FALSE)
    }

    # Output filename
    fname   <- sub(fext_pattern, "", in_base, perl = TRUE)
    outpath <- file.path(output_dir, paste0(fname, fname_page, ".tif"))

    # handle overwrites
    if (file.exists(outpath)) {
        if (isTRUE(overwrite)) {
            unlink(outpath, force = TRUE) # if overwrite, delete original
        } else {
            stop("File already exists: ", outpath,
                "\nSet overwrite = TRUE to replace.\n",
                call. = FALSE
            )
        }
    }
    # Convert (Python expects 0-based page)
    py_tif_convert(input_file = input_file, output_file = outpath, page = page_0based)

    invisible(outpath)
}

#' @describeIn to_simple_tif deprecated.
#' @export
ometif_to_tif <- to_simple_tif




#' @name tif_metadata
#' @title Read Metadata of a Specialized tif
#' @description Read the XML metadata of a .tif file and pull specific nodes
#' out of it. The XML is taken from the `ImageDescription` tag, which needs no
#' image decoding, so OME-TIFF and qptiff are handled without python. The R
#' package \{xml2\} is used to query the document. Formats that keep their
#' metadata in private binary tags instead (lsm, fluoview, nih, micromanager)
#' still fall back to the python package \pkg{tifffile}.
#' @param path character. filepath to tif image
#' @param node character vector. Specific xml node to get. More terms can be
#' added to get a node from a specific hierarchy.
#' @param page numeric or NULL. Specific page to get metadata from. Currently only used
#' for `.qptiff`.
#' @param type character. Type of data to extract. Only affects
#' `output = data.frame` (Matches to one of "attribute", "text", "double",
#' "integer"). `output = "structure"` can help
#' with figuring out which is most appropriate.
#' @param output character. One of "data.frame" to return a data.frame of the
#' attributes information of the xml node, "xml" for an \{xml2\} representation
#' of the node, "list" for an R native list (note that many items in the
#' list may have overlapping names that make indexing difficult),
#' "structure" to invisibly return NULL, but print the structure of the XML
#' document/node, or "kv" (extract key/value pairs from OME MapAnnotations).
#' @returns list/data.frame/XML depending on `output`
#' @examples
#' if (FALSE) {
#' # check structure of metadata
#' tif_metadata("path/to/ometif", output = "structure")
#'
#' # xenium morphology ometif - find channels/biomarkers
#' tif_metadata("path/to/ometif", node = "Channel")
#'
#' # phenocycler qptiff - find channels/biomarkers
#' tif_metadata("path/to/qptiff",
#'     page = NULL,
#'     node = "Biomarker",
#'     type = "text"
#' )
#' }
#' @family tif utility functions
#' @export
tif_metadata <- function(path,
    node = NULL,
    page = NULL,
    type = c("attribute", "text", "double", "integer"),
    output = c("data.frame", "xml", "list", "structure", "kv")) {
    checkmate::assert_file_exists(path)
    package_check(pkg_name = "xml2", repository = "CRAN:xml2")
    output <- match.arg(output,
        choices = c("data.frame", "xml", "list", "structure", "kv")
    )
    type <- match.arg(type,
        choices = c("attribute", "text", "double", "integer")
    )

    # the XML lives in the ImageDescription tag, which is readable without
    # decoding any pixels and so without python.
    xml <- .tif_metadata_xml(path, page = page)

    if (!is.null(xml)) {
        res <- lapply(xml, .tif_metadata_parse,
            node = node, type = type, output = output
        )
        if (length(res) == 1L) {
            return(res[[1L]])
        }
        if (inherits(res[[1L]], "data.frame")) {
            return(Reduce(rbind, res))
        }
        return(res)
    }

    # formats whose metadata is not in ImageDescription (lsm, fluoview, nih,
    # micromanager, ...) still need tifffile to parse their private tags.
    package_check(
        pkg_name   = c("tifffile", "imagecodecs"),
        repository = c("pip:tifffile", "pip:imagecodecs")
    )
    TIF <- reticulate::import("tifffile", convert = TRUE, delay_load = TRUE)
    reticulate::import("imagecodecs", delay_load = TRUE)
    img <- TIF$TiffFile(path)
    on.exit(try(img$close(), silent = TRUE), add = TRUE)

    .tif_metadata_extract(
        img = img,
        node = node,
        page = page,
        type = type,
        output = output
    )
}

#' @title Read the XML metadata string(s) of a tif without python
#' @name .tif_metadata_xml
#' @description
#' Pull the ImageDescription tag from the requested pages. OME-TIFFs carry
#' the whole OME-XML document on the first page; qptiffs carry a separate
#' block per page. Returns `NULL` when no page holds anything XML-like, which
#' is the signal to fall back to tifffile.
#' @param path character. Filepath to the tif
#' @param page numeric or NULL. 1-based page indices. `NULL` means all pages
#' for a qptiff and the first page for anything else.
#' @returns list of character scalars, or NULL
#' @keywords internal
#' @noRd
.tif_metadata_xml <- function(path, page = NULL) {
    offs <- try(.tif_page_offsets(path), silent = TRUE)
    if (inherits(offs, "try-error") || !length(offs)) {
        return(NULL)
    }

    first <- .tif_read_ifd(path, ifd_offset = offs[[1L]])
    desc1 <- first[[.TIF_TAG$description]]
    if (is.null(desc1) || !nzchar(desc1)) {
        return(NULL)
    }
    if (!grepl("^\\s*<", desc1)) {
        return(NULL) # e.g. an ImageJ "key=value" block
    }

    # an OME document on page 1 describes the whole file
    is_ome <- grepl("<OME", desc1, fixed = TRUE)
    if (is_ome) {
        return(list(desc1))
    }

    # otherwise metadata is per page (qptiff)
    if (is.null(page)) page <- seq_along(offs)
    page <- page[!is.na(page) & page >= 1]
    if (!length(page)) {
        stop("No valid page indices after filtering.", call. = FALSE)
    }
    if (any(page > length(offs))) {
        oob <- page[page > length(offs)]
        warning(sprintf("pages %s do not exist", paste(oob, collapse = ", ")),
            call. = FALSE
        )
        page <- page[page <= length(offs)]
        if (!length(page)) {
            stop("No valid page indices after filtering.", call. = FALSE)
        }
    }

    out <- lapply(page, function(p) {
        tg <- .tif_read_ifd(path, ifd_offset = offs[[p]])
        tg[[.TIF_TAG$description]]
    })
    keep <- vapply(out, function(x) !is.null(x) && nzchar(x), logical(1L))
    if (!any(keep)) {
        return(NULL)
    }
    out[keep]
}

#' @describeIn tif_metadata deprecated.
#' @export
ometif_metadata <- tif_metadata


.tif_metadata_extract <- function(img, node, page = NULL, type, output) {
    npages <- tryCatch(length(img$pages), error = function(e) NA_integer_)
    if (is.na(npages)) {
        npages <- length(img$series[[1L]]$pages)
    }
    # ensure pages are in subscript bounds
    if (is.null(page)) page <- seq_len(npages)
    page <- page[!is.na(page) & page >= 1]
    if (length(page) == 0L) {
        stop("No valid page indices after filtering.", call. = FALSE)
    }
    if (any(page > npages)) {
        oob <- page[page > npages]
        warning(sprintf("pages %s do not exist", paste(oob, collapse = ", ")), call. = FALSE)
        page <- page[page <= npages]
        if (length(page) == 0L) {
            stop("No valid page indices after filtering.", call. = FALSE)
        }
    }
    # if multiple pages, lapply recurse
    if (length(page) > 1L && isTRUE(img$is_qpi)) {
        reslist <- lapply(page, function(p) {
            .tif_metadata_extract(
                img = img,
                node = node,
                page = p,
                type = type,
                output = output
            )
        })
        if (length(reslist) && inherits(reslist[[1]], "data.frame")) {
            reslist <- Reduce(rbind, reslist)
        }
        return(reslist)
    }



    # Select XML text source
    if (isTRUE(img$is_ome)) {
        x <- img$ome_metadata
        if (is.null(x) || !nzchar(x)) {
            p1 <- as.integer(page)[1]
            x  <- tryCatch(img$pages[[p1 - 1L]]$description, error = function(e) NULL)
        }
    } else if (isTRUE(img$is_qpi)) {
        # qptiff: per-page description
        p1 <- as.integer(page)[1]
        x  <- tryCatch(img$pages[[p1 - 1L]]$description, error = function(e) NULL)
        if (is.null(x) || !nzchar(x)) {
            x <- tryCatch(img$series[[1]]$pages[[p1 - 1L]]$description, error = function(e) NULL)
        }
    } else if (img$is_fluoview) x <- img$fluoview_metadata
    else if (img$is_nih) x <- img$nih_metadata
    else if (img$is_astrotiff) x <- img$astrotiff_metadata
    else if (img$is_imagej) x <- img$imagej_metadata
    else if (img$is_lsm) x <- img$lsm_metadata
    else if (img$is_micromanager) x <- img$micromanager_metadata
    else stop("unrecognized tif format\n", call. = FALSE)

    if (is.null(x) || !nzchar(x)) {
        stop("No XML metadata found in file (empty description/OME-XML).", call. = FALSE)
    }

    .tif_metadata_parse(x = x, node = node, type = type, output = output)
}

# shared xml2 half: turn an XML string into the requested output.
# reached from both the pure-R and the python metadata paths.
.tif_metadata_parse <- function(x, node, type, output) {
    node_parts <- node
    x <- xml2::read_xml(x)
    ns <- xml2::xml_ns(x)
    has_namespace <- length(ns) > 0L

    ## NEW: output = "kv" (read OME MapAnnotation K/Vs) ---
    if (identical(output, "kv")) {
        m_nodes <- xml2::xml_find_all(
            x,
            ".//*[local-name()='StructuredAnnotations']//*[local-name()='MapAnnotation']/*[local-name()='Value']//*[local-name()='M']"
        )
        if (!length(m_nodes)) return(NULL)
        keys   <- xml2::xml_attr(m_nodes, "K")
        values <- xml2::xml_text(m_nodes)
        out <- as.list(values)
        names(out) <- keys
        return(out)
    }

    if (!is.null(node)) {
        node_parts <- node
        node_path  <- paste(node_parts, collapse = "/")
        if (has_namespace) {
            x_try <- xml2::xml_find_all(
                x, sprintf("//d1:%s", node_path),
                ns = ns
            )
        } else {
            x_try <- xml2::xml_find_all(
                x, sprintf("//%s", node_path)
            )
        }

        # 2) if nothing found, retry with a namespace-agnostic XPath
        if (length(x_try) == 0L) {
            ln <- paste(sprintf("*[local-name()='%s']", node_parts), collapse = "/")
            x  <- xml2::xml_find_all(x, paste0("//", ln))
        } else {
            x <- x_try
        }
    }
    # choose a single column label for scalar returns
    node_label <- if (is.null(node)) "value" else tail(node_parts, 1)

    switch(output,
        "data.frame" = {
            switch(type,
                "attribute" = {
                    attrs <- xml2::xml_attrs(x)
                    if (!length(attrs)) return(data.frame())
                    x <- Reduce("rbind", attrs)
                    # a single matching node reduces to a bare named vector,
                    # which would transpose into a one-column frame
                    if (is.null(dim(x))) {
                        x <- matrix(x,
                            nrow = 1L,
                            dimnames = list(NULL, names(attrs[[1L]]))
                        )
                    }
                    rownames(x) <- NULL
                    return(as.data.frame(x, stringsAsFactors = FALSE))
                },
                "text" = {
                    x <- (as.data.frame(xml2::xml_text(x)))
                    colnames(x) <- node_label
                    return(x)
                },
                "double" = {
                    x <- (as.data.frame(xml2::xml_double(x)))
                    colnames(x) <- node_label
                    return(x)
                },
                "integer" = {
                    x <- (as.data.frame(xml2::xml_integer(x)))
                    colnames(x) <- node_label
                    return(x)
                }
            )
        },
        "xml" = return(x),
        "list" = return(xml2::as_list(x)),
        "structure" = {
            xml2::xml_structure(x)
            return(invisible())
        }
    )
}
