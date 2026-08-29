# Generate the JPEG-2000 test fixtures in inst/extdata/.
#
# No vendor-shipped file could serve as one: the cropped Xenium mini dataset
# was re-encoded to Deflate by its crop tool, so without these the whole
# `.tif_vrt()` path would pass CI while broken. Each fixture lifts a couple of
# real JPEG-2000 tiles out of a source image and wraps them in a hand-written
# BigTIFF directory. No pixel data is decoded or re-encoded.
#
# Usage:
#   Rscript inst/scripts/make_toy_jp2k.R <xenium.ome.tif> [<aperio.svs>]
#
#   toy_jp2k.ome.tif      single-sample, compression 34712 (JP2 boxes),
#                         plus one SubIFD pyramid level and OME-XML.
#                         Source: any 10x Xenium morphology_focus_*.ome.tif
#   toy_jp2k_rgb.tif      3-sample interleaved, compression 33005 (bare J2K
#                         codestreams). Source: an Aperio SVS. Only written
#                         when a second argument is given.

suppressMessages(source(file.path("R", "tif.R")))

args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) stop("need a source Xenium ome.tif", call. = FALSE)
outdir <- file.path("inst", "extdata")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

OME_XML <- paste0(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<OME xmlns="http://www.openmicroscopy.org/Schemas/OME/2016-06">',
    '<Image ID="Image:0" Name="toy_jp2k">',
    '<Pixels ID="Pixels:0" DimensionOrder="XYZCT" Type="uint16" ',
    'SizeX="2048" SizeY="1024" SizeZ="1" SizeC="1" SizeT="1" ',
    'PhysicalSizeX="0.2125" PhysicalSizeXUnit="µm" ',
    'PhysicalSizeY="0.2125" PhysicalSizeYUnit="µm">',
    '<Channel ID="Channel:0" Name="DAPI" SamplesPerPixel="1"/>',
    "</Pixels></Image>",
    "<StructuredAnnotations>",
    '<MapAnnotation ID="Annotation:0"><Value>',
    '<M K="Channel">DAPI</M><M K="Purpose">Nuclear</M>',
    "</Value></MapAnnotation>",
    "</StructuredAnnotations></OME>"
)

# ---- helpers --------------------------------------------------------------

grab <- function(path, offset, len) {
    con <- file(path, "rb")
    on.exit(close(con))
    seek(con, offset)
    readBin(con, "raw", len)
}

# pick the n smallest tiles above a floor, so the fixture stays small but the
# tiles still carry signal
pick_tiles <- function(path, tags, n, floor = 5000) {
    counts <- tags[[.TIF_TAG$tile_counts]]
    offs <- tags[[.TIF_TAG$tile_offsets]]
    i <- order(counts)
    i <- i[counts[i] > floor][seq_len(n)]
    lapply(i, function(k) grab(path, offs[k], counts[k]))
}

u16 <- function(x) writeBin(as.integer(x), raw(), size = 2L, endian = "little")
u32 <- function(x) writeBin(as.integer(x), raw(), size = 4L, endian = "little")
u64 <- function(x) {
    v <- as.numeric(x)
    as.raw(vapply(0:7, function(i) (v %/% 256^i) %% 256, numeric(1)))
}

ENTRY <- 20L # BigTIFF directory entry
HEADER <- 16L

# one BigTIFF entry; `payload` must be exactly 8 bytes
entry <- function(tag, type, count, payload) {
    c(u16(tag), u16(type), u64(count), payload)
}
# pad a value into the 8-byte inline slot
inline <- function(bytes) {
    out <- raw(8)
    out[seq_along(bytes)] <- bytes
    out
}

#' Write a minimal tiled BigTIFF whose tiles are copied verbatim.
#'
#' @param tiles    list of raw vectors, left-to-right then top-to-bottom
#' @param ov_tile  optional raw vector for a single-tile SubIFD level
write_toy <- function(out, tiles, w, h, tilew, tileh, bits, spp, photometric,
    compression, xml = NULL, ov_tile = NULL, ov_w = NULL, ov_h = NULL) {

    pos <- HEADER
    tile_pos <- numeric(0)
    for (t in tiles) {
        tile_pos <- c(tile_pos, pos)
        pos <- pos + length(t)
    }
    ov_pos <- pos
    if (!is.null(ov_tile)) pos <- pos + length(ov_tile)

    xml_raw <- if (is.null(xml)) NULL else c(charToRaw(xml), as.raw(0L))
    xml_pos <- pos
    if (!is.null(xml_raw)) pos <- pos + length(xml_raw)

    toff_pos <- pos; pos <- pos + 8L * length(tiles)
    tcnt_pos <- pos; pos <- pos + 8L * length(tiles)

    # BitsPerSample is one SHORT per sample; 3 of them still fit the 8-byte slot
    bits_raw <- inline(unlist(lapply(rep(bits, spp), u16)))

    n_main <- 11L + as.integer(!is.null(xml)) + as.integer(!is.null(ov_tile))
    n_ov <- 11L
    ifd0_pos <- pos
    pos <- pos + 8L + n_main * ENTRY + 8L
    subifd_pos <- pos

    main <- c(
        entry(256, 4, 1, inline(u32(w))),
        entry(257, 4, 1, inline(u32(h))),
        entry(258, 3, spp, bits_raw),
        entry(259, 3, 1, inline(u16(compression))),
        entry(262, 3, 1, inline(u16(photometric))),
        if (!is.null(xml_raw)) entry(270, 2, length(xml_raw), u64(xml_pos)),
        entry(277, 3, 1, inline(u16(spp))),
        entry(284, 3, 1, inline(u16(1))), # PlanarConfiguration: chunky
        entry(322, 4, 1, inline(u32(tilew))),
        entry(323, 4, 1, inline(u32(tileh))),
        entry(324, 16, length(tiles), u64(toff_pos)),
        entry(325, 16, length(tiles), u64(tcnt_pos)),
        if (!is.null(ov_tile)) entry(330, 18, 1, u64(subifd_pos))
    )
    stopifnot(length(main) == n_main * ENTRY)

    ov <- if (!is.null(ov_tile)) c(
        entry(254, 4, 1, inline(u32(1))), # reduced-resolution subfile
        entry(256, 4, 1, inline(u32(ov_w))),
        entry(257, 4, 1, inline(u32(ov_h))),
        entry(258, 3, spp, bits_raw),
        entry(259, 3, 1, inline(u16(compression))),
        entry(262, 3, 1, inline(u16(photometric))),
        entry(277, 3, 1, inline(u16(spp))),
        entry(322, 4, 1, inline(u32(tilew))),
        entry(323, 4, 1, inline(u32(tileh))),
        entry(324, 16, 1, u64(ov_pos)),
        entry(325, 16, 1, u64(length(ov_tile)))
    ) else NULL
    if (!is.null(ov)) stopifnot(length(ov) == n_ov * ENTRY)

    con <- file(out, "wb")
    on.exit(close(con))
    writeBin(c(charToRaw("II"), u16(43), u16(8), u16(0), u64(ifd0_pos)), con)
    for (t in tiles) writeBin(t, con)
    if (!is.null(ov_tile)) writeBin(ov_tile, con)
    if (!is.null(xml_raw)) writeBin(xml_raw, con)
    for (o in tile_pos) writeBin(u64(o), con)
    for (t in tiles) writeBin(u64(length(t)), con)
    writeBin(c(u64(n_main), main, u64(0)), con)
    if (!is.null(ov)) writeBin(c(u64(n_ov), ov, u64(0)), con)

    message(sprintf("wrote %s (%.0f KB)", out, file.size(out) / 1024))
}

# ---- 1. single-sample, JP2 boxes, with a pyramid level --------------------

src <- args[1]
main <- .tif_read_ifd(src)
stopifnot(.tif_is_jp2k_tiled(main))
tiles <- pick_tiles(src, main, 2, floor = 50000)
lvl <- .tif_read_ifd(src, ifd_offset = main[[.TIF_TAG$subifds]][[1L]])
ov_tile <- pick_tiles(src, lvl, 1, floor = 50000)[[1L]]

write_toy(
    out = file.path(outdir, "toy_jp2k.ome.tif"),
    tiles = tiles, w = 2048, h = 1024, tilew = 1024, tileh = 1024,
    bits = 16, spp = 1, photometric = 1, compression = 34712,
    xml = OME_XML, ov_tile = ov_tile, ov_w = 1024, ov_h = 512
)

# ---- 2. 3-sample interleaved, bare J2K codestreams ------------------------

if (length(args) > 1) {
    svs <- args[2]
    # page 5 of an Aperio pyramid is small and still tiled at 240x240
    pg <- .tif_read_ifd(svs, ifd_offset = .tif_page_offsets(svs)[5L])
    stopifnot(.tif_is_jp2k_tiled(pg), pg[[.TIF_TAG$samples]][[1L]] == 3)
    rgb_tiles <- pick_tiles(svs, pg, 2, floor = 5000)

    write_toy(
        out = file.path(outdir, "toy_jp2k_rgb.tif"),
        tiles = rgb_tiles, w = 480, h = 240, tilew = 240, tileh = 240,
        bits = 8, spp = 3, photometric = 2,
        compression = pg[[.TIF_TAG$compression]][[1L]]
    )
}
