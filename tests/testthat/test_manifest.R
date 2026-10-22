# Manifest generation against a real giotto object. The pure diff logic is
# covered separately in test_manifest_diff.R.

g <- GiottoData::loadGiottoMini("visium", verbose = FALSE)

# the one record in `slot` whose key fields match `...`
.rec <- function(m, slot, ...) {
    keys <- list(...)
    hit <- Filter(function(r) {
        all(vapply(names(keys), function(k) identical(r$keys[[k]], keys[[k]]),
            logical(1L)))
    }, m$slots[[slot]])
    if (length(hit) != 1L) stop("expected one record, found ", length(hit))
    hit[[1L]]
}

# values of one key field across a slot's records
.keyvals <- function(m, slot, field) {
    unlist(lapply(m$slots[[slot]], function(r) r$keys[[field]]))
}

.raw <- function(m) {
    .rec(m, "expression", spat_unit = "cell", feat_type = "rna", name = "raw")
}

describe("objManifest", {
    m <- objManifest(g)

    it("emits the documented top-level shape", {
        expect_s3_class(m, "gmanifest")
        expect_identical(m$schema_version, "0.2.0")
        expect_true(all(c("object", "summary", "slots", "warnings") %in%
            names(m)))
        expect_match(m$generated_by, "^GiottoClass ")
    })

    it("lists one record per subobject, keyed by accessor arguments", {
        expect_true("expression" %in% names(m$slots))
        expect_null(names(m$slots$expression))
        expect_setequal(
            .keyvals(m, "expression", "name"), c("raw", "normalized", "scaled")
        )
        expect_identical(.raw(m)$class, "exprObj")
    })

    it("carries the extra keys a dim reduction is retrieved by", {
        pca <- .rec(m, "dimension_reduction", name = "pca")
        expect_identical(pca$keys$reduction, "cells")
        expect_identical(pca$keys$reduction_method, "pca")
        # identity lives in keys and nowhere else
        expect_null(pca$name)
        expect_null(pca$spat_unit)
    })

    it("gives polygons and points only the key their getter takes", {
        gv <- GiottoData::loadGiottoMini("vizgen", verbose = FALSE)
        mv <- objManifest(gv)
        poly <- mv$slots$spatial_info[[1]]
        pts <- mv$slots$feat_info[[1]]
        expect_named(poly$keys, "name")
        expect_named(pts$keys, "feat_type")
        expect_s4_class(
            do.call(getPolygonInfo, c(list(gv), poly$keys,
                list(return_giottoPolygon = TRUE))),
            "giottoPolygon"
        )
        expect_s4_class(
            do.call(getFeatureInfo, c(list(gv), pts$keys,
                list(return_giottoPoints = TRUE))),
            "giottoPoints"
        )
    })

    it("maps a record's keys straight onto a getter call", {
        pca <- .rec(m, "dimension_reduction", name = "pca")
        got <- do.call(getDimReduction,
            c(list(g), pca$keys, list(output = "dimObj")))
        expect_s4_class(got, "dimObj")
        expect_identical(objName(got), "pca")
    })

    it("carries no shape or count fields on its records", {
        # sizes cost a scan on disk-backed payloads; the counts that matter
        # are in the summary block
        expect_null(.raw(m)$shape)
        expect_identical(m$summary$n_cells$cell, length(spatIDs(g)))
    })

    it("fingerprints ids rather than listing them", {
        ids <- .rec(objManifest(g), "cell_ID",
            spat_unit = "cell")
        expect_type(ids$fingerprint, "character")
        expect_null(ids$ids)
    })

    it("names a dim reduction's components", {
        pca <- .rec(m, "dimension_reduction", name = "pca")
        expect_identical(pca$columns,
            colnames(g@dimension_reduction$cells$cell$rna$pca$pca@coordinates))
    })

    it("counts metadata levels only when asked, and never the id column", {
        col <- function(mm, nm) {
            cols <- .rec(mm, "cell_metadata", spat_unit = "cell",
                feat_type = "rna")$columns
            cols[[nm]]
        }
        expect_null(col(m, "leiden_clus")$n_levels)
        expect_type(col(m, "leiden_clus")$fingerprint, "character")

        ml <- objManifest(g, levels = TRUE)
        expect_identical(
            col(ml, "leiden_clus")$n_levels,
            length(unique(pDataDT(g)$leiden_clus))
        )
        expect_null(col(ml, "cell_ID")$n_levels)
    })

    it("does not read a manifest with level counts as a change", {
        d <- manifestDiff(m, objManifest(g, levels = TRUE))
        expect_false(d$changed)
        expect_identical(d$summary, "no state change")
    })

    it("sorts slots and records so output is canonical", {
        expect_identical(names(m$slots), sort(names(m$slots)))
        ids <- vapply(m$slots$expression, function(r) {
            .manifest_item_id("expression", r)
        }, character(1L))
        expect_identical(ids, sort(ids))
    })

    it("prints each slot's records by their keys, without content", {
        out <- capture.output(print(m))
        expect_true("  expression" %in% out)
        expect_true("    cell / rna / raw" %in% out)
        # a polygon set's spat_unit and name are the same value
        expect_true("    cell" %in% out)
        expect_false("    cell / cell" %in% out)
        expect_false(any(grepl(.raw(objManifest(g))$fingerprint, out)))
    })

    it("reads a clean object without unreadable fields", {
        expect_identical(m$warnings, character(0))
    })

    it("is insensitive to the insertion order of a slot's contents", {
        g2 <- g
        g2@expression$cell$rna <- rev(g2@expression$cell$rna)
        expect_identical(objManifest(g2)$slots, m$slots)
    })
})

describe("objManifest accessor guarding", {
    it("degrades one field to NULL and records its path", {
        testthat::local_mocked_bindings(
            .manifest_columns = function(...) stop("accessor exploded")
        )
        m <- objManifest(g)
        meta <- .rec(m, "cell_metadata", spat_unit = "cell", feat_type = "rna")
        expect_null(meta$columns)
        expect_true("cell_metadata.cell.rna.columns" %in% m$warnings)
        # and the rest of the manifest is intact
        expect_identical(.raw(m)$class, "exprObj")
    })
})

describe("manifest fingerprints", {
    it("are always present", {
        expect_type(.raw(objManifest(g))$fingerprint, "character")
    })

    it("are stable across repeated calls", {
        expect_identical(
            objManifest(g)$slots,
            objManifest(g)$slots
        )
    })

    it("change when content changes but the shape does not", {
        before <- objManifest(g)
        g2 <- g
        e <- g2@expression$cell$rna$raw
        e@exprMat[1, 1] <- e@exprMat[1, 1] + 1000
        g2@expression$cell$rna$raw <- e
        after <- objManifest(g2)

        expect_false(identical(
            .raw(before)$fingerprint, .raw(after)$fingerprint
        ))
        expect_match(manifestDiff(before, after)$summary, "fingerprint")
    })

    it("do not disturb the RNG", {
        set.seed(42)
        target <- runif(3)
        set.seed(42)
        invisible(objManifest(g))
        expect_identical(runif(3), target)
    })
})

describe("objManifest_json", {
    it("round-trips through JSON", {
        j <- objManifest_json(g)
        back <- jsonlite::fromJSON(j, simplifyVector = FALSE)
        expect_identical(back$schema_version, "0.2.0")
        expect_identical(.raw(back)$class, "exprObj")
    })

    it("encodes NA, NaN and Inf as strings rather than null", {
        prepped <- .manifest_json_prep(list(
            a = c(1, NA, NaN, Inf, -Inf), b = NA_character_, c = 1.5
        ))
        expect_identical(prepped$a, c("1", "NA", "NaN", "Inf", "-Inf"))
        expect_identical(prepped$b, "NA")
        expect_identical(prepped$c, 1.5)
    })

    it("writes list-valued fields as arrays at any length", {
        # auto_unbox turned a length-1 vector into a bare value, so these
        # changed shape with the data: "cell" here, ["z0", "z1"] there
        arrays <- function(node, out = list()) {
            if (!is.list(node)) return(out)
            for (nm in names(node)) {
                if (nm %in% GiottoClass:::.MANIFEST_ARRAY_FIELDS &&
                    !is.null(node[[nm]])) {
                    out[[length(out) + 1L]] <- list(nm, node[[nm]])
                }
            }
            for (child in node) out <- arrays(child, out)
            out
        }
        gv <- GiottoData::loadGiottoMini("vizgen", verbose = FALSE)
        gv <- spatShift(gv, dx = 1, space = "s")
        for (obj in list(g, gv)) {
            back <- jsonlite::fromJSON(objManifest_json(obj),
                simplifyVector = FALSE)
            found <- arrays(back)
            expect_gt(length(found), 0L)
            for (f in found) {
                # a metadata record's columns is an object keyed by name
                expect_true(is.list(f[[2]]), info = f[[1]])
                if (identical(f[[1]], "columns") && !is.null(names(f[[2]]))) {
                    next
                }
                expect_null(names(f[[2]]), info = f[[1]])
            }
        }
        # the single-valued cases that used to unbox
        back <- jsonlite::fromJSON(objManifest_json(g), simplifyVector = FALSE)
        expect_identical(back$summary$spat_units, list("cell"))
        expect_identical(back$warnings, list())
    })

    it("validates against the shipped schema's required fields", {
        schema_path <- system.file(
            "schema", "giotto-manifest-0.2.0.json",
            package = "GiottoClass"
        )
        skip_if(!nzchar(schema_path))
        schema <- jsonlite::fromJSON(schema_path, simplifyVector = FALSE)
        m <- jsonlite::fromJSON(objManifest_json(g), simplifyVector = FALSE)
        expect_true(all(unlist(schema$required) %in% names(m)))
        expect_identical(m$schema_version, schema$properties$schema_version$const)
    })
})

describe("object uid", {
    it("is minted at creation and is unique per object", {
        expect_false(is.null(.gobject_uid(g)))
        expect_false(identical(.gobject_uid(giotto()), .gobject_uid(giotto())))
    })

    it("survives a copy", {
        g2 <- g
        expect_identical(.gobject_uid(g2), .gobject_uid(g))
    })
})

describe("object source", {
    it("is null for an in-memory object, and stays a key in the JSON", {
        expect_null(objManifest(g)$object$source)
        back <- jsonlite::fromJSON(objManifest_json(g), simplifyVector = FALSE)
        expect_true("source" %in% names(back$object))
        expect_null(back$object$source)
    })

    it("reports the class of a backing source", {
        gb <- g
        gb@source <- structure(list(), class = c("fakeSource", "list"))
        expect_identical(objManifest(gb)$object$source, "fakeSource")
    })
})

describe("fingerprint_data extension point", {
    gs <- g
    e <- gs@expression$cell$rna$raw
    e@exprMat <- structure(list(), class = "fakeStore")
    gs@expression$cell$rna$raw <- e

    it("reports an unrecognised payload as unreadable, not unchanged", {
        m <- objManifest(gs)
        expect_null(.raw(m)$fingerprint)
        expect_true("expression.cell.rna.raw.fingerprint" %in% m$warnings)
    })

    it("uses a method registered for the payload class", {
        setOldClass("fakeStore")
        setMethod("fingerprint_data", "fakeStore", function(x, ...) "abc123")
        on.exit(removeMethod("fingerprint_data", "fakeStore"), add = TRUE)

        m <- objManifest(gs)
        expect_type(.raw(m)$fingerprint, "character")
        expect_false("expression.cell.rna.raw.fingerprint" %in% m$warnings)
    })
})

describe("metadata column fingerprints", {
    it("names the column a one-cell edit touched", {
        # relabelling one cell to an existing cluster moves no level count;
        # only the column's fingerprint can see it
        before <- objManifest(g)
        cm <- getCellMetadata(g, output = "cellMetaObj")
        lv <- unique(cm[]$leiden_clus)
        cm[]$leiden_clus[1L] <- setdiff(lv, cm[]$leiden_clus[1L])[[1L]]
        g2 <- setGiotto(g, cm, verbose = FALSE)
        d <- manifestDiff(before, objManifest(g2))
        expect_match(d$summary, "cell metadata changed: leiden_clus")
        expect_false(grepl("total_expr", d$summary))
    })
})

describe("fingerprint_data methods", {
    it("identifies a sparse matrix by its content, not its object", {
        e <- g@expression$cell$rna$raw@exprMat
        rebuilt <- Matrix::Matrix(as.matrix(e), sparse = TRUE)
        expect_identical(fingerprint_data(e), fingerprint_data(rebuilt))
        e2 <- e
        e2@x[1] <- e2@x[1] + 1
        expect_false(identical(fingerprint_data(e), fingerprint_data(e2)))
    })

    it("ignores a data.table index but not a value", {
        dt <- pDataDT(g)
        d2 <- data.table::copy(dt)
        data.table::setindex(d2, leiden_clus)
        expect_identical(fingerprint_data(dt), fingerprint_data(d2))
        d2[1L, leiden_clus := leiden_clus + 1]
        expect_false(identical(fingerprint_data(dt), fingerprint_data(d2)))
    })

    it("follows the pixels, not the file they sit in", {
        r <- g@images$image@raster_object
        # the same pixels in another file, as when terra spills a result to a
        # temp file. INT2U because 255 is GDAL's default no-data value for an
        # 8-bit TIFF, which would turn this image's alpha band into NaN.
        spilled <- terra::writeRaster(r, tempfile(fileext = ".tif"),
            datatype = "INT2U")
        expect_identical(fingerprint_data(r), fingerprint_data(spilled))
        renamed <- r
        names(renamed) <- paste0("band_", seq_len(terra::nlyr(r)))
        expect_identical(fingerprint_data(r), fingerprint_data(renamed))
        expect_false(identical(fingerprint_data(r), fingerprint_data(r + 1)))
    })

    it("sees a change to an affine image's placement", {
        img <- g@images$image
        a <- spin(img, 30)
        leaf <- function(x) {
            .manifest_leaf(x, wenv = NULL, path = "t")$fingerprint
        }
        expect_type(leaf(a), "character")
        expect_false(identical(leaf(a), leaf(spin(img, 45))))
        expect_false(identical(leaf(a), leaf(img)))
    })

    it("gives two handles on one image file the same fingerprint", {
        r <- g@images$image@raster_object
        src <- terra::sources(r)[[1L]]
        skip_if(!nzchar(src))
        r2 <- suppressWarnings(terra::rast(src))
        expect_identical(fingerprint_data(r), fingerprint_data(r2))
    })

    it("does not identify a list holding something it cannot hash", {
        expect_type(fingerprint_data(list(a = 1, b = list("x", NULL))),
            "character")
        expect_error(fingerprint_data(list(new.env())), "no method")
    })

    it("identifies a function by its code", {
        mk <- function(k) function(x) x * k
        f <- mk(2)
        fp <- fingerprint_data(f)
        for (i in 1:3) f(1) # JIT-compiled after a few calls
        expect_identical(fingerprint_data(f), fp)
        expect_false(identical(fp, fingerprint_data(function(x) x + k)))
        expect_false(identical(fingerprint_data(sum), fingerprint_data(max)))
        # captured values are not part of it
        expect_identical(fingerprint_data(mk(3)), fp)
    })

    it("fingerprints a BPCells matrix from its handle", {
        skip_if_not_installed("BPCells")
        m <- as(g@expression$cell$rna$raw@exprMat, "dgCMatrix")
        d <- tempfile()
        b <- BPCells::write_matrix_dir(m, d, compress = FALSE)
        fp <- fingerprint_data(b)
        expect_type(fp, "character")
        # a copy elsewhere is the same matrix
        d2 <- tempfile()
        dir.create(d2)
        file.copy(list.files(d, full.names = TRUE), d2)
        expect_identical(fingerprint_data(BPCells::open_matrix_dir(d2)), fp)
        # queued operations and other content are not
        expect_false(identical(fingerprint_data(log1p(b)), fp))
        b3 <- BPCells::write_matrix_dir(m[, 1:10], tempfile(), compress = FALSE)
        expect_false(identical(fingerprint_data(b3), fp))
    })

    it("fingerprints an HDF5Array matrix from its handle", {
        skip_if_not_installed("HDF5Array")
        m <- as.matrix(g@expression$cell$rna$raw@exprMat[1:50, 1:20])
        f <- tempfile(fileext = ".h5")
        h <- HDF5Array::writeHDF5Array(m, f, name = "x")
        fp <- fingerprint_data(h)
        f2 <- tempfile(fileext = ".h5")
        file.copy(f, f2)
        expect_identical(fingerprint_data(HDF5Array::HDF5Array(f2, "x")), fp)
        expect_false(identical(fingerprint_data(h[1:5, ]), fp))
    })

    it("has no fingerprint only for an absent payload", {
        expect_null(fingerprint_data(NULL))
        expect_error(fingerprint_data(new.env()), "no method")
    })
})

describe("views and spaces", {
    gr <- subset(g, nr_feats > 300, view = "qc")
    gr <- spatShift(gr, dx = 10, space = "shifted")
    m <- objManifest(gr)

    it("lists each recipe by name with its kind and steps", {
        v <- .rec(m, "view", name = "qc")
        expect_identical(v$class, "giottoView")
        expect_identical(v$n_steps, 1L)
        expect_identical(v$step_types, "filter")
        sp <- .rec(m, "spaces", name = "shifted")
        expect_identical(sp$class, "perSampleSpace")
        expect_identical(sp$step_types, "transform")
    })

    it("reports a recipe added, and a step recorded onto it", {
        d <- manifestDiff(objManifest(g), m)
        expect_match(d$summary, "view added: qc")
        expect_match(d$summary, "space added: shifted")

        gr2 <- spin(gr, 30, space = "shifted")
        d2 <- manifestDiff(m, objManifest(gr2))
        expect_match(d2$summary, "space shifted modified")
        expect_false(grepl("view", d2$summary))
    })
})

describe("history records", {
    it("reads legacy character entries", {
        recs <- ghistory_records(g)
        expect_gt(length(recs), 0L)
        expect_true(all(vapply(recs, function(r) r$status, character(1L))
            == "ok"))
    })

    it("records an unattributed step for an unlogged mutation", {
        before <- objManifest(g)
        g2 <- g
        g2@expression$cell$rna$scaled <- NULL
        d <- manifestDiff(before, objManifest(g2))
        expect_true(d$changed)

        g2 <- recordGiottoStep(
            g2,
            fn = "direct slot assignment", status = "unattributed",
            diff = d$detail
        )
        last <- tail(ghistory_records(g2), 1L)[[1L]]
        expect_identical(last$status, "unattributed")
        expect_identical(last$fn, "direct slot assignment")
        expect_false(is.null(last$diff))
        expect_match(last$timestamp, "^\\d{4}-\\d{2}-\\d{2}T")
    })

    it("records a failed step", {
        g2 <- recordGiottoStep(
            g, fn = "runPCA", status = "error", error = "not enough features"
        )
        last <- tail(ghistory_records(g2), 1L)[[1L]]
        expect_identical(last$status, "error")
        expect_identical(last$error, "not enough features")
    })

    it("serializes as one JSON object per line", {
        txt <- objHistory_ndjson(g)
        lines <- strsplit(txt, "\n")[[1]]
        expect_length(lines, length(objHistory(g)))
        parsed <- jsonlite::fromJSON(lines[[1]], simplifyVector = FALSE)
        expect_true(all(c("step_id", "fn", "params", "status") %in%
            names(parsed)))
    })
})

describe("update_giotto_params structured record", {
    it("carries the call as deparsed expressions", {
        f <- function(gobject, dims_to_use = 1:30, k = 30) {
            update_giotto_params(gobject, description = "_testfn")
        }
        g2 <- f(g)
        last <- tail(ghistory_records(g2), 1L)[[1L]]
        expect_identical(last$status, "ok")
        expect_identical(last$fn, "f")
        # the flattened character form reduces `1:30` to "1"; the structured
        # params keep the expression the user actually wrote
        expect_identical(last$params$dims_to_use, "1:30")
    })

    it("leaves the legacy character entry untouched", {
        f <- function(gobject) update_giotto_params(gobject, "_legacy")
        entry <- tail(objHistory(f(g)), 1L)[[1L]]
        expect_s3_class(entry, "ghistory_item")
        expect_type(unclass(entry), "character")
    })
})

describe("save / load sidecars", {
    td <- file.path(tempdir(), "manifest_sidecars")
    on.exit(unlink(td, recursive = TRUE), add = TRUE)
    dir.create(td, showWarnings = FALSE, recursive = TRUE)
    saveGiotto(g, dir = td, foldername = "obj", verbose = FALSE,
        overwrite = TRUE)
    saved <- file.path(td, "obj")

    it("writes manifest.json and history.ndjson next to the object", {
        expect_true(file.exists(file.path(saved, "manifest.json")))
        expect_true(file.exists(file.path(saved, "history.ndjson")))
    })

    it("writes a manifest that matches the object saved", {
        sc <- jsonlite::fromJSON(
            file.path(saved, "manifest.json"), simplifyVector = FALSE
        )
        expect_identical(sc$object$uid, .gobject_uid(g))
        expect_identical(.raw(sc)$class, "exprObj")
    })

    it("preserves the uid and data fingerprints through a round trip", {
        g2 <- loadGiotto(saved, verbose = FALSE)
        expect_identical(.gobject_uid(g2), .gobject_uid(g))

        a <- objManifest(g)
        b <- objManifest(g2)
        expect_identical(.raw(a)$fingerprint, .raw(b)$fingerprint)
        meta <- function(m) {
            .rec(m, "cell_metadata", spat_unit = "cell", feat_type = "rna")
        }
        expect_identical(meta(a)$fingerprint, meta(b)$fingerprint)
        # saveGiotto exports images to new files; the pixels are unchanged
        expect_identical(
            lapply(a$slots$images, `[[`, "fingerprint"),
            lapply(b$slots$images, `[[`, "fingerprint")
        )
    })
})

describe("network slots", {
    it("describes and fingerprints an igraph-carried network", {
        # spatialNetworkObj/nnNetObj hold an igraph since 0.6.0; objects saved
        # before that hold an edge data.table. Both must describe the same way
        sn <- g@spatial_network$cell$Delaunay_network
        skip_if(is.null(sn))
        m <- objManifest(g)
        leaf <- .rec(m, "spatial_network", name = "Delaunay_network")
        expect_type(leaf$fingerprint, "character")
    })

    it("gives different networks different fingerprints", {
        # hashing an unreadable carrier as NULL made every network hash the
        # same constant, so no network ever appeared to change
        sn <- g@spatial_network$cell$Delaunay_network
        nn <- g@nn_network$cell$rna$sNN$sNN.pca
        skip_if(is.null(sn) || is.null(nn))
        expect_false(identical(
            fingerprint_data(sn@network), fingerprint_data(nn@network)
        ))
    })

    it("returns no fingerprint rather than a constant when content is absent", {
        sn <- g@spatial_network$cell$Delaunay_network
        skip_if(is.null(sn))
        sn@network <- NULL
        w <- .manifest_wenv()
        leaf <- .manifest_leaf(sn, wenv = w, path = "t")
        expect_null(leaf$fingerprint)
        expect_identical(w$warnings, character(0))
    })
})

describe("summary block", {
    it("reports a spat_unit that has no expression matrix", {
        # Xenium's `nucleus` carries polygons, locations and IDs but no
        # matrix. Reading spat_units off @expression dropped exactly those.
        gv <- GiottoData::loadGiottoMini("vizgen", verbose = FALSE)
        expect_true("z1" %in% names(gv@expression))
        gv@expression$z1 <- NULL

        m <- objManifest(gv)
        expect_true("z1" %in% m$summary$spat_units)
        expect_true("z1" %in% .keyvals(m, "spatial_info", "name"))
        expect_false("z1" %in% .keyvals(m, "expression", "spat_unit"))
    })

    it("omits units that hold no ids", {
        gv <- GiottoData::loadGiottoMini("vizgen", verbose = FALSE)
        gv@cell_ID$phantom <- character(0)
        expect_false("phantom" %in% names(objManifest(gv)$summary$n_cells))
    })
})

describe("giottoBinPoints leaf", {
    make_gbp <- function(n_bins = 50L, seed = 1L) {
        set.seed(seed)
        ids <- sprintf("bin_%d", seq_len(n_bins))
        sl <- createSpatLocsObj(rnorm(n_bins * 2L))
        sl$cell_ID <- ids
        m <- matrix(floor(runif(n_bins * 10L) * 3),
            ncol = n_bins, dimnames = list(letters[1:10], ids)
        )
        createGiottoBinPoints(createExprObj(m), sl)
    }

    it("describes its keys, form and content", {
        gbp <- make_gbp()
        leaf <- .manifest_leaf(gbp, wenv = NULL, path = "t")

        expect_identical(leaf$class, "giottoBinPoints")
        expect_type(leaf$compact, "logical")
        expect_type(leaf$fingerprint, "character")
    })

    it("no longer falls through to the ANY fallback", {
        leaf <- .manifest_leaf(make_gbp(), wenv = NULL, path = "t")
        expect_false(identical(names(leaf), c("class", "length")))
    })

    it("fingerprints differently when the content differs", {
        fp <- function(x) {
            .manifest_leaf(x, wenv = NULL, path = "t")$fingerprint
        }
        a <- fp(make_gbp(seed = 1L))
        b <- fp(make_gbp(seed = 2L))
        expect_type(a, "character")
        expect_false(identical(a, b))
    })

    it("is visible to a diff when set on a giotto object", {
        gv <- GiottoData::loadGiottoMini("vizgen", verbose = FALSE)
        before <- objManifest(gv)
        # the manifest keys records by their own tags, so the object is
        # tagged as the slot key it is assigned under
        gbp <- make_gbp()
        featType(gbp) <- "binpoints"
        gv@feat_info$binpoints <- gbp
        d <- manifestDiff(before, objManifest(gv))
        expect_true(d$changed)
        expect_match(d$summary, "points added: binpoints")
    })

    it("flags two records sharing an identity and keeps both", {
        # direct assignment under a key the object's tags do not match
        gv <- GiottoData::loadGiottoMini("vizgen", verbose = FALSE)
        gv@feat_info$dup <- gv@feat_info$rna
        m <- objManifest(gv)
        expect_true("feat_info.rna (duplicate identity)" %in% m$warnings)
        expect_length(.manifest_flatten(m$slots["feat_info"]), 2L)
    })
})

describe("geometry fingerprints", {
    make_poly <- function() {
        d <- data.table::data.table(
            poly_ID = rep(c("a", "b"), each = 4L),
            x = c(0, 4, 4, 0, 10, 14, 14, 10),
            y = c(0, 0, 4, 4, 0, 0, 4, 4)
        )
        createGiottoPolygon(d)
    }

    it("is invariant to vertex ordering", {
        # writeVector -> shapefile -> vect returns the same shapes with ring
        # winding and start vertex normalised. A positional hash called every
        # saved-and-reloaded object modified.
        gp <- make_poly()
        a <- fingerprint_data(gp@spatVector)

        sv <- gp@spatVector
        gp2 <- gp
        gp2@spatVector <- rbind(sv[2], sv[1])[c(2L, 1L)]
        skip_if(!identical(
            sort(paste(terra::crds(gp@spatVector)[, 1],
                       terra::crds(gp@spatVector)[, 2])),
            sort(paste(terra::crds(gp2@spatVector)[, 1],
                       terra::crds(gp2@spatVector)[, 2]))
        ), "reordering changed the vertex multiset")

        expect_identical(fingerprint_data(gp2@spatVector), a)
    })

    it("still changes when a vertex actually moves", {
        gp <- make_poly()
        a <- fingerprint_data(gp@spatVector)

        moved <- terra::shift(gp@spatVector, dx = 0.5)
        gp2 <- gp
        gp2@spatVector <- moved
        expect_false(identical(fingerprint_data(gp2@spatVector), a))
    })

    it("survives a saveGiotto / loadGiotto round trip", {
        gv <- GiottoData::loadGiottoMini("vizgen", verbose = FALSE)
        td <- file.path(tempdir(), "geom_rt")
        unlink(td, recursive = TRUE)
        dir.create(td, recursive = TRUE)
        on.exit(unlink(td, recursive = TRUE), add = TRUE)

        saveGiotto(gv, dir = td, foldername = "obj", verbose = FALSE,
            overwrite = TRUE)
        gv2 <- loadGiotto(file.path(td, "obj"), verbose = FALSE)

        a <- objManifest(gv)
        b <- objManifest(gv2)
        # points as well as polygons: a lossy attribute table is what made
        # feat_info report a change on every save/load
        for (slot_nm in c("spatial_info", "feat_info")) {
            fps <- function(m) lapply(m$slots[[slot_nm]], `[[`, "fingerprint")
            expect_gt(length(fps(a)), 0L)
            expect_identical(fps(a), fps(b))
        }
    })
})

describe("numeric tolerance", {
    ulp <- function(x) x + .Machine$double.eps * abs(x)

    it("compares in-memory content exactly", {
        # metadata and matrices round-trip exactly through saveGiotto (RDS),
        # so they carry no tolerance and a one-ulp change is a change
        dt <- data.table::data.table(v = as.double(1:10))
        d2 <- data.table::copy(dt)
        d2$v <- ulp(d2$v)
        expect_false(identical(fingerprint_data(dt), fingerprint_data(d2)))
    })

    it("tolerates the last bits a shapefile drops from geometry attributes", {
        # Serialization drops the last bits of a double - shapefile DBF stores
        # numerics as text - and hashing that reported a reloaded object as
        # modified in the same words as a real edit.
        d <- data.table::data.table(
            poly_ID = rep(c("a", "b"), each = 4L),
            x = c(0, 4, 4, 0, 10, 14, 14, 10),
            y = c(0, 0, 4, 4, 0, 0, 4, 4)
        )
        gp <- createGiottoPolygon(d)
        gp@spatVector$score <- c(1.4511308670043945, 2.5)
        a <- fingerprint_data(gp@spatVector)

        nudged <- gp
        # the exact value DBF returns for that first score
        nudged@spatVector$score <- c(1.451130867004395, 2.5)
        expect_identical(fingerprint_data(nudged@spatVector), a)

        changed <- gp
        changed@spatVector$score <- c(1.4511308680043945, 2.5)
        expect_false(identical(fingerprint_data(changed@spatVector), a))
    })
})
