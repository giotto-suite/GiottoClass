#' @include generics.R
NULL

#### giotto object manifest ####

# The manifest is a machine-readable inventory of what a giotto object
# contains: one record per subobject, grouped by slot, keyed by the accessor
# arguments that retrieve it, with cheap per-record statistics and optional
# content fingerprints. It is DERIVED, never stored, so it cannot go stale.
#
# DESIGN RULE (do not relax): every accessor call is individually guarded. An
# accessor that errors degrades one field to NULL and records its path in
# `warnings`; it never errors the manifest. A state report that crashes is
# worse than no state report.

.MANIFEST_SCHEMA_VERSION <- "0.2.0"

# Slots read through as.list(giotto), and the slot each subobject class is
# filed under. as.list() flattens the slot nesting away, so the slot is
# recovered from the class. `cell_ID`/`feat_ID` are handled separately since
# they hold plain character vectors rather than subobjects. `multiomics` is
# left out: it holds bare results, not subobjects.
.MANIFEST_SLOTS <- c(
    "expression", "spatial_locs", "spatial_info", "feat_info",
    "cell_metadata", "feat_metadata", "dimension_reduction", "nn_network",
    "spatial_network", "spatial_enrichment", "images"
)

# checked in order with inherits(), so a subclass resolves to its parent's slot
.MANIFEST_CLASS_SLOT <- c(
    exprObj = "expression",
    spatLocsObj = "spatial_locs",
    giottoPolygon = "spatial_info",
    giottoPoints = "feat_info",
    giottoBinPoints = "feat_info",
    cellMetaObj = "cell_metadata",
    featMetaObj = "feat_metadata",
    dimObj = "dimension_reduction",
    nnNetObj = "nn_network",
    spatialNetworkObj = "spatial_network",
    spatEnrObj = "spatial_enrichment",
    giottoLargeImage = "images",
    giottoImage = "images"
)

# Record fields that identify an item within its slot, in identity order.
# These are the accessor arguments, so a record's `keys` map straight to a
# getter call. A record carries only the ones that apply to it.
.MANIFEST_KEY_FIELDS <- c(
    "reduction", "spat_unit", "feat_type", "reduction_method", "nn_type",
    "name"
)

# Slots whose getter takes fewer keys than the subobject's tags. A polygon
# set's spat_unit is its name (getPolygonInfo() takes `name`), and a point
# set's name is its feat_type (getFeatureInfo() takes `feat_type`).
.MANIFEST_SLOT_KEYS <- list(
    spatial_info = "name",
    feat_info = "feat_type"
)

## guarded field collection ####

# Collector shared by every leaf describer of a single manifest build.
# `wenv` accumulates dotted paths of fields that could not be read.
.manifest_wenv <- function() {
    e <- new.env(parent = emptyenv())
    e$warnings <- character()
    e
}

# Evaluate `f()`, degrading to NULL and recording `path` when it ERRORS.
# A field that reads cleanly and is simply absent (an empty slot, a subobject
# with no provenance) returns NULL without being reported: `warnings` means
# "could not be read", not "is empty".
.mfield <- function(wenv, path, f) {
    ok <- TRUE
    v <- tryCatch(f(), error = function(e) {
        ok <<- FALSE
        NULL
    })
    if (!ok) .mwarn(wenv, path)
    v
}

.mwarn <- function(wenv, path) {
    if (!is.null(wenv)) wenv$warnings <- c(wenv$warnings, path)
    invisible(NULL)
}

## uid ####

# Object uid, minted at creation so it survives renames and copies. Uses
# tempfile() for entropy rather than the RNG, so a user's seed is untouched
# and results stay reproducible.
.make_gobject_uid <- function() {
    rand <- sub("^gid", "", basename(tempfile("gid")))
    sprintf(
        "g-%s-%s",
        format(as.POSIXlt(Sys.time(), tz = "UTC"), "%Y%m%d%H%M%S"), rand
    )
}

# Objects serialized before the `versions` slot existed are repaired by
# updateGiottoObject(), which runs during initialize(). Until then the slot is
# genuinely absent, so both accessors treat that as "no uid" rather than
# erroring - a legacy object must still be loadable.
.gobject_uid <- function(gobject) {
    if (!methods::.hasSlot(gobject, "versions")) return(NULL)
    v <- slot(gobject, "versions")
    if (!is.list(v)) return(NULL)
    v$uid
}

`.gobject_uid<-` <- function(gobject, value) {
    if (!methods::.hasSlot(gobject, "versions")) return(gobject)
    v <- slot(gobject, "versions")
    if (!is.list(v)) v <- list()
    v$uid <- value
    slot(gobject, "versions") <- v
    gobject
}

# mint on first sight; a uid already present is never replaced
.gobject_uid_init <- function(gobject) {
    if (is.null(.gobject_uid(gobject))) {
        .gobject_uid(gobject) <- .make_gobject_uid()
    }
    gobject
}

#' @name objManifest
#' @title Giotto object manifest
#' @description
#' Machine-readable inventory of a `giotto` object's contents: identity,
#' a summary block, and one record per subobject grouped by slot. Each record
#' carries the keys that retrieve it under `keys` (`spat_unit`, `feat_type`,
#' `name`, and `reduction` / `reduction_method` / `nn_type` where they apply),
#' named as the accessor arguments. Derived on demand, so it is always current.
#'
#' Companion to [objHistory()], which records *why* an object looks the way it
#' does. The manifest records *what it is*. Object state is never reconstructed
#' by replaying history.
#'
#' Every accessor used is individually guarded: a field that cannot be read
#' becomes `NULL` and its path is listed in the `warnings` element, rather than
#' erroring the manifest.
#' Every record carries a content fingerprint from [fingerprint_data()]. That
#' is what detects a step that overwrites a matrix or a column in place, which
#' leaves every name unchanged. In-memory content is hashed in full;
#' disk-backed payloads are identified from their handle; images from their
#' pixels at display resolution. Geometry is stride-sampled, with coordinates
#' compared to 12 significant digits so a shapefile round trip does not read
#' as a change. Sizes and extents are not reported: on a disk-backed object
#' they cost a scan.
#' @param x giotto object
#' @param levels logical. Whether to count the distinct values of each
#' metadata column (`n_levels`), which shows a clustering result with its number
#' of groups. Off by default: it is a full pass over every column, and the
#' per-column fingerprints already detect the change. The id column is never
#' counted.
#' @param ... additional params (none implemented)
#' @returns list of class `gmanifest`
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' m <- objManifest(g)
#' names(m$slots)
#' @export
setMethod("objManifest", signature("giotto"), function(
        x, levels = FALSE, ...) {
    checkmate::assert_flag(levels)
    wenv <- .manifest_wenv()

    slots <- .manifest_items(x, wenv = wenv, levels = levels)
    slots <- c(slots, .manifest_recipes(x, wenv = wenv))

    ids <- .manifest_ids(x, wenv = wenv)
    if (length(ids) > 0L) slots <- c(slots, ids)

    out <- list(
        schema_version = .MANIFEST_SCHEMA_VERSION,
        generated_by = paste0(
            "GiottoClass ", as.character(utils::packageVersion("GiottoClass"))
        ),
        generated_at = format(
            as.POSIXlt(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"
        ),
        object = .manifest_object_info(x, wenv),
        summary = .manifest_summary(x, wenv),
        slots = slots[order(names(slots))],
        warnings = unique(wenv$warnings)
    )
    class(out) <- c("gmanifest", "list")
    out
})

## identity and summary ####

.manifest_object_info <- function(x, wenv) {
    versions <- .mfield(wenv, "object.versions", function() {
        v <- slot(x, "versions")
        # package_version objects do not survive JSON encoding
        lapply(v, function(i) if (is.null(i)) NULL else as.character(i))
    })
    # class of the backing source, NULL when in memory. Only the class: a
    # source's path is machine-specific and would read as a change on move
    source <- if (!methods::.hasSlot(x, "source")) {
        NULL
    } else {
        .mfield(wenv, "object.source", function() {
            s <- slot(x, "source")
            if (is.null(s)) NULL else class(s)[[1]]
        })
    }
    list(
        uid = .mfield(wenv, "object.uid", function() .gobject_uid(x)),
        class = class(x)[[1]],
        source = source,
        versions = versions[order(names(versions %||% list()))]
    )
}

.manifest_summary <- function(x, wenv) {
    # spatUnit() / featType() fold over every subobject the object holds, so a
    # unit that carries polygons and locations but no expression matrix - a
    # Xenium `nucleus`, say - is reported. Reading `names(@expression)` instead
    # dropped exactly those units while `n_cells` still counted them, leaving
    # the two halves of this block disagreeing.
    su <- .mfield(wenv, "summary.spat_units", function() spatUnit(x))
    ft <- .mfield(wenv, "summary.feat_types", function() featType(x))

    # a unit with no ids is an empty slot entry, not a unit
    nonempty <- function(ids) {
        n <- vapply(ids, length, integer(1L))
        as.list(n[n > 0L])
    }
    n_cells <- .mfield(wenv, "summary.n_cells", function() {
        nonempty(slot(x, "cell_ID"))
    })
    n_features <- .mfield(wenv, "summary.n_features", function() {
        nonempty(slot(x, "feat_ID"))
    })

    filled <- function(sn) {
        v <- tryCatch(slot(x, sn), error = function(e) NULL)
        length(v) > 0L
    }

    list(
        spat_units = su,
        feat_types = ft,
        n_cells = n_cells,
        n_features = n_features,
        has_images = filled("images"),
        has_spatial_info = filled("spatial_info"),
        has_feat_info = filled("feat_info"),
        has_spatial_network = filled("spatial_network"),
        has_dim_reduction = filled("dimension_reduction"),
        has_nn_network = filled("nn_network"),
        has_spatial_enrichment = filled("spatial_enrichment"),
        is_joined = !is.null(tryCatch(slot(x, "join_info"),
            error = function(e) NULL
        )),
        n_history_steps = length(
            tryCatch(slot(x, "parameters"), error = function(e) list())
        )
    )
}

# cell_ID / feat_ID hold plain character vectors keyed by spat_unit /
# feat_type. Fingerprint the ids rather than listing them: an id vector is
# millions of entries wide and defeats the point of a manifest. Their counts
# are in the summary block.
.manifest_ids <- function(x, wenv) {
    key <- c(cell_ID = "spat_unit", feat_ID = "feat_type")
    out <- list()
    for (sn in names(key)) {
        node <- .mfield(wenv, sn, function() slot(x, sn))
        if (is.null(node) || length(node) == 0L) next
        nms <- sort(names(node))
        out[[sn]] <- lapply(nms, function(nm) {
            rec <- list(keys = stats::setNames(list(nm), key[[sn]]))
            rec$fingerprint <- .manifest_fp(node[[nm]], wenv,
                paste(sn, nm, sep = "."))
            rec
        })
    }
    out
}

## items ####

# Move a described record's key fields under `keys`, in identity order. What is
# in `keys` is the identity and nothing else is, so a reader needs no list of
# which fields identify and which describe. Done once here rather than in every
# leaf describer.
.manifest_keyed <- function(rec, slot = NULL) {
    kf <- intersect(.MANIFEST_KEY_FIELDS, names(rec))
    keys <- rec[intersect(.MANIFEST_SLOT_KEYS[[slot %||% ""]] %||% kf, kf)]
    # an empty named list, so a record with no keys still serializes `{}`
    if (!length(keys)) keys <- stats::setNames(list(), character(0))
    c(list(keys = keys), rec[setdiff(names(rec), kf)])
}

# Identity of a record within the manifest: its slot, then its keys. Used to
# sort records, to prefix the warnings they raise, and by manifestDiff() to
# pair records across manifests.
.manifest_item_id <- function(slot, rec) {
    keys <- vapply(rec$keys, function(k) paste(k, collapse = ","),
        character(1L))
    paste(c(slot, keys), collapse = ".")
}

.manifest_class_slot <- function(obj) {
    for (cl in names(.MANIFEST_CLASS_SLOT)) {
        if (inherits(obj, cl)) return(.MANIFEST_CLASS_SLOT[[cl]])
    }
    NULL
}

# One record per subobject from as.list(giotto), grouped by slot. Each record
# is described with a private warnings collector first, since its identity is
# only known once its key fields have been read; the warnings are then filed
# under that identity. Records are sorted by identity so output is canonical
# and diffs are stable.
.manifest_items <- function(x, wenv, levels = FALSE) {
    objs <- .mfield(wenv, "slots", function() {
        as.list(x, slots = .MANIFEST_SLOTS)
    })
    out <- list()
    ids <- list()
    for (obj in objs) {
        sn <- .manifest_class_slot(obj)
        if (is.null(sn)) {
            .mwarn(wenv, paste0("slots.", class(obj)[[1L]]))
            next
        }
        iw <- .manifest_wenv()
        rec <- .manifest_keyed(
            .manifest_leaf(obj, wenv = iw, path = "", levels = levels),
            slot = sn
        )
        id <- .manifest_item_id(sn, rec)
        for (w in iw$warnings) .mwarn(wenv, paste0(id, w))
        out[[sn]] <- c(out[[sn]], list(rec))
        ids[[sn]] <- c(ids[[sn]], id)
    }
    for (sn in names(out)) {
        # Keys come from each subobject's own tags. Two subobjects tagged
        # alike - one placed by direct slot assignment under a key its tags do
        # not match - share an identity, which no getter call can tell apart.
        for (id in unique(ids[[sn]][duplicated(ids[[sn]])])) {
            .mwarn(wenv, paste0(id, " (duplicate identity)"))
        }
        out[[sn]] <- out[[sn]][order(ids[[sn]])]
    }
    out
}

# Views and spaces are recipes, not data: plain-list steps kept by name in
# `@view` / `@spaces`. as.list(giotto) does not return them, and their name
# is the list key rather than a tag on the object, so they are read here by key.
.manifest_recipes <- function(x, wenv) {
    out <- list()
    for (sn in c("view", "spaces")) {
        if (!methods::.hasSlot(x, sn)) next
        lst <- .mfield(wenv, sn, function() slot(x, sn))
        if (length(lst) == 0L) next
        recs <- lapply(sort(names(lst)), function(nm) {
            iw <- .manifest_wenv()
            rec <- .manifest_keyed(c(
                list(name = nm),
                .manifest_leaf(lst[[nm]], wenv = iw, path = "")
            ))
            id <- .manifest_item_id(sn, rec)
            for (w in iw$warnings) .mwarn(wenv, paste0(id, w))
            rec
        })
        out[[sn]] <- recs
    }
    out
}

## leaf describers ####

# Internal generic. One method per subobject family; every field guarded.
setGeneric(".manifest_leaf", function(x, wenv, path, ...) {
    standardGeneric(".manifest_leaf")
})

# NULL, or the single NA that the nesting accessors return when a class does
# not carry that piece of information
.is_blank <- function(v) {
    is.null(v) || (length(v) == 1L && is.atomic(v) && is.na(v))
}

# fields shared by all giotto subobjects
.manifest_leaf_base <- function(x, wenv, path) {
    g <- function(nm, f) .mfield(wenv, paste(path, nm, sep = "."), f)
    out <- list(
        class = class(x)[[1]],
        name = g("name", function() objName(x)),
        spat_unit = g("spat_unit", function() spatUnit(x)),
        feat_type = g("feat_type", function() featType(x)),
        # prov() has no ANY fallback, so asking a class that carries no
        # provenance would error and be reported as unreadable
        provenance = if (!methods::is(x, "provData")) {
            NULL
        } else {
            g("provenance", function() {
                p <- prov(x)
                if (is.null(p)) NULL else as.character(p)
            })
        }
    )
    # a subobject that legitimately has no name / unit / type is not a warning,
    # and the NA the accessors return for it is not worth emitting
    out[!vapply(out, .is_blank, logical(1L))]
}

# Metadata id columns: one distinct value per row, so a level count says
# nothing and is the costliest to take.
.MANIFEST_ID_COLS <- c("cell_ID", "feat_ID")

# describe the columns of a data.table: name, type and fingerprint. Each column
# carries its own fingerprint, so a diff can name the column an edit touched
# instead of only the table. With `levels`, also its distinct count, so an
# added cluster column is visible with its number of groups; that is a full
# pass over every column, so it is opt-in.
.manifest_columns <- function(dt, wenv = NULL, path = "", levels = FALSE) {
    if (!inherits(dt, "data.frame")) return(NULL)
    # entries are keyed by column name; data.table allows a name twice, which
    # would collide, so a repeat is renamed and reported rather than dropped
    cns <- colnames(dt)
    if (anyDuplicated(cns)) {
        .mwarn(wenv, paste(path, "columns (duplicate column names)", sep = "."))
    }
    out <- lapply(seq_along(cns), function(i) {
        cn <- cns[[i]]
        col <- dt[[i]]
        entry <- list(
            dtype = class(col)[[1]],
            n_levels = if (!isTRUE(levels) || cn %in% .MANIFEST_ID_COLS) {
                NULL
            } else {
                tryCatch(
                    as.integer(data.table::uniqueN(col)),
                    error = function(e) NULL
                )
            }
        )
        if (is.factor(col)) entry$levels <- as.integer(nlevels(col))
        entry$fingerprint <- .manifest_fp(col, wenv,
            paste(path, "columns", cn, sep = "."))
        entry[!vapply(entry, is.null, logical(1L))]
    })
    stats::setNames(out, make.unique(cns))
}

setMethod(".manifest_leaf", signature("ANY"), function(x, wenv, path, ...) {
    list(class = class(x)[[1]])
})

setMethod(".manifest_leaf", signature("exprData"), function(
        x, wenv, path, ...) {
    g <- function(nm, f) .mfield(wenv, paste(path, nm, sep = "."), f)
    m <- g("exprMat", function() slot(x, "exprMat"))

    out <- .manifest_leaf_base(x, wenv, path)
    out$dtype <- if (is.null(m)) NULL else class(m)[[1]]
    out$fingerprint <- .manifest_fp(m, wenv, path)
    out
})

setMethod(".manifest_leaf", signature("metaData"), function(
        x, wenv, path, levels = FALSE, ...) {
    g <- function(nm, f) .mfield(wenv, paste(path, nm, sep = "."), f)
    dt <- g("metaDT", function() slot(x, "metaDT"))

    out <- .manifest_leaf_base(x, wenv, path)
    out$columns <- g("columns", function() {
        .manifest_columns(dt, wenv = wenv, path = path, levels = levels)
    })
    out$col_desc <- g("col_desc", function() {
        cd <- slot(x, "col_desc")
        if (length(cd) == 1L && is.na(cd)) NULL else as.list(cd)
    })
    # The table's fingerprint is built from its columns' rather than hashing
    # the table again. A table that is not in memory is identified whole.
    out$fingerprint <- if (inherits(dt, "data.frame")) {
        fps <- lapply(out$columns, `[[`, "fingerprint")
        if (any(vapply(fps, is.null, logical(1L)))) {
            NULL
        } else {
            .fp_hash(unname(fps))
        }
    } else {
        .manifest_fp(dt, wenv, path)
    }
    out
})

setMethod(".manifest_leaf", signature("coordDataDT"), function(
        x, wenv, path, ...) {
    g <- function(nm, f) .mfield(wenv, paste(path, nm, sep = "."), f)
    dt <- g("coordinates", function() slot(x, "coordinates"))

    out <- .manifest_leaf_base(x, wenv, path)
    out$columns <- if (is.null(dt)) NULL else colnames(dt)
    out$fingerprint <- .manifest_fp(dt, wenv, path)
    out
})

setMethod(".manifest_leaf", signature("dimObj"), function(
        x, wenv, path, ...) {
    g <- function(nm, f) .mfield(wenv, paste(path, nm, sep = "."), f)
    out <- .manifest_leaf_base(x, wenv, path)
    out$reduction <- g("reduction", function() slot(x, "reduction"))
    out$reduction_method <- g("reduction_method",
        function() slot(x, "reduction_method"))
    # the component names, which is how a reader learns how many there are
    out$columns <- g("columns", function() colnames(slot(x, "coordinates")))
    out$fingerprint <- .manifest_fp(slot(x, "coordinates"), wenv, path)
    out
})

setMethod(".manifest_leaf", signature("nnData"), function(
        x, wenv, path, ...) {
    g <- function(nm, f) .mfield(wenv, paste(path, nm, sep = "."), f)
    net <- g("network", function() slot(x, "network"))

    out <- .manifest_leaf_base(x, wenv, path)
    out$nn_type <- g("nn_type", function() slot(x, "nn_type"))
    out$directed <- g("directed", function() {
        if (inherits(net, "igraph")) igraph::is_directed(net) else NULL
    })
    out$fingerprint <- .manifest_fp(net, wenv, path)
    out
})

setMethod(".manifest_leaf", signature("spatNetData"), function(
        x, wenv, path, ...) {
    g <- function(nm, f) .mfield(wenv, paste(path, nm, sep = "."), f)
    net <- g("network", function() slot(x, "network"))

    out <- .manifest_leaf_base(x, wenv, path)
    out$method <- g("method", function() slot(x, "method"))
    out$has_unfiltered <- !is.null(
        tryCatch(slot(x, "unfiltered"), error = function(e) NULL)
    )
    out$fingerprint <- .manifest_fp(net, wenv, path)
    out
})

setMethod(".manifest_leaf", signature("enrData"), function(
        x, wenv, path, ...) {
    g <- function(nm, f) .mfield(wenv, paste(path, nm, sep = "."), f)
    dt <- g("enrichDT", function() slot(x, "enrichDT"))

    out <- .manifest_leaf_base(x, wenv, path)
    out$method <- g("method", function() slot(x, "method"))
    out$columns <- if (is.null(dt)) NULL else colnames(dt)
    out$fingerprint <- .manifest_fp(dt, wenv, path)
    out
})

setMethod(".manifest_leaf", signature("terraVectData"), function(
        x, wenv, path, ...) {
    g <- function(nm, f) .mfield(wenv, paste(path, nm, sep = "."), f)
    out <- .manifest_leaf_base(x, wenv, path)
    out$crs <- g("crs", function() {
        cr <- terra::crs(slot(x, "spatVector"), describe = TRUE)$name
        if (is.na(cr)) NULL else cr
    })

    if (inherits(x, "giottoPolygon")) {
        out$centroids_cached <- !is.null(
            tryCatch(slot(x, "spatVectorCentroids"), error = function(e) NULL)
        )
        out$overlaps_computed <- g("overlaps_computed", function() {
            o <- slot(x, "overlaps")
            if (is.null(o)) NULL else names(o)
        })
    }
    out$fingerprint <- .manifest_fp(slot(x, "spatVector"), wenv, path)
    out
})

# giottoBinPoints carries `featData` and `giottoSubobject` but not
# `terraVectData`, so without its own method it matched only the ANY fallback
# and reported nothing but its class: a binpoints object could be added,
# rewritten or dropped and no diff would say so. Its geometry lives in
# `@spatial` and its detections in `@counts`, which is why the generic
# terra describer does not fit it.
setMethod(".manifest_leaf", signature("giottoBinPoints"), function(
        x, wenv, path, ...) {
    g <- function(nm, f) .mfield(wenv, paste(path, nm, sep = "."), f)
    out <- .manifest_leaf_base(x, wenv, path)
    out$compact <- g("compact", function() isTRUE(slot(x, "compact")))
    # detections in @counts, geometry in @spatial, and the bin / feature ids
    out$fingerprint <- .manifest_fp(list(
        counts = slot(x, "counts"), spatial = slot(x, "spatial"),
        bid = slot(x, "bid"), fid = slot(x, "fid")
    ), wenv, path)
    out
})

setMethod(".manifest_leaf", signature("giottoLargeImage"), function(
        x, wenv, path, ...) {
    g <- function(nm, f) .mfield(wenv, paste(path, nm, sep = "."), f)
    r <- g("raster_object", function() slot(x, "raster_object"))

    out <- list(
        class = class(x)[[1]],
        name = g("name", function() objName(x)),
        file_path = g("file_path", function() {
            p <- slot(x, "file_path")
            if (is.null(p) || all(is.na(p))) NULL else as.character(p)
        }),
        on_disk = g("on_disk", function() {
            any(nzchar(terra::sources(r)))
        })
    )
    # an affine image's placement lives in @affine, not in the raster
    payload <- if (inherits(x, "giottoAffineImage")) {
        list(raster = r, affine = slot(x, "affine"))
    } else {
        r
    }
    out$fingerprint <- .manifest_fp(payload, wenv, path)
    out[!vapply(out, is.null, logical(1L))]
})

# a recipe: its kind is the class, its content the step list
.manifest_leaf_recipe <- function(x, wenv, path) {
    g <- function(nm, f) .mfield(wenv, paste(path, nm, sep = "."), f)
    steps <- g("steps", function() slot(x, "steps"))
    out <- list(
        class = class(x)[[1]],
        n_steps = length(steps),
        step_types = g("step_types", function() {
            unique(vapply(steps, function(s) s$type %||% NA_character_,
                character(1L)))
        })
    )
    out$fingerprint <- .manifest_fp(steps, wenv, path)
    out[!vapply(out, is.null, logical(1L))]
}

setMethod(".manifest_leaf", signature("giottoView"), function(
        x, wenv, path, ...) {
    .manifest_leaf_recipe(x, wenv, path)
})

setMethod(".manifest_leaf", signature("giottoSpace"), function(
        x, wenv, path, ...) {
    .manifest_leaf_recipe(x, wenv, path)
})

setMethod(".manifest_leaf", signature("giottoImage"), function(
        x, wenv, path, ...) {
    g <- function(nm, f) .mfield(wenv, paste(path, nm, sep = "."), f)
    out <- list(
        class = class(x)[[1]],
        name = g("name", function() objName(x)),
        resolution = g("resolution", function() slot(x, "resolution")),
        file_path = g("file_path", function() {
            p <- slot(x, "file_path")
            if (is.null(p) || all(is.na(p))) NULL else as.character(p)
        })
    )
    out[!vapply(out, is.null, logical(1L))]
})

## fingerprints ####

# Geometries sampled per SpatVector, the one in-memory payload that is
# stride-sampled rather than hashed in full.
.MANIFEST_FP_N <- 1000L

# Significant digits a fingerprint compares numbers to.
#
# A fingerprint answers "are these the same values", not "are these the same
# bit patterns". Serialization formats lose the last bits of a double -
# shapefile DBF stores numerics as fixed-width text, so a coordinate attribute
# comes back differing by ~5e-16 relative - and hashing that difference reports
# a reloaded object as modified in exactly the same words as a real edit. Two
# decimal digits of slack removes that without approaching anything
# analytically meaningful: a change of 1e-9 relative still registers.
.MANIFEST_FP_DIGITS <- 12L

.fp_num <- function(x) {
    if (is.double(x)) signif(x, .MANIFEST_FP_DIGITS) else x
}

# Every fingerprint is hashed here, so the algorithm is chosen in one place:
# fingerprints compare only when they come from the same hash.
# A hash of NULL is a constant, so hashing "nothing" would make every
# object whose content could not be read look identical to every other - and
# permanently unchanged. Absent is reported as absent.
.fp_hash <- function(parts) {
    if (is.null(parts)) return(NULL)
    rlang::hash(parts)
}

# Used by the leaf describers, each handing over its payload. Never errors:
# an unrecognised payload (fingerprint_data()'s ANY method errors) or an
# accessor failure degrades to NULL with the path recorded in `warnings`.
# `payload` is a promise, so a slot read that fails is caught here too.
.manifest_fp <- function(payload, wenv, path) {
    .mfield(wenv, paste(path, "fingerprint", sep = "."), function() {
        fingerprint_data(payload)
    })
}

#' @name fingerprint_data
#' @title Fingerprint a subobject payload
#' @description
#' Extension point for [objManifest()]. Returns a short hash identifying the
#' content of a payload - the matrix, table, graph or store a subobject holds -
#' so that a manifest diff can tell when it changed.
#'
#' GiottoClass hashes the in-memory payloads it knows itself. A package that
#' provides another payload representation, such as a disk-backed store,
#' registers a method here. The method must answer from handle state (ids,
#' recorded operations, schema, file metadata) and must never read the data:
#' an identical handle has to mean identical content, and a changed one
#' different content. Never hash the payload object directly. Objects holding
#' external pointers or environments hash by instance, not by content.
#'
#' A method returns a hash or errors; it never returns `NULL` for a payload it
#' was given. The default method errors, and the manifest reports that as an
#' unreadable fingerprint in its `warnings` rather than treating the payload
#' as unchanged. Only an absent payload (`NULL`) has no fingerprint.
#' @param x a payload
#' @param ... additional params (none implemented)
#' @returns character scalar, or `NULL` for a `NULL` payload
#' @keywords internal
#' @export
NULL

# A payload nothing recognises is an error, which .manifest_fp() turns into a
# warning: a missing fingerprint must be visible, never read as "unchanged".
#' @rdname fingerprint_data
#' @export
setMethod("fingerprint_data", signature("ANY"), function(x, ...) {
    stop("[fingerprint_data] no method for payload class ", class(x)[[1L]],
        call. = FALSE)
})

# an absent payload is not an error
#' @rdname fingerprint_data
#' @export
setMethod("fingerprint_data", signature("NULL"), function(x, ...) NULL)

# Coordinates as a canonical multiset: same vertices in the same order
# regardless of how the geometry happens to be laid out.
#
# Writing a SpatVector to a shapefile and reading it back returns the same
# shapes with their ring winding and start vertex normalised, so a positional
# hash of the coordinates reports every saved-and-reloaded object as modified.
# Ordering the vertices removes that false positive. Vertex multiplicity is
# preserved, so a moved, added or dropped vertex still changes the hash; what
# is given up is sensitivity to a rearrangement that leaves the whole
# coordinate multiset intact, which `n`, the extent and the attributes are
# there to catch.
.fp_canonical_crds <- function(sv) {
    cr <- .fp_num(terra::crds(sv))
    if (nrow(cr) == 0L) return(cr)
    cr[order(cr[, 1L], cr[, 2L]), , drop = FALSE]
}

## serialization ####

# Fields the schema types as arrays. R has no scalar type, and objManifest_json()
# writes with auto_unbox, so a length-1 vector would otherwise come out as a
# bare value: `"provenance": "cell"` on one object and `["z0", "z1"]` on
# another. These are kept arrays at any length.
.MANIFEST_ARRAY_FIELDS <- c(
    "spat_units", "feat_types", "warnings", "provenance", "columns",
    "overlaps_computed", "resolution", "file_path", "step_types"
)

# Explicit schema rule: NA / NaN / Inf are encoded as strings, not as JSON
# null (jsonlite's default), so a missing value and a not-a-number stay
# distinguishable on the far side of the contract.
.manifest_json_prep <- function(x, field = NULL) {
    if (is.list(x)) {
        nms <- names(x)
        out <- lapply(seq_along(x), function(i) {
            .manifest_json_prep(x[[i]], field = nms[i])
        })
        names(out) <- nms
        return(out)
    }
    x <- .manifest_json_value(x)
    if (!is.null(field) && !is.na(field) && field %in% .MANIFEST_ARRAY_FIELDS &&
        is.atomic(x)) {
        x <- I(x)
    }
    x
}

.manifest_json_value <- function(x) {
    if (is.factor(x)) x <- as.character(x)
    if (is.numeric(x)) {
        bad <- !is.finite(x)
        if (any(bad)) {
            out <- as.character(x)
            out[is.na(x) & !is.nan(x)] <- "NA"
            out[is.nan(x)] <- "NaN"
            out[!is.na(x) & is.infinite(x) & x > 0] <- "Inf"
            out[!is.na(x) & is.infinite(x) & x < 0] <- "-Inf"
            return(out)
        }
        return(x)
    }
    if (is.character(x) && anyNA(x)) {
        x[is.na(x)] <- "NA"
        return(x)
    }
    x
}

#' @name objManifest_json
#' @title Giotto object manifest as JSON
#' @description
#' Serialize the manifest from [objManifest()]. Keys are emitted in canonical
#' (sorted) order and `NA`/`NaN`/`Inf` are encoded as the strings `"NA"`,
#' `"NaN"`, `"Inf"` and `"-Inf"` by schema rule.
#' @param x giotto object or a `gmanifest` from [objManifest()]
#' @param file character. Optional path to write to. When `NULL` (default) the
#' JSON is returned as a character scalar.
#' @param pretty logical. Whether to indent the output
#' @param ... additional params passed to [objManifest()] when `x` is a
#' `giotto` object
#' @returns character scalar of JSON, or the file path, invisibly, when
#' `file` is given
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' cat(substr(objManifest_json(g), 1, 200))
#' @export
objManifest_json <- function(x, file = NULL, pretty = TRUE, ...) {
    GiottoUtils::package_check("jsonlite", repository = "CRAN:jsonlite")

    m <- if (inherits(x, "gmanifest")) x else objManifest(x, ...)
    txt <- jsonlite::toJSON(
        .manifest_json_prep(unclass(m)),
        auto_unbox = TRUE, null = "null", na = "null", pretty = pretty,
        digits = NA
    )
    if (is.null(file)) return(as.character(txt))

    writeLines(as.character(txt), con = file)
    invisible(file)
}

#' @export
#' @keywords internal
print.gmanifest <- function(x, ...) {
    # what the object holds, by the keys that retrieve it. Content is left to
    # manifestDiff(); a hash per record says nothing on its own.
    s <- x$summary
    counts <- function(n) {
        if (!length(n)) return("none")
        paste(names(n), unlist(n), collapse = ", ")
    }
    cat(sprintf("<gmanifest> schema %s
", x$schema_version))
    cat(sprintf("  uid: %s | source: %s
", x$object$uid %||% "<none>",
        x$object$source %||% "in memory"))
    cat(sprintf("  cells: %s | features: %s
",
        counts(s$n_cells), counts(s$n_features)))
    # the id slots are the counts above
    for (sn in setdiff(names(x$slots), c("cell_ID", "feat_ID"))) {
        cat(sprintf("  %s\n", sn))
        for (rec in x$slots[[sn]]) {
            cat(sprintf("    %s\n", paste(unlist(rec$keys), collapse = " / ")))
        }
    }
    if (length(x$warnings)) {
        cat(sprintf("  unreadable (%d): %s\n", length(x$warnings),
            paste(x$warnings, collapse = ", ")))
    }
    invisible(x)
}


# fingerprint_data() methods ####

# Payloads GiottoClass itself understands. Content is hashed from extracted
# atomic data, never from the payload object: an object holding external
# pointers or environments hashes by instance (a SpatRaster read twice from one
# file gives two hashes), and incidental attributes - a data.table key, a
# Matrix factor cache - would read as content. .fp_hash() uses rlang::hash(),
# which streams into the hasher without building a serialized copy.

# atomic content hashes in full, names and dimnames included. A list is the
# hash of its elements' fingerprints, so an element nothing recognises makes
# the whole list unidentifiable.
#' @rdname fingerprint_data
#' @export
setMethod("fingerprint_data", signature("vector"), function(x, ...) {
    if (is.atomic(x)) return(.fp_hash(x))
    if (!is.list(x)) callNextMethod()
    .fp_hash(list(names = names(x), parts = lapply(x, fingerprint_data)))
})

# by column, so a data.table key or index does not read as a change
#' @rdname fingerprint_data
#' @export
setMethod("fingerprint_data", signature("data.frame"), function(x, ...) {
    .fp_hash(list(names = names(x), cols = lapply(x, fingerprint_data)))
})

# Matrix classes are plain S4 over atomic slots (@x, @i, @p, @Dim, ...), so
# every slot is hashed except the cached factorizations
#' @rdname fingerprint_data
#' @export
setMethod("fingerprint_data", signature("Matrix"), function(x, ...) {
    sn <- setdiff(methods::slotNames(x), "factors")
    .fp_hash(c(
        list(class = class(x)[[1L]]),
        stats::setNames(lapply(sn, function(s) slot(x, s)), sn)
    ))
})

setOldClass("igraph")

# integer edge list plus attributes; vertex names travel in the attributes
#' @rdname fingerprint_data
#' @export
setMethod("fingerprint_data", signature("igraph"), function(x, ...) {
    .fp_hash(list(
        n = igraph::vcount(x),
        directed = igraph::is_directed(x),
        edges = igraph::as_edgelist(x, names = FALSE),
        vattr = igraph::vertex_attr(x),
        eattr = igraph::edge_attr(x)
    ))
})

# Stride-sampled: geometry lives behind a pointer and extracting all of it is
# the cost this avoids. `k` values are read regardless of size (Inf reads
# all). Coordinates are compared as a canonical multiset and to 12
# significant digits, so a shapefile round trip does not read as a change.
#' @rdname fingerprint_data
#' @param k numeric. SpatVector only: number of geometries sampled
#' @export
setMethod("fingerprint_data", signature("SpatVector"), function(
        x, k = .MANIFEST_FP_N, ...) {
    n <- terra::nrow(x)
    idx <- if (is.infinite(k) || n <= k) {
        seq_len(n)
    } else {
        unique(as.integer(seq.int(1L, n, length.out = k)))
    }
    sub <- x[idx]
    .fp_hash(list(
        n = n,
        ext = .fp_num(as.vector(terra::ext(x))),
        crds = .fp_canonical_crds(sub),
        att = lapply(terra::values(sub), function(cc) as.character(.fp_num(cc)))
    ))
})

# a deferred affine transform: every slot is plain numeric / character, so all
# of them are hashed (the matrix, its decomposition, and the anchor extent)
#' @rdname fingerprint_data
#' @export
setMethod("fingerprint_data", signature("affine2d"), function(x, ...) {
    sn <- methods::slotNames(x)
    .fp_hash(stats::setNames(
        lapply(sn, function(s) fingerprint_data(slot(x, s))), sn
    ))
})

# Pixels at display resolution, plus the placement. Identifying the source file
# instead gave the same pixels a new fingerprint whenever they moved to a new
# file - a saveGiotto() export, or the temp file terra spills an arithmetic
# result to - and none at all to a raster held in memory.
#
# method = "display" is what plot() reads: it uses the overviews where a file
# has them, so a pyramid or COG never reads full-resolution tiles. Its values
# depend on the format (overview pixels are resampled), so two fingerprints
# compare only for the same image in the same format; that is the only
# comparison a diff makes. It can miss a local edit finer than the grid.
.MANIFEST_FP_RASTER_N <- 10000L

#' @rdname fingerprint_data
#' @export
setMethod("fingerprint_data", signature("SpatRaster"), function(x, ...) {
    d <- terra::spatSample(x, size = .MANIFEST_FP_RASTER_N,
        method = "display")
    .fp_hash(list(
        dim = dim(x),
        ext = .fp_num(as.vector(terra::ext(x))),
        crs = terra::crs(x),
        # layer names are dropped: a re-export renames layers without
        # touching a pixel
        vals = unname(terra::values(d, mat = TRUE))
    ))
})
