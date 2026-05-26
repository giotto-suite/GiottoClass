# =============================================================================
# giottoMulti — multi-dataset container for shared expression-space analysis
# =============================================================================
#
# DESIGN NOTES (sketch, 2026-05-18)
# ---------------------------------
# `giottoMulti` represents N giotto objects that each keep their own SPATIAL
# information (different physical/embedding spaces) but participate in a SHARED
# expression-space analysis (joint normalization, dim reduction, NN graphs,
# clustering, integration).
#
# Inheritance:
#   gAny (virtual)
#   ├── giotto
#   └── giottoMulti
#
# Why a virtual base rather than `giottoMulti contains "giotto"`:
#   * `giottoMulti`'s spatial slots are intentionally empty — they live in
#     `@objects`. Inheriting from `giotto` would let spatial-domain methods
#     silently fall through to those empty slots. With `gAny` as a virtual
#     base, an undefined method fails loudly via no-method dispatch.
#   * Shared-domain methods are written once on `gAny` and apply to both.
#   * `giotto`'s validity rules don't have to accommodate empty spatial slots.
#
# Slot strategy: ALIGN where semantics are shared, ADD where new.
#   * Aligned with `giotto` (same name, same type): @expression,
#     @cell_metadata, @feat_metadata, @dimension_reduction, @nn_network,
#     @spatial_enrichment, @multiomics, @instructions, @parameters, etc.
#     Existing accessor generics (`getExpression`, `pDataDT`, ...) can be
#     promoted from `"giotto"` to `"gAny"` one at a time.
#   * NEW: @objects, @id_map — multi-specific.
#   * OMITTED from `giotto`: @spatial_locs, @spatial_info, @spatial_network,
#     @spatial_grid, @feat_info, @images, @join_info, @offset_file, @h5_file.
#     These are per-dataset and live in the children.
#
# Children in @objects:
#   * Named list of `giotto` objects (or disk-backed equivalents).
#   * `list` chosen over `environment` to keep value semantics — disk-backed
#     children get reference semantics from the storage layer regardless;
#     avoiding R-level reference semantics on the container removes a class
#     of surprise.
#
# id_map structure:
#   * `@id_map$cells`: data.table(object, local_id, global_id) — namespaces
#     cell IDs across children. Default global_id = paste(object, local_id, sep = "::").
#   * `@id_map$feats`: data.table(object, local_id, global_id) — features
#     usually overlap legitimately across datasets, so global_id often equals
#     local_id. Used to track which features are present in which datasets.
#
# Scope on per-child operations is explicit, not stateful: pass `object =`
# to target a subset of children, omit it (or pass NULL) to operate on all.
# There is no "active" pointer — set/get with omitted `object` would otherwise
# diverge depending on whether the joint slot is populated yet.
#
# Per-child defaults (active spat_unit / feat_type) are derived live from
# children via `activeSpatUnit(child)` / `activeFeatType(child)`. No cache:
# a cache drifts whenever a child is mutated standalone, and the suite
# discourages divergent per-child analyses on a multi anyway. If a child
# needs a different spat_unit semantically, the right move is usually a
# new gmulti rather than flipping defaults in place.
#
# Dispatch pattern (recommended, applied incrementally):
#   * Shared-domain method:
#       setMethod("getExpression", "gAny", function(gobject, ...) { ... })
#   * Spatial-domain / per-object method:
#       setMethod("getSpatialLocations", "giottoMulti",
#           function(gobject, object = NULL, ...) {
#               objs <- .gm_resolve_objects(gobject, object)
#               lapply(objs, function(o) getSpatialLocations(gobject[[o]], ...))
#           })
#
# Open design questions (NOT resolved in this sketch):
#   * How does joinGiottoObjects interact with giottoMulti? Most natural
#     read: `giottoMulti(list(g1, g2, ...))` is the "preserve spaces"
#     constructor; `joinGiottoObjects()` remains the "merge into one
#     giotto" path. They are siblings, not nested.
#   * How does `subset()` work on giottoMulti? Subset children, subset cells
#     globally (with id_map lookup), or both?
#   * Do per-child cell-metadata columns get reflected upward into the joint
#     @cell_metadata, or kept separate and merged on demand?
#   * Where do integration-method parameters (Harmony, Seurat-anchor) live?
#     Probably @parameters, possibly a dedicated @integration slot if it
#     grows. Defer until we have a concrete use case.
# =============================================================================


# CLASS ####

#' @title S4 giottoMulti
#' @name giottoMulti-class
#' @description
#' Container for multiple `giotto` objects whose spatial information is kept
#' separate (one space per child) but whose expression-space analysis is
#' shared across all cells.
#'
#' @slot objects named `list` of `giotto` objects (children)
#' @slot id_map `list` with elements `cells` and `feats`, each a `data.table`
#'   mapping `(object, local_id) → global_id`
#'
#' @slot expression shared expression matrices (rows = union of features,
#'   cols = global cell IDs)
#' @slot cell_metadata shared cell metadata (one row per global cell ID)
#' @slot feat_metadata shared feature metadata (one row per global feature)
#' @slot cell_ID shared cell ID lists (global IDs)
#' @slot feat_ID shared feature ID lists (global IDs)
#' @slot dimension_reduction shared joint dim-reductions (PCA, UMAP, harmony)
#' @slot nn_network shared joint NN graphs
#' @slot spatial_enrichment shared spatial enrichment results
#' @slot multiomics shared multi-omics info
#'
#' @slot instructions giotto-style instructions
#' @slot parameters analysis parameters (mirrors `giotto@parameters`)
#' @slot versions package versions
#' @slot misc miscellaneous
#' @slot source on-disk source / project manager (e.g.
#'   [GiottoDisk::gDirSource]) where multi-level shared-domain artifacts
#'   live. `NULL` for in-memory multis. Children may carry their own
#'   per-sample sources; multi-level slots (shared `@expression`,
#'   `@dimension_reduction`, `@nn_network`) write to this one.
#'
#' @returns giottoMulti object
#' @exportClass giottoMulti
giottoMulti <- setClass(
    "giottoMulti",
    contains = "gAny",
    slots = c(
        # multi-specific
        objects             = "list",
        id_map              = "list",
        id_sig              = "list",

        # shared-domain (names aligned with giotto)
        expression          = "nullOrList",
        expression_feat     = "nullOrChar",
        cell_metadata       = "nullOrList",
        feat_metadata       = "nullOrList",
        cell_ID             = "nullOrList",
        feat_ID             = "nullOrList",
        spatial_enrichment  = "nullOrList",
        dimension_reduction = "nullOrList",
        nn_network          = "nullOrList",
        multiomics          = "ANY",

        # infrastructure
        instructions        = "nullOrInstructions",
        parameters          = "ANY",
        versions            = "list",
        misc                = "list",
        source              = "ANY"
    ),
    prototype = list(
        objects             = list(),
        id_map              = list(cells = NULL, feats = NULL),
        id_sig              = list(),

        expression          = NULL,
        expression_feat     = NULL,
        cell_metadata       = NULL,
        feat_metadata       = NULL,
        cell_ID             = NULL,
        feat_ID             = NULL,
        spatial_enrichment  = NULL,
        dimension_reduction = NULL,
        nn_network          = NULL,
        multiomics          = NULL,

        instructions        = NULL,
        parameters          = list(),
        versions            = .versions_info(),
        misc                = list(),
        source              = NULL
    )
)


# INITIALIZE ####

#' @noRd
setMethod("initialize", signature("giottoMulti"), function(.Object, objects = NULL, ...) {
    .Object <- callNextMethod(.Object, ...)

    # Construction path: ingest the children. Skipped on bare re-init calls
    # (no `objects` arg) so an already-constructed giottoMulti can be
    # re-initialized without re-supplying its children.
    if (!is.null(objects) && length(objects) > 0L) {
        checkmate::assert_list(objects, types = "giotto", names = "unique",
            .var.name = "objects")

        .Object@objects <- objects
    }

    if (length(.Object@objects) == 0L) return(.Object)

    # Source resolution: federated multi keeps per-child sources intact but
    # also carries one multi-level source for cross-sample shared-domain
    # artifacts (joint PCA, joint NN networks). Enforce backend-type
    # homogeneity across any sourced children: mixing parquet- and
    # bpcells-backed children breaks union/cbind dispatch downstream.
    .Object@source <- .gm_resolve_source(.Object@source, .Object@objects)

    # id_map: cache rebuilt only when child length-signatures differ from the
    # cached signature. Bare re-init when nothing changed is a no-op modulo a
    # signature comparison over N children — microseconds even at atlas scale.
    cur_sig <- .gm_compute_sig(.Object@objects)
    if (!identical(cur_sig, .Object@id_sig)) {
        .Object@id_map$cells <- .gm_build_cell_idmap(.Object@objects)
        .Object@id_map$feats <- .gm_build_feat_idmap(.Object@objects)
        .Object@id_sig <- cur_sig
    }

    .Object
})


# Decide the multi's @source slot from a (possibly-NULL) explicit value and
# the children's per-child sources.
# - All sourced children must share the same source class. Error otherwise.
# - If `explicit` is provided, it must also match that class. If it doesn't
#   match because no children have sources, accept it as-is.
# - If `explicit` is NULL, adopt the first sourced child's source.
# - If no children have sources and no explicit source provided, leave NULL.
.gm_resolve_source <- function(explicit, objects) {
    child_sources <- lapply(objects, function(g) g@source)
    have_source <- !vapply(child_sources, is.null, logical(1L))
    if (any(have_source)) {
        classes <- vapply(child_sources[have_source],
            function(s) class(s)[[1L]], character(1L))
        if (length(unique(classes)) > 1L) {
            stop("giottoMulti: children carry sources of different classes (",
                paste(unique(classes), collapse = ", "),
                "). All sourced children must use the same backend.",
                call. = FALSE)
        }
        if (!is.null(explicit) && class(explicit)[[1L]] != classes[[1L]]) {
            stop("giottoMulti: explicit source class '",
                class(explicit)[[1L]],
                "' does not match children's source class '",
                classes[[1L]], "'.", call. = FALSE)
        }
    }
    if (!is.null(explicit)) return(explicit)
    first_idx <- which(have_source)[1L]
    if (!is.na(first_idx)) return(child_sources[[first_idx]])
    NULL
}


# CONSTRUCTOR ####

#' @title Create a giottoMulti object
#' @name createGiottoMulti
#' @description Container for multiple `giotto` objects analyzed in a shared
#' expression space. Each child keeps its own spatial information; shared
#' analyses (joint dim reduction, NN graphs, clustering) live on the parent.
#'
#' @param objects named `list` of `giotto` objects
#' @param instructions a `giottoInstructions` object (optional)
#' @param source on-disk source / project manager (e.g.
#'   [GiottoDisk::gDirSource]) for cross-sample shared-domain artifacts.
#'   If `NULL` (default), auto-acquired from the first sourced child; if
#'   no child carries a source, the multi is in-memory. When supplied,
#'   must be the same backend class as any source the children carry.
#'
#' @returns `giottoMulti`
#' @examples
#' \dontrun{
#' g1 <- GiottoData::loadGiottoMini("visium")
#' g2 <- GiottoData::loadGiottoMini("viz")
#' mg <- createGiottoMulti(list(visium = g1, viz = g2))
#' }
#' @export
createGiottoMulti <- function(objects, instructions = NULL, source = NULL) {
    checkmate::assert_list(objects, types = "giotto", names = "unique")
    args <- list(objects = objects)
    if (!is.null(instructions)) args$instructions <- instructions
    if (!is.null(source)) args$source <- source
    do.call(new, c("giottoMulti", args))
}


# COERCION ####

#' Wrap a single `giotto` as a one-child `giottoMulti`.
#'
#' Useful as a quick path for code that wants to operate uniformly on a multi
#' (no class branches), and to gain the lazy view layer (`subset()` /
#' `compact()` / view-filter on read) on top of a single giotto without
#' committing to a destructive in-place subset.
#'
#' The child is named `"sample1"` by default. To pick a different name use
#' `createGiottoMulti(list(my_name = g))` directly.
#' @name as-giottoMulti
#' @aliases as,giotto,giottoMulti-method
setAs("giotto", "giottoMulti", function(from) {
    createGiottoMulti(list(sample1 = from))
})


# INTROSPECTION ####

#' @noRd
setMethod("names", "giottoMulti", function(x) names(x@objects))

#' @noRd
setMethod("length", "giottoMulti", function(x) length(x@objects))

#' @noRd
setMethod("[[", signature(x = "giottoMulti", i = "ANY", j = "missing"),
    function(x, i, j, ...) x@objects[[i]])

#' @noRd
setReplaceMethod("[[", signature(x = "giottoMulti", i = "ANY", j = "missing", value = "giotto"),
    function(x, i, j, ..., value) {
        x@objects[[i]] <- value
        # id_map is not eagerly refreshed; the user (or the next initialize()
        # call) is responsible. initialize() detects the change via the
        # length-signature fast-path.
        x
    }
)

#' @noRd
setMethod("[", signature(x = "giottoMulti", i = "ANY"),
    function(x, i, j, ..., drop = TRUE) {
        # Select children by name or integer index; return a new giottoMulti
        # with the chosen subset. Joint shared slots are NOT rewritten — the
        # caller can subset/compact if they want them aligned.
        sel <- if (is.character(i)) {
            bad <- setdiff(i, names(x))
            if (length(bad) > 0L) {
                stop("unknown child(ren): ",
                    paste(bad, collapse = ", "), call. = FALSE)
            }
            i
        } else {
            names(x)[i]
        }
        out <- x
        out@objects <- x@objects[sel]
        # rebuild id_map for the new child set
        out@id_sig <- list()
        initialize(out)
    }
)

#' @noRd
setReplaceMethod("names", signature(x = "giottoMulti", value = "character"),
    function(x, value) {
        old_names <- names(x@objects)
        if (length(value) != length(old_names)) {
            stop(sprintf(
                "names() <- requires length %d (got %d)",
                length(old_names), length(value)
            ), call. = FALSE)
        }
        if (anyDuplicated(value)) {
            stop("child names must be unique", call. = FALSE)
        }

        # Joint shared slots encode child names inside their globals
        # (sample::id colnames, cell_ID values, etc.). Renaming would
        # silently break those references — the view filter would then
        # see no overlap and return zero rows. Refuse rather than
        # corrupt. User must populate joint state AFTER renaming, or
        # drop the joint state first (setExpression(mg, NULL), etc.).
        populated_slots <- c("expression", "cell_metadata", "feat_metadata",
            "dimension_reduction", "nn_network", "spatial_enrichment")
        has_joint <- vapply(populated_slots, function(s) {
            v <- slot(x, s)
            !is.null(v) && length(v) > 0L
        }, logical(1L))
        if (any(has_joint)) {
            stop(wrap_txt(sprintf(
                "Cannot rename children of a giottoMulti with populated
                joint shared slots: %s. Joint content is keyed on the
                current child names; renaming would invalidate it
                silently. Rename children before populating joint state,
                or drop the joint slots first (e.g.
                setExpression(mg, NULL, ...) per entry).",
                paste(names(has_joint)[has_joint], collapse = ", ")
            )), call. = FALSE)
        }

        names(x@objects) <- value
        # id_map embeds the old names in object column AND in global_id;
        # full rebuild is the simplest correct path.
        x@id_sig <- list()
        initialize(x)
    }
)


# COMPACT ####

# SUBSET ####

#' @title Subset a giottoMulti
#' @name subset-giottoMulti
#' @description
#' Narrow the joint analysis view of the multi to a subset of global cell IDs
#' and/or global feature IDs. Eager: `@id_map` is narrowed and every populated
#' joint shared slot (`@expression`, `@cell_metadata`, `@feat_metadata`,
#' `@dimension_reduction`, `@nn_network`, `@spatial_enrichment`) is trimmed
#' in place to the surviving globals.
#'
#' Children (`@objects`) are the spatial axis and are not touched. If you
#' want narrowed spatial content on a specific child, do that explicitly on
#' the child.
#'
#' Subset returns a new `giottoMulti`; R copy-on-modify means the original is
#' untouched and acts as the "widen back" handle.
#' @param x a `giottoMulti`
#' @param cells `character` vector of global cell IDs to retain. `NULL` =
#'   no cell-level filter.
#' @param features `character` vector of global feature IDs to retain.
#'   `NULL` = no feature-level filter.
#' @param ... not used
#' @returns a `giottoMulti` with narrowed `@id_map` and trimmed joint slots
#' @export
setMethod("subset", "giottoMulti",
    function(x, cells = NULL, features = NULL, ...) {
        if (!is.null(cells)) {
            checkmate::assert_character(cells, any.missing = FALSE)
            m <- x@id_map$cells
            keep <- m$global_id %in% cells
            missing <- setdiff(cells, m$global_id)
            if (length(missing) > 0L) {
                warning(sprintf(
                    "%d requested cell global_id(s) not in id_map (ignored)",
                    length(missing)
                ), call. = FALSE)
            }
            x@id_map$cells <- m[keep, ]
        }
        if (!is.null(features)) {
            checkmate::assert_character(features, any.missing = FALSE)
            m <- x@id_map$feats
            keep <- m$global_id %in% features
            missing <- setdiff(features, m$global_id)
            if (length(missing) > 0L) {
                warning(sprintf(
                    "%d requested feature global_id(s) not in id_map (ignored)",
                    length(missing)
                ), call. = FALSE)
            }
            x@id_map$feats <- m[keep, ]
        }

        # Eagerly trim populated joint slots to the new id_map.
        joint_slots <- c("expression", "cell_metadata", "feat_metadata",
            "dimension_reduction", "nn_network", "spatial_enrichment")
        for (s in joint_slots) {
            v <- slot(x, s)
            if (!is.null(v)) slot(x, s) <- .gm_walk_apply_view(v, x)
        }

        x
    }
)

#' Recursively walk a nested list of joint-slot subobjects, trimming each
#' leaf to the current `@id_map` view. The shared slots are organized as
#' nested lists keyed by `[spat_unit][feat_type]` (and additional levels for
#' some, e.g. dim reduction's `[reduction][method][name]`).
#' @noRd
.gm_walk_apply_view <- function(node, gobject) {
    if (is.null(node)) return(node)
    if (inherits(node, "giottoSubobject")) {
        return(.gm_apply_view(node, gobject))
    }
    if (is.list(node)) {
        return(lapply(node, .gm_walk_apply_view, gobject))
    }
    node
}


# SHOW ####

#' @noRd
setMethod("show", "giottoMulti", function(object) {
    cat(sprintf("An object of class %s\n", class(object)))

    # children: cell + feature counts from each child's active default
    # (spatIDs / featIDs). Mirrors what id_map and the view counters use,
    # so the per-child totals sum to the global "total" below — avoiding
    # the double-counting that summing lengths(child@cell_ID) would do
    # when a child has cells in multiple spat_units.
    nms <- names(object)
    cat(sprintf("  %d child object(s):\n", length(object)))
    for (nm in nms) {
        g <- object@objects[[nm]]
        n_c <- length(tryCatch(spatIDs(g), error = function(e) character()))
        n_f <- length(tryCatch(featIDs(g), error = function(e) character()))
        cat(sprintf("    %s: %d cells, %d features\n", nm, n_c, n_f))
    }

    # view: subset filter state — visible / total reflects whether the user
    # has narrowed the view. Totals match the union of children's spatIDs /
    # featIDs (i.e. the unfiltered baseline).
    if (!is.null(object@id_map$cells) || !is.null(object@id_map$feats)) {
        n_c_vis <- if (!is.null(object@id_map$cells)) {
            nrow(object@id_map$cells)
        } else 0L
        n_c_total <- sum(vapply(object@objects, function(g) {
            length(tryCatch(spatIDs(g),
                error = function(e) character()))
        }, integer(1L)))

        per_child_feats <- lapply(object@objects, function(g) {
            tryCatch(featIDs(g), error = function(e) character())
        })
        n_f_vis <- if (!is.null(object@id_map$feats)) {
            length(unique(object@id_map$feats$global_id))
        } else 0L
        n_f_total <- length(unique(unlist(per_child_feats, use.names = FALSE)))

        c_flag <- if (n_c_vis < n_c_total) " (filtered)" else ""
        f_flag <- if (n_f_vis < n_f_total) " (filtered)" else ""
        cat(sprintf("  view: %d / %d cells%s, %d / %d features%s\n",
            n_c_vis, n_c_total, c_flag, n_f_vis, n_f_total, f_flag))

        # shared: how many features are simultaneously present in all active
        # children. This is what getExpression(mg) would return as features
        # when assembling from children. When all children share a panel the
        # count matches the view total; with mismatched panels it's smaller.
        f_intersect <- Reduce(intersect, per_child_feats)
        if (!is.null(object@id_map$feats)) {
            f_intersect <- intersect(f_intersect,
                unique(object@id_map$feats$global_id))
        }
        if (length(f_intersect) != n_f_total) {
            cat(sprintf("  shared: %d feature(s) common to all children\n",
                length(f_intersect)))
        }
    }

    # populated joint shared slots
    slot_check <- list(
        expression = "expression",
        cell_metadata = "cell_metadata",
        feat_metadata = "feat_metadata",
        dimension_reduction = "dimension_reduction",
        nn_network = "nn_network",
        spatial_enrichment = "spatial_enrichment"
    )
    populated <- vapply(names(slot_check), function(s) {
        v <- slot(object, s)
        !is.null(v) && length(v) > 0L
    }, logical(1L))
    if (any(populated)) {
        cat(sprintf("  joint slots: %s\n",
            paste(names(slot_check)[populated], collapse = ", ")))
    }

    invisible(NULL)
})


# INTERNAL HELPERS ####

#' Apply the giottoMulti id_map view to a joint-slot subobject.
#'
#' Shared-domain getter methods read the joint slot and pass the result here.
#' For a giottoMulti, this filters the subobject's per-cell axis (and/or
#' per-feature axis) down to whichever globals are currently in scope per
#' `@id_map`. For a single giotto this is a no-op — there is no id_map to
#' consult, and the subobject is already aligned with the gobject's cells.
#'
#' Filter axis by subobject class:
#' * `exprObj`              cells = matrix cols, feats = matrix rows
#' * `cellMetaObj` / `spatEnrObj` cells = cell_ID column
#' * `featMetaObj`          feats = feat_ID column
#' * `dimObj`               cells = coordinates row names
#'
#' Other subobject classes (nnNetObj, multiomics ...) pass through unfiltered
#' for now; wire them in as the joint use cases come up.
#' @noRd
.gm_apply_view <- function(x, gobject) {
    if (!inherits(gobject, "giottoMulti")) return(x)
    cells <- gobject@id_map$cells$global_id
    feats <- gobject@id_map$feats$global_id

    if (inherits(x, "exprObj")) {
        mat <- x[]
        if (!is.null(cells)) {
            keep <- colnames(mat) %in% cells
            mat <- mat[, keep, drop = FALSE]
        }
        if (!is.null(feats)) {
            keep <- rownames(mat) %in% feats
            mat <- mat[keep, , drop = FALSE]
        }
        x[] <- mat
        return(x)
    }

    if (inherits(x, c("cellMetaObj", "spatEnrObj"))) {
        if (!is.null(cells)) {
            cell_ID <- NULL  # data.table NSE
            dt <- x[]
            x[] <- dt[cell_ID %in% cells]
        }
        return(x)
    }

    if (inherits(x, "featMetaObj")) {
        if (!is.null(feats)) {
            feat_ID <- NULL  # data.table NSE
            dt <- x[]
            x[] <- dt[feat_ID %in% feats]
        }
        return(x)
    }

    if (inherits(x, "dimObj")) {
        if (!is.null(cells)) {
            coords <- x@coordinates
            keep <- rownames(coords) %in% cells
            x@coordinates <- coords[keep, , drop = FALSE]
        }
        return(x)
    }

    if (inherits(x, "nnNetObj")) {
        if (!is.null(cells)) {
            g <- x@igraph
            vnames <- names(igraph::V(g))
            keep <- vnames %in% cells
            x@igraph <- igraph::induced_subgraph(g, igraph::V(g)[keep])
        }
        return(x)
    }

    x
}

#' Assemble a joint expression matrix from children.
#'
#' Naive concat: pull each child's matching exprObj, rename columns to
#' sample::id, intersect features across children (safe default — avoids
#' introducing NAs), cbind. The first child's exprObj is used as the metadata
#' template (spat_unit, feat_type, name) with its matrix replaced.
#'
#' Nesting args (`spat_unit`, `feat_type`) are resolved per-child when NULL:
#' each child uses its own active default. When supplied explicitly they're
#' broadcast across all children (must match in each). The joint global
#' namespace is `sample::id` regardless — children with different per-child
#' spat_unit layouts still contribute correctly.
#'
#' Called by `getExpression(giottoMulti, ...)` when `@expression` is empty.
#' This is the baseline view; integration tools (Harmony, scVI, etc.) overwrite
#' it via `setExpression(mg, joint)` once they've produced a corrected matrix.
#' @noRd
.gm_assemble_expression <- function(gobject, spat_unit, feat_type, values) {
    children <- names(gobject)
    if (length(children) == 0L) {
        stop("giottoMulti has no children to assemble expression from",
            call. = FALSE)
    }

    user_su <- !is.null(spat_unit)
    user_ft <- !is.null(feat_type)

    # Per-child resolved nesting (used both for finding common `values` and
    # for the actual fetch loop below).
    resolved <- lapply(children, function(nm) {
        g <- gobject@objects[[nm]]
        su <- if (user_su) spat_unit
            else tryCatch(set_default_spat_unit(g), error = function(e) NA_character_)
        ft <- if (user_ft) feat_type
            else tryCatch(set_default_feat_type(g, spat_unit = su),
                error = function(e) NA_character_)
        list(su = su, ft = ft)
    })

    # When `values` is not given, pick the first name common to all children
    # under each child's resolved nesting.
    if (is.null(values)) {
        avail_per_child <- mapply(function(nm, r) {
            g <- gobject@objects[[nm]]
            tryCatch(
                list_expression_names(g, spat_unit = r$su, feat_type = r$ft),
                error = function(e) NULL
            )
        }, children, resolved, SIMPLIFY = FALSE)
        common <- Reduce(intersect, Filter(Negate(is.null), avail_per_child))
        if (length(common) == 0L) {
            stop(wrap_txt("No expression matrix name common to all
                children of giottoMulti. Specify `values =` explicitly,
                or run integration and setExpression() the result."),
                call. = FALSE)
        }
        values <- common[[1L]]
    }

    per_child <- mapply(function(nm, r) {
        g <- gobject@objects[[nm]]
        e <- tryCatch(getExpression(g,
            spat_unit = r$su, feat_type = r$ft,
            values = values,
            output = "exprObj", set_defaults = FALSE),
            error = function(err) NULL)
        if (is.null(e)) return(NULL)
        mat <- e[]
        colnames(mat) <- paste(nm, colnames(mat), sep = "::")
        list(mat = mat, exprObj = e, name = nm)
    }, children, resolved, SIMPLIFY = FALSE)
    per_child <- Filter(Negate(is.null), per_child)
    if (length(per_child) == 0L) {
        stop(sprintf(
            "No child has expression \"%s\" matching the requested nesting",
            values), call. = FALSE)
    }

    feats_common <- Reduce(intersect,
        lapply(per_child, function(x) rownames(x$mat)))
    if (length(feats_common) == 0L) {
        stop("No features common to all children for this expression set",
            call. = FALSE)
    }

    mats <- lapply(per_child, function(x) x$mat[feats_common, , drop = FALSE])
    joint_mat <- do.call(cbind, mats)

    # Use first child's exprObj as metadata template; the joint matrix's
    # spat_unit / feat_type tags inherit from that template, even though the
    # cells across children may have come from different per-child spat_units.
    # The global IDs (sample::id) disambiguate.
    template <- per_child[[1L]]$exprObj
    template[] <- joint_mat
    template
}

#' Assemble joint cell metadata from children's per-child cell metadata.
#'
#' Pulls each child's cellMetaObj for the resolved nesting, prefixes cell_ID
#' to globals (sample::id), and rbinds. Columns are intersected across
#' children to avoid NA inflation (the common case has matching schema; in
#' the worst case the intersection at minimum has cell_ID).
#'
#' Called by `getCellMetadata(giottoMulti, ...)` when the joint slot is
#' empty. The first child's cellMetaObj is the metadata template.
#' @noRd
.gm_assemble_cell_metadata <- function(gobject, spat_unit, feat_type) {
    children <- names(gobject)
    user_su <- !is.null(spat_unit)
    user_ft <- !is.null(feat_type)

    per_child <- lapply(children, function(nm) {
        g <- gobject@objects[[nm]]
        su <- if (user_su) spat_unit
            else tryCatch(set_default_spat_unit(g),
                error = function(e) NA_character_)
        ft <- if (user_ft) feat_type
            else tryCatch(set_default_feat_type(g, spat_unit = su),
                error = function(e) NA_character_)
        cm <- tryCatch(getCellMetadata(g, spat_unit = su, feat_type = ft,
            output = "cellMetaObj", set_defaults = FALSE),
            error = function(e) NULL)
        if (is.null(cm)) return(NULL)
        dt <- data.table::copy(cm[])
        dt[, cell_ID := paste(nm, cell_ID, sep = "::")]
        # Sample-origin tag — matches joinGiottoObjects' convention so
        # downstream tools (e.g. runGiottoHarmony's vars_use = "list_ID"
        # default) work out of the box on the assembled multi metadata.
        dt[, list_ID := nm]
        list(cm = cm, dt = dt)
    })
    per_child <- Filter(Negate(is.null), per_child)
    if (length(per_child) == 0L) {
        stop("No child has cell metadata for the requested nesting",
            call. = FALSE)
    }

    cols_common <- Reduce(intersect, lapply(per_child, function(x) names(x$dt)))
    dts <- lapply(per_child, function(x) x$dt[, cols_common, with = FALSE])
    joint_dt <- data.table::rbindlist(dts, use.names = TRUE)

    template <- per_child[[1L]]$cm
    template[] <- joint_dt
    template
}

#' Assemble joint feature metadata from children's per-child feat metadata.
#'
#' Parallel to .gm_assemble_cell_metadata. Feature IDs are passthrough
#' (no global namespacing), so the rbind happens directly; rows for the
#' same feature across children are deduplicated by `feat_ID` (first
#' child's row wins — typical assumption is shared panel).
#' @noRd
.gm_assemble_feat_metadata <- function(gobject, spat_unit, feat_type) {
    children <- names(gobject)
    user_su <- !is.null(spat_unit)
    user_ft <- !is.null(feat_type)

    per_child <- lapply(children, function(nm) {
        g <- gobject@objects[[nm]]
        su <- if (user_su) spat_unit
            else tryCatch(set_default_spat_unit(g),
                error = function(e) NA_character_)
        ft <- if (user_ft) feat_type
            else tryCatch(set_default_feat_type(g, spat_unit = su),
                error = function(e) NA_character_)
        fm <- tryCatch(getFeatureMetadata(g, spat_unit = su, feat_type = ft,
            output = "featMetaObj", set_defaults = FALSE),
            error = function(e) NULL)
        if (is.null(fm)) return(NULL)
        list(fm = fm, dt = data.table::copy(fm[]))
    })
    per_child <- Filter(Negate(is.null), per_child)
    if (length(per_child) == 0L) {
        stop("No child has feature metadata for the requested nesting",
            call. = FALSE)
    }

    cols_common <- Reduce(intersect, lapply(per_child, function(x) names(x$dt)))
    dts <- lapply(per_child, function(x) x$dt[, cols_common, with = FALSE])
    joint_dt <- unique(data.table::rbindlist(dts, use.names = TRUE),
        by = "feat_ID")

    template <- per_child[[1L]]$fm
    template[] <- joint_dt
    template
}

#' Compute a cheap length-signature of each child's ID slots.
#'
#' Used by `initialize(giottoMulti)` as a fast-path: if signatures match the
#' cached `@id_sig`, the id_map is up-to-date and we skip the rebuild. Catches
#' the realistic mutation modes (cells/features added or removed; child added
#' or replaced). Misses same-length-different-content edits, which are
#' user-error territory at the multi level.
#' @noRd
.gm_compute_sig <- function(objects) {
    lapply(objects, function(g) {
        list(
            cell = lengths(slot(g, "cell_ID")),
            feat = lengths(slot(g, "feat_ID"))
        )
    })
}

#' @noRd
.gm_build_cell_idmap <- function(objects, sep = "::") {
    parts <- lapply(names(objects), function(nm) {
        ids <- tryCatch(spatIDs(objects[[nm]]), error = function(e) character())
        if (length(ids) == 0L) return(NULL)
        data.table::data.table(
            object = nm,
            local_id = ids,
            global_id = paste(nm, ids, sep = sep)
        )
    })
    parts <- Filter(Negate(is.null), parts)
    if (length(parts) == 0L) return(NULL)
    data.table::rbindlist(parts)
}

#' @noRd
.gm_build_feat_idmap <- function(objects) {
    parts <- lapply(names(objects), function(nm) {
        ids <- tryCatch(featIDs(objects[[nm]]), error = function(e) character())
        if (length(ids) == 0L) return(NULL)
        # default passthrough: feature names are the same global vocabulary
        data.table::data.table(
            object = nm,
            local_id = ids,
            global_id = ids
        )
    })
    parts <- Filter(Negate(is.null), parts)
    if (length(parts) == 0L) return(NULL)
    data.table::rbindlist(parts)
}

#' Resolve which children a per-object method should operate on.
#'
#' @param x giottoMulti
#' @param object NULL (all children), or character vector of object names
#' @returns character vector of object names
#' @noRd
.gm_resolve_objects <- function(x, object = NULL) {
    if (is.null(object)) return(names(x))
    checkmate::assert_character(object)
    bad <- setdiff(object, names(x))
    if (length(bad) > 0L) {
        stop("unknown object(s): ", paste(bad, collapse = ", "), call. = FALSE)
    }
    object
}


# MULTI-SPECIFIC ACCESSORS ####

#' @title id_map accessor for giottoMulti
#' @name idMap
#' @description
#' Return the `(object, local_id, global_id)` mapping for cells or features.
#' @param x a `giottoMulti`
#' @param which one of `"cells"` or `"feats"`
#' @returns a `data.table` or `NULL`
#' @export
setGeneric("idMap", function(x, ...) standardGeneric("idMap"))

#' @rdname idMap
#' @export
setMethod("idMap", "giottoMulti", function(x, which = c("cells", "feats"), ...) {
    which <- match.arg(which, c("cells", "feats"))
    x@id_map[[which]]
})


# spatIDs / featIDs — return GLOBAL ids from id_map ####

#' @rdname spatIDs-generic
#' @export
setMethod(
    "spatIDs", signature(x = "giottoMulti"),
    function(x, object = NULL, local = FALSE, ...) {
        m <- x@id_map$cells
        if (is.null(m) || nrow(m) == 0L) return(character())
        if (!is.null(object)) {
            target <- .gm_resolve_objects(x, object)
            # pre-compute keep to avoid data.table column-name shadowing
            keep <- m$object %in% target
            m <- m[keep, ]
        }
        if (isTRUE(local)) return(m$local_id)
        m$global_id
    }
)

#' @rdname spatIDs-generic
#' @export
setMethod(
    "featIDs", signature(x = "giottoMulti"),
    function(x, object = NULL, local = FALSE, uniques = TRUE, ...) {
        m <- x@id_map$feats
        if (is.null(m) || nrow(m) == 0L) return(character())
        if (!is.null(object)) {
            target <- .gm_resolve_objects(x, object)
            keep <- m$object %in% target
            m <- m[keep, ]
        }
        ids <- if (isTRUE(local)) m$local_id else m$global_id
        if (isTRUE(uniques)) unique(ids) else ids
    }
)


# SHARED-DOMAIN OVERRIDES ON giottoMulti ####

#' @rdname getExpression
#' @export
setMethod("getExpression", "giottoMulti",
    function(gobject, values = NULL, spat_unit = NULL, feat_type = NULL,
             output = c("exprObj", "matrix"), set_defaults = TRUE) {
        output <- match.arg(output, choices = c("exprObj", "matrix"))

        # Capture before default resolution so the assembly path can tell
        # user-supplied (broadcast to every child) from defaulted (let each
        # child resolve its own active spat_unit / feat_type — children may
        # have different spat_unit layouts).
        nospec_unit <- is.null(spat_unit)
        nospec_feat <- is.null(feat_type)

        if (isTRUE(set_defaults)) {
            .set_default_nesting(gobject, spat_unit, feat_type)
        }

        # Joint slot populated for this (spat_unit, feat_type)? The slot is
        # authoritative — eager subset trims it in place. Defer to gAny, which
        # reads the slot directly.
        joint_avail <- list_expression_names(gobject,
            spat_unit = spat_unit, feat_type = feat_type)
        target_values <- if (is.null(values)) {
            if (length(joint_avail) > 0L) joint_avail[[1L]] else NULL
        } else values
        if (!is.null(target_values) && target_values %in% joint_avail) {
            return(callNextMethod(gobject, values = target_values,
                spat_unit = spat_unit, feat_type = feat_type,
                output = output, set_defaults = FALSE))
        }

        # Joint slot empty — assemble naive joint from children, prefix to
        # globals, intersect features. Assembly is wide; intersect with the
        # (possibly narrower) @id_map at the end so post-subset reads honor
        # the current view. Integration output overrides assembly via
        # setExpression(mg, ...) — that's the materialization entry point.
        e <- .gm_assemble_expression(gobject,
            spat_unit = if (nospec_unit) NULL else spat_unit,
            feat_type = if (nospec_feat) NULL else feat_type,
            values = values)
        e <- .gm_apply_view(e, gobject)
        if (output == "matrix") return(e[])
        e
    }
)

#' @rdname getCellMetadata
#' @export
setMethod("getCellMetadata", "giottoMulti", function(gobject,
    spat_unit = NULL,
    feat_type = NULL,
    output = c("cellMetaObj", "data.table"),
    copy_obj = TRUE,
    set_defaults = TRUE) {
    output <- match.arg(output, choices = c("cellMetaObj", "data.table"))
    nospec_unit <- is.null(spat_unit)
    nospec_feat <- is.null(feat_type)
    if (isTRUE(set_defaults)) {
        .set_default_nesting(gobject, spat_unit, feat_type)
    }

    # Joint slot populated? Defer to gAny — slot is the authoritative source
    # (eager subset trims in place).
    joint <- gobject@cell_metadata[[spat_unit]][[feat_type]]
    if (inherits(joint, "cellMetaObj")) {
        return(callNextMethod(gobject,
            spat_unit = spat_unit, feat_type = feat_type,
            output = output, copy_obj = copy_obj, set_defaults = FALSE))
    }

    # Empty — assemble from children, then intersect with @id_map so any
    # narrowing from subset() is honored. setCellMetadata(mg, ...) is the
    # materialization entry point.
    cm <- .gm_assemble_cell_metadata(gobject,
        spat_unit = if (nospec_unit) NULL else spat_unit,
        feat_type = if (nospec_feat) NULL else feat_type)
    cm <- .gm_apply_view(cm, gobject)
    if (output == "data.table") return(cm[])
    cm
})

#' @rdname getFeatureMetadata
#' @export
setMethod("getFeatureMetadata", "giottoMulti", function(gobject,
    spat_unit = NULL,
    feat_type = NULL,
    output = c("featMetaObj", "data.table"),
    copy_obj = TRUE,
    set_defaults = TRUE) {
    output <- match.arg(output, choices = c("featMetaObj", "data.table"))
    nospec_unit <- is.null(spat_unit)
    nospec_feat <- is.null(feat_type)
    if (isTRUE(set_defaults)) {
        .set_default_nesting(gobject, spat_unit, feat_type)
    }

    # Joint slot populated? Slot is authoritative; defer to gAny.
    joint <- gobject@feat_metadata[[spat_unit]][[feat_type]]
    if (inherits(joint, "featMetaObj")) {
        return(callNextMethod(gobject,
            spat_unit = spat_unit, feat_type = feat_type,
            output = output, copy_obj = copy_obj, set_defaults = FALSE))
    }

    # Empty — assemble, then intersect with @id_map.
    fm <- .gm_assemble_feat_metadata(gobject,
        spat_unit = if (nospec_unit) NULL else spat_unit,
        feat_type = if (nospec_feat) NULL else feat_type)
    fm <- .gm_apply_view(fm, gobject)
    if (output == "data.table") return(fm[])
    fm
})


# SPATIAL-DOMAIN METHODS — per-child dispatch ####
#
# Getters: `object = NULL` (default) routes to all children and returns a
# named list of per-child results. Pass a character vector of names to scope
# to specific children. Children are returned as-is — subset() on the multi
# narrows the joint analysis view, not children's spatial state.
#
# Setters: `object` must name exactly one child. Setting "broadcast"
# semantics across children would silently duplicate spatial data and
# is almost never what the caller means; require an explicit target.

#' @noRd
.gm_set_target <- function(gobject, object) {
    if (missing(object) || is.null(object)) {
        stop("`object` must name the child to write into", call. = FALSE)
    }
    if (length(object) != 1L) {
        stop("`object` must be length 1 for setters on a giottoMulti",
            call. = FALSE)
    }
    .gm_resolve_objects(gobject, object)
}

#' @rdname getSpatialLocations
#' @export
setMethod("getSpatialLocations", signature("giottoMulti"),
    function(gobject, object = NULL, ...) {
        objs <- .gm_resolve_objects(gobject, object)
        out <- lapply(objs, function(nm) {
            getSpatialLocations(gobject@objects[[nm]], ...)
        })
        names(out) <- objs
        out
    }
)

#' @rdname setSpatialLocations
#' @export
setMethod("setSpatialLocations", signature("giottoMulti"),
    function(gobject, x, object = NULL, ...) {
        nm <- .gm_set_target(gobject, object)
        gobject@objects[[nm]] <- setSpatialLocations(
            gobject@objects[[nm]], x = x, ...)
        gobject
    }
)

#' @rdname getSpatialNetwork
#' @export
setMethod("getSpatialNetwork", signature("giottoMulti"),
    function(gobject, object = NULL, ...) {
        objs <- .gm_resolve_objects(gobject, object)
        out <- lapply(objs, function(nm) {
            getSpatialNetwork(gobject@objects[[nm]], ...)
        })
        names(out) <- objs
        out
    }
)

#' @rdname setSpatialNetwork
#' @export
setMethod("setSpatialNetwork", signature("giottoMulti"),
    function(gobject, x, object = NULL, ...) {
        nm <- .gm_set_target(gobject, object)
        gobject@objects[[nm]] <- setSpatialNetwork(
            gobject@objects[[nm]], x = x, ...)
        gobject
    }
)

#' @rdname getPolygonInfo
#' @export
setMethod("getPolygonInfo", signature("giottoMulti"),
    function(gobject, object = NULL, ...) {
        objs <- .gm_resolve_objects(gobject, object)
        out <- lapply(objs, function(nm) {
            getPolygonInfo(gobject@objects[[nm]], ...)
        })
        names(out) <- objs
        out
    }
)

#' @rdname setPolygonInfo
#' @export
setMethod("setPolygonInfo", signature("giottoMulti"),
    function(gobject, x, object = NULL, ...) {
        nm <- .gm_set_target(gobject, object)
        gobject@objects[[nm]] <- setPolygonInfo(
            gobject@objects[[nm]], x = x, ...)
        gobject
    }
)

#' @rdname getFeatureInfo
#' @export
setMethod("getFeatureInfo", signature("giottoMulti"),
    function(gobject, object = NULL, ...) {
        objs <- .gm_resolve_objects(gobject, object)
        out <- lapply(objs, function(nm) {
            getFeatureInfo(gobject@objects[[nm]], ...)
        })
        names(out) <- objs
        out
    }
)

#' @rdname setFeatureInfo
#' @export
setMethod("setFeatureInfo", signature("giottoMulti"),
    function(gobject, x, object = NULL, ...) {
        nm <- .gm_set_target(gobject, object)
        gobject@objects[[nm]] <- setFeatureInfo(
            gobject@objects[[nm]], x = x, ...)
        gobject
    }
)

#' @rdname getGiottoImage
#' @export
setMethod("getGiottoImage", signature("giottoMulti"),
    function(gobject, object = NULL, ...) {
        objs <- .gm_resolve_objects(gobject, object)
        out <- lapply(objs, function(nm) {
            getGiottoImage(gobject@objects[[nm]], ...)
        })
        names(out) <- objs
        out
    }
)

#' @rdname setGiottoImage
#' @export
setMethod("setGiottoImage", signature("giottoMulti"),
    function(gobject, image, object = NULL, ...) {
        nm <- .gm_set_target(gobject, object)
        gobject@objects[[nm]] <- setGiottoImage(
            gobject@objects[[nm]], image = image, ...)
        gobject
    }
)