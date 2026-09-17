#' @include classes-resolver.R
#' @include classes-view.R
#' @include classes-space.R
#' @include classes.R
NULL

# =============================================================================
# methods-resolver.R — resolveSubobject generic + dataTableCoordinator methods
#
# resolveSubobject(subobj, gobject, view, space, coordinator, ...) takes one
# subobject, the parent gobject (for spatValues lookups), and the
# view+space+coordinator. Returns a new subobject projected through the
# recipe.
#
# The `coordinator` is a `viewCoordinator`-inheriting object that brokers
# IDs and joins between storage backings during resolution — see
# `R/classes-resolver.R` for the protocol notes.
#
# Cell-set narrowing is computed once via `.surviving_cell_ids()`; tabular
# subobjects narrow by that set, spatial subobjects narrow + apply
# transforms via existing eager GiottoClass dispatch.
# =============================================================================


# Generic ####

#' @title resolveSubobject
#' @name resolveSubobject
#' @description Apply a [giottoView] (and optional [giottoSpace])
#' recipe to a single subobject, returning the projected subobject. Dispatch
#' is on `(subobj, coordinator)` so different storage-bridging coordinators
#' register different methods.
#'
#' @param subobj a giotto subobject (e.g. `cellMetaObj`, `spatLocsObj`, ...)
#' @param gobject the parent `giotto` (needed for cross-slot lookups via
#'   `spatValues()`)
#' @param view a [giottoView] or `NULL`
#' @param space a [giottoSpace] or `NULL`
#' @param coordinator a [viewCoordinator-class]-inheriting object brokering
#'   IDs and joins between storage backings
#' @param ... reserved for backend-specific args
#' @returns the projected subobject (same class as `subobj`)
#' @export
setGeneric("resolveSubobject",
    function(subobj, gobject, view, space, coordinator, ...)
        standardGeneric("resolveSubobject"))


# Coordinator protocol ####

#' @title prepareIds
#' @name prepareIds
#' @description Coordinator-side protocol method: promote an R-memory
#' cell_ID character vector into the form the coordinator's preferred
#' backend uses for filtering. For [dataTableCoordinator-class] this is
#' the identity transform; downstream coordinators (e.g. duckDB / sedona
#' from GiottoDisk) register methods that perform ephemeral table
#' registration or similar.
#'
#' @param coordinator a [viewCoordinator-class]-inheriting object
#' @param ids character vector of cell_IDs
#' @param ... reserved
#' @returns the prepared IDs in the coordinator's preferred form
#' @export
setGeneric("prepareIds",
    function(coordinator, ids, ...) standardGeneric("prepareIds"))

#' @rdname prepareIds
#' @export
setMethod("prepareIds", signature(coordinator = "dataTableCoordinator"),
    function(coordinator, ids, ...) ids
)


# Helpers ####

#' @title defaultViewCoordinator
#' @name defaultViewCoordinator
#' @description Pick the default [viewCoordinator-class] for resolving views
#' on a `gobject` whose source is `source`. GiottoClass provides the
#' `ANY`-signature method returning [dataTableCoordinator-class] (in-memory
#' reference). Downstream packages register their own coordinators by
#' adding methods for their source class — e.g. GiottoDisk registers a
#' `gsource` method returning `parquetCoordinator()`. S4 inheritance picks
#' up subclasses automatically.
#'
#' @param source the `@source` slot of the gobject (`NULL` is handled
#'   upstream by `.default_view_coordinator()`)
#' @param ... reserved
#' @returns a `viewCoordinator`-inheriting object
#' @export
setGeneric("defaultViewCoordinator",
    function(source, ...) standardGeneric("defaultViewCoordinator"))

#' @rdname defaultViewCoordinator
#' @export
setMethod("defaultViewCoordinator", signature(source = "ANY"),
    function(source, ...) dataTableCoordinator()
)

# Resolve which `coordinator` to use when the caller doesn't pass one
# explicitly. In-memory by default for sourceless gobjects; otherwise
# dispatch on the source class via [defaultViewCoordinator()].
#' @keywords internal
#' @noRd
.default_view_coordinator <- function(gobject) {
    src <- gobject@source
    if (is.null(src)) return(dataTableCoordinator())
    defaultViewCoordinator(src)
}

# Resolve the samples step into the set of children participating. For a
# plain giotto, sample selection is a no-op (returns NA to signal "no
# multi-scope"); for giottoMulti, returns the intersection of selected names
# with available children.
#' @keywords internal
#' @noRd
.resolve_sample_select <- function(gobject, view) {
    ss_steps <- .view_steps_of(view, "samples")
    if (length(ss_steps) == 0L) return(NA)
    if (!inherits(gobject, "giottoMulti")) {
        warning(call. = FALSE,
            "selectSamples step ignored: parent is not a giottoMulti")
        return(NA)
    }
    sel <- unique(unlist(lapply(ss_steps, function(s) s$samples)))
    avail <- names(gobject@objects)
    bad <- setdiff(sel, avail)
    if (length(bad) > 0L) {
        warning(call. = FALSE, sprintf(
            "selectSamples: missing children ignored (%s)",
            paste(bad, collapse = ", ")))
    }
    intersect(sel, avail)
}

# Free-var names in `predicate` that name data columns — these are what
# need pulling via spatValues.
#
# The predicate arrives self-contained: `.eager_substitute_env()` inlined
# every env-resident value at record time, and `all.vars()` does not report
# function names in call position. So what is left is columns, plus (rarely)
# a function passed as a value, which is filtered out here.
#
# NSE caveat: a column whose name collides with a function's (`c`, `mean`)
# is treated as the function. Same ambiguity as dplyr's `mutate(df, x = x)`.
#' @keywords internal
#' @noRd
.predicate_column_refs <- function(predicate) {
    Filter(
        function(v) !exists(v, envir = globalenv(), mode = "function",
            inherits = TRUE),
        all.vars(predicate)
    )
}

# Evaluate one filter step against the parent gobject via spatValues and
# return the surviving cell_ID vector. Predicates that reference columns not
# co-existing in a single artifact raise via spatValues' own contract.
#
# The predicate is stored deparsed, so it is parsed here. `globalenv()` is
# the evaluation enclosure: the step carries no environment by design (Q7),
# and functions resolve from there through the attached-package chain.
#' @keywords internal
#' @noRd
.eval_view_filter <- function(step, gobject) {
    pred <- str2lang(step$predicate)
    cols <- .predicate_column_refs(pred)
    if (length(cols) == 0L) {
        # purely constant predicate; pull all cell_IDs and let eval decide
        cell_ids <- spatIDs(gobject)
        keep <- eval(pred, envir = list(), enclos = globalenv())
        return(if (isTRUE(keep)) cell_ids else character())
    }
    sv_args <- c(list(gobject = gobject, feats = cols),
        step$scope_args %||% list())
    sv <- do.call(spatValues, sv_args)
    keep <- eval(pred, envir = sv, enclos = globalenv())
    if (!is.logical(keep)) {
        stop("view filter predicate did not evaluate to a logical vector: ",
            step$predicate, call. = FALSE)
    }
    sv[["cell_ID"]][which(keep)]
}

# Normalise the explicit `space` argument to a giottoSpace (or NULL).
# Accepts: NULL (no space), a giottoSpace, or a character name to look up
# on the gobject.
#
# IMPORTANT: this only resolves the *output* space -- the frame that
# transforms get applied to on returned data. The *predicate* frame
# (how a recorded crop region is interpreted) lives on the view's `space`
# field and is consulted directly by the crop step handlers. Conflating the
# two was the original bug that caused `getSpatialLocations(g, view =
# "test")` to silently return rotated coords whenever the view was bound to
# a space.
#' @keywords internal
#' @noRd
.resolve_space <- function(gobject, space = NULL) {
    if (is.null(space)) return(NULL)
    # already-resolved recipe passed through internally
    if (inherits(space, "giottoSpace")) return(space)
    if (is.character(space)) return(giottoSpace(gobject, space))
    stop("`space` must be NULL, a character(1) name, or a giottoSpace",
        call. = FALSE)
}

# Apply a giottoSpace's transforms to a subobject via existing eager
# GiottoClass dispatch. Each transform step becomes
# `do.call(op, c(list(x = subobj), args))`.
#
# `sample` names the sample identity of `subobj` -- a gmulti child's name,
# or `NA_character_` for a plain `giotto`, which is one sample with no
# name. `[[` owns the resolution of that name against the recipe, so this
# walks whatever it hands back and no rule is re-implemented here.
#' @keywords internal
#' @noRd
.apply_space_to_subobj <- function(subobj, gobject, space, coordinator,
                                   sample = NA_character_) {
    if (is.null(space)) return(subobj)
    for (step in space[[sample]]) {
        subobj <- do.call(step$op, c(list(x = subobj), step$args))
    }
    subobj
}

# Crop routing: relation decides, not storage kind ####
#
# A crop step narrows the cell set, which means reducing each cell to a
# geometry and testing it against the region. Which geometry is DECLARED on
# the step as `geom` (see `.view_crop_geoms` in classes-view.R), not
# inferred here:
#
#   geom = "centroid"  test the cell's `spatial_locs` row. Cheap, and the
#                      conventional choice in spatial omics —
#                      `combineCellData()` documents the same for an
#                      intersects-style test. Approximate: a cell whose
#                      polygon straddles the region boundary with its
#                      centroid outside is dropped.
#   geom = "poly"      test the cell's actual polygon. Exact; needs a
#                      polygon source on the object.
#
# Reading the declaration rather than deriving it from the relation is what
# lets a recorded recipe state which question it asks, and lets the backed
# resolvers in {GiottoDisk} route on the same field instead of on target
# storage kind. Storage is not a discriminator: a geom-mode predicate
# evaluates against the gobject's polygon source and the resulting cell_ID
# set narrows any target downstream.

# Fetch the polygon source for `geom = "poly"`, in the predicate frame.
#
# Returns a `giottoPolygon`, not a bare SpatVector, so `spatRelate()`
# dispatches on it — the whole point of routing the geom arm through that
# generic is that one call covers terra here and sedona/duckdb on a store.
#
# giottoMulti: polygons live per child, so each child's are fetched,
# space-scoped, and their poly_IDs prefixed to `sample::id` to match the
# joint cell vocabulary — the same shape `.get_projected_spatlocs()`
# produces for the centroid arm.
#' @keywords internal
#' @noRd
.get_projected_polys <- function(gobject, space, coordinator,
                                 spat_unit = NULL) {
    one <- function(g, samp) {
        gp <- tryCatch(
            getPolygonInfo(g, name = spat_unit,
                return_giottoPolygon = TRUE, verbose = FALSE),
            error = function(e) NULL)
        if (!inherits(gp, "giottoPolygon")) return(NULL)
        .apply_space_to_subobj(gp, g, space, coordinator, sample = samp)
    }

    if (inherits(gobject, "giottoMulti")) {
        parts <- lapply(names(gobject@objects), function(nm) {
            gp <- one(gobject@objects[[nm]], nm)
            if (is.null(gp)) return(NULL)
            sv <- gp@spatVector
            sv$poly_ID <- paste(nm, terra::values(sv)$poly_ID,
                sep = .gm_id_sep)
            gp@spatVector <- sv
            gp@unique_ID_cache <- terra::values(sv)$poly_ID
            gp
        })
        parts <- Filter(Negate(is.null), parts)
        if (length(parts) == 0L) return(NULL)
        if (length(parts) == 1L) return(parts[[1L]])
        return(do.call(rbind, parts))
    }
    one(gobject, NA_character_)
}

# Route one crop step and return its surviving cell_IDs.
#
# Two arms, selected by the step's DECLARED `geom` — no relation
# inspection. A declared `geom = "poly"` with no polygon source on the
# object is a loud error: the caller asked for the geometric question, and
# silently answering the centroid one instead is the failure mode this
# whole design exists to remove.
#' @keywords internal
#' @noRd
.cells_in_crop_step <- function(gobject, step, carriers, coordinator,
                                spat_unit = NULL) {
    region <- .materialize_crop_region(step$region)
    if (is.null(region)) return(NULL)
    switch(step$geom,
        centroid = {
            pts <- carriers$points(step$space)
            if (is.null(pts)) return(NULL)
            spatRelate(pts, region, relation = step$relation)$cell_ID
        },
        poly = {
            polys <- carriers$polys(step$space)
            if (is.null(polys)) {
                stop(sprintf(paste0(
                    "[crop] geom = \"poly\" was requested (relation '%s'), ",
                    "but this object has no polygon source to evaluate it ",
                    "on.\nEither add polygons (`setPolygonInfo()`) or use ",
                    "geom = \"centroid\"."),
                    step$relation), call. = FALSE)
            }
            spatIDs(spatRelate(polys, region, relation = step$relation))
        },
        stop("[crop] unknown geom '", step$geom, "'", call. = FALSE)
    )
}

# Carriers for the crop arms, built lazily and memoised PER FRAME.
#
# Per frame, because the frame is a property of the step: two crop steps in
# one view may name different spaces, and each needs its geometry projected
# into its own. Steps sharing a frame -- the common case, and the only case
# before the frame moved onto the step -- share one build.
#
# A `NULL` build is a real answer ("this object has no spatial locations"),
# so it is cached too and the warning fires once per frame rather than once
# per step.
#' @keywords internal
#' @noRd
.crop_carriers <- function(gobject, coordinator, spat_unit = NULL) {
    memo <- new.env(parent = emptyenv())
    memoised <- function(kind, space_name, build) {
        key <- paste0(kind, ":", if (is.na(space_name)) "" else space_name)
        if (!exists(key, envir = memo, inherits = FALSE)) {
            sp <- if (is.na(space_name)) NULL else {
                .resolve_space(gobject, space_name)
            }
            assign(key, build(sp), envir = memo)
        }
        base::get(key, envir = memo)
    }
    list(
        points = function(space_name) {
            out <- memoised("pts", space_name, function(sp) {
                .get_projected_spatlocs(gobject, sp, coordinator)
            })
            if (is.null(out)) {
                warning("crop step skipped: no spatial locations available",
                    call. = FALSE)
            }
            out
        },
        polys = function(space_name) {
            memoised("poly", space_name, function(sp) {
                .get_projected_polys(gobject, sp, coordinator,
                    spat_unit = spat_unit)
            })
        }
    )
}

# Pull the gobject's spatLocs (active spat_unit) as a points `SpatVector` in
# the predicate frame, optionally applying the relevant space's transforms
# first. Consumed by `.cells_in_crop_step()`'s centroid arm.
#
# A bare points `SpatVector` is the common representation the predicate
# primitive works on, and `as.points()` passes the whole coordinate table
# through, so `cell_ID` rides along as an attribute and survivors are read
# off by ID rather than recovered positionally.
#
# giottoMulti: getSpatialLocations returns a per-child named list (spatial
# locations live per-child, no joint slot). Scope the space to each child,
# apply, promote each child's IDs to the joint vocabulary, then fold with
# `rbind2()` -- a data.table rbind -- and convert ONCE at the end. Folding
# first costs one terra allocation instead of one per child. Promote before
# folding, or `.check_id_dups()` fires on IDs the children share.
#' @keywords internal
#' @noRd
.get_projected_spatlocs <- function(gobject, space, coordinator) {
    sl <- .gm_fused_spatlocs(gobject, space, coordinator)
    if (is.null(sl)) return(NULL)
    as.points(sl)
}

# The fold itself: every child's locations, space-scoped and promoted to the
# joint `sample::id` vocabulary, folded into ONE `spatLocsObj`.
#
# Split out of `.get_projected_spatlocs()` because the fused object is worth
# more than the points it was being converted into. A cross-sample spatial
# network needs exactly this -- one coordinate table spanning samples in a
# shared frame, with globally unique IDs -- and the crop carrier is just one
# consumer that happens to want it as points.
#
# `samples =` narrows to a frame's members. That is a sample selector on a
# READER, which adr/0006 permits: the caller gets a value it can widen by
# asking differently, and nothing is persisted. Multi-only, matching the
# getters -- a plain `giotto` has no such formal at all.
#
# It runs through `.gm_resolve_samples()` here and again inside the getter.
# That is two calls to ONE authority, not two implementations: the second is
# an idempotent re-check of literal child names. The first exists only
# because it has to happen outside the tryCatch (see below), and paying it
# is cheaper than the alternative -- a local membership test, which is
# exactly the shape of the five copied `samples =` checks stage 7 removed.
#
# The space is NOT handed to the getter, and cannot be: a gmulti's frames
# are slotted on the PARENT, while the getter forwards `...` to each child,
# so `getSpatialLocations(mg, space = "atlas")` resolves "atlas" against a
# child that has no such frame and errors. Each child's chain is applied
# here instead, which makes this the second path -- after `materialize()` --
# that scopes a frame across a multi correctly.
#
# Order is the content: the space applies per child (each sample has its own
# chain, which cannot be expressed once they are one table), then IDs are
# promoted to `sample::id` (children share local IDs, so `rbind2()`'s
# `.check_id_dups()` fires if the fold goes first), then one fold.
#' @keywords internal
#' @noRd
.gm_fused_spatlocs <- function(gobject, space, coordinator,
    spat_unit = NULL, name = NULL, samples = NULL) {
    cell_ID <- NULL  # NSE
    is_multi <- inherits(gobject, "giottoMulti")
    if (!is.null(samples) && !is_multi) {
        stop("[gmulti fused spatlocs] `samples =` is only meaningful on a ",
            "giottoMulti", call. = FALSE)
    }
    # Resolve BEFORE the fetch. The tryCatch below absorbs "this object has
    # no spatial locations", which is a legitimate answer -- but it would
    # equally absorb a typo or a stale group member, turning a loud error
    # into a silently smaller fold. Only the fetch may fail quietly.
    if (is_multi && !is.null(samples)) {
        samples <- .gm_resolve_samples(gobject, samples,
            "gmulti fused spatlocs")
    }
    args <- list(gobject, spat_unit = spat_unit, name = name,
        output = "spatLocsObj")
    if (is_multi) args$samples <- samples
    sl <- tryCatch(do.call(getSpatialLocations, args),
        error = function(e) NULL)
    if (is.null(sl)) return(NULL)

    if (is.list(sl) && !inherits(sl, "spatLocsObj")) {
        parts <- lapply(names(sl), function(nm) {
            child_sl <- sl[[nm]]
            if (!inherits(child_sl, "spatLocsObj")) return(NULL)
            child_sl <- .apply_space_to_subobj(child_sl, gobject,
                space, coordinator, sample = nm)
            dt <- data.table::copy(child_sl[])
            dt[, cell_ID := .gm_global_cell_ids(gobject, nm, cell_ID)]
            child_sl[] <- dt
            child_sl
        })
        parts <- Filter(Negate(is.null), parts)
        if (length(parts) == 0L) return(NULL)
        sl <- Reduce(rbind2, parts)
    } else if (!is.null(space)) {
        sl <- .apply_space_to_subobj(sl, gobject, space, coordinator)
    }
    sl
}

# JIT helper for getters: apply view/space projection to a single subobject
# fetched by an accessor. Returns the subobject unchanged if neither view
# nor space is supplied. Normalises character `view` / `space` lookups
# against the gobject; picks the default resolver from `gobject@source`.
#
# Use at the tail of a getter:
#   obj <- getterLogic(...)
#   obj <- .apply_view_space(obj, gobject, view, space)
#   return(obj)
#' @keywords internal
#' @noRd
.apply_view_space <- function(subobj, gobject, view = NULL, space = NULL,
                              coordinator = NULL) {
    if (is.null(view) && is.null(space)) return(subobj)
    # view contract: character(1) name of a slotted view, or NULL.
    # Inline giottoView objects were considered and rejected — views
    # are curated artifacts; build + slot via giottoView<-(g, name) <- v
    # if programmatic composition is needed. See
    # vignettes/articles/DESIGN_gmulti_federation.md for the reasoning.
    if (!is.null(view)) {
        checkmate::assert_string(view, .var.name = "view")
    }
    v <- if (is.null(view)) NULL else giottoView(gobject, view)
    # `space` here is the OUTPUT frame -- the predicate frame (the view's `space`)
    # is consulted independently by the crop step handlers below.
    s <- .resolve_space(gobject, space)
    co <- if (is.null(coordinator)) .default_view_coordinator(gobject)
        else coordinator
    resolveSubobject(subobj, gobject, v, s, co)
}


# Per-call cache for expensive computations (currently just the surviving
# cell_ID set). Created by materialize() at entry; threaded through
# resolveSubobject via `.cache` in `...`. Each materialize call gets a
# fresh env; JIT getter calls that don't pass a cache just recompute.
#
# Scope: per-materialize-call. Persistent caching across calls needs a
# version-stamp invalidation scheme (deferred).
#' @keywords internal
#' @noRd
.new_resolver_cache <- function() new.env(parent = emptyenv())

# Memoising wrapper around .surviving_cell_ids. Reads/writes through `cache`
# if supplied; falls back to a direct call when cache is NULL.
#' @keywords internal
#' @noRd
.cached_surviving_cell_ids <- function(gobject, view, coordinator,
                                       cache = NULL) {
    if (is.null(cache)) {
        return(.surviving_cell_ids(gobject, view, coordinator))
    }
    if (exists("surviving_ids", envir = cache, inherits = FALSE)) {
        return(get("surviving_ids", envir = cache))
    }
    ids <- .surviving_cell_ids(gobject, view, coordinator)
    assign("surviving_ids", ids, envir = cache)
    ids
}

# Compute the cell_ID set that survives a view's filter + crop steps.
# Returns a character vector of cell_IDs; NULL means "no narrowing" (all
# cells survive).
#
# The PREDICATE frame for crop steps is the view's `space` -- the frame the
# crop region was drawn in. This is independent of any output space the
# caller may have requested via the explicit `space=` arg, which is why
# this helper does not take a `space` argument.
#' @keywords internal
#' @noRd
.surviving_cell_ids <- function(gobject, view, coordinator) {
    if (is.null(view)) return(NULL)

    filter_steps <- .view_steps_of(view, "filter")
    crop_steps   <- .view_steps_of(view, "crop")

    if (length(filter_steps) == 0L && length(crop_steps) == 0L) return(NULL)

    surviving <- spatIDs(gobject)
    for (step in filter_steps) {
        keep <- .eval_view_filter(step, gobject)
        surviving <- intersect(surviving, keep)
    }
    if (length(crop_steps) > 0L) {
        # Carriers are built lazily and per frame, so an all-poly recipe on
        # a polygon-only object never asks for spatial locations and never
        # warns about their absence.
        carriers <- .crop_carriers(gobject, coordinator)
        for (step in crop_steps) {
            keep <- .cells_in_crop_step(gobject, step, carriers, coordinator)
            # NULL = this step could not be evaluated (no region recorded,
            # or no centroid source), which is a skip rather than an empty
            # result.
            if (is.null(keep)) next
            surviving <- intersect(surviving, keep)
        }
    }
    surviving
}

# Apply every recorded crop step geometrically to a non-cell-keyed
# subobject (points, images), in the post-transform frame.
#' @keywords internal
#' @noRd
.apply_crops_geometrically <- function(subobj, view) {
    for (step in .view_steps_of(view, "crop")) {
        subobj <- crop(subobj, .materialize_crop_region(step$region))
    }
    subobj
}


# Tabular subobject methods (dataTableCoordinator) ####
# Tabular subobjects (cellMetaObj, exprObj, dimObj, spatEnrObj) narrow by
# the surviving cell_ID set and are otherwise untouched by space transforms
# (which are no-ops on non-spatial data).
#
# Note: for dataTableCoordinator, `prepareIds()` is the identity transform,
# so these methods consume `keep` directly via `%in%`. Backed coordinators
# (duckDB / sedona) register their own resolveSubobject methods that route
# through `prepareIds()` to promote the ID set into a JOIN-able table
# reference before applying it.

#' @rdname resolveSubobject
#' @export
setMethod("resolveSubobject",
    signature(subobj = "cellMetaObj", coordinator = "dataTableCoordinator"),
    function(subobj, gobject, view, space, coordinator, ...) {
        cache <- list(...)$.cache
        keep <- .cached_surviving_cell_ids(gobject, view, coordinator, cache)
        if (is.null(keep)) return(subobj)
        .narrow_subobject(subobj, cells = keep)
    }
)

#' @rdname resolveSubobject
#' @export
setMethod("resolveSubobject",
    signature(subobj = "exprObj", coordinator = "dataTableCoordinator"),
    function(subobj, gobject, view, space, coordinator, ...) {
        cache <- list(...)$.cache
        keep <- .cached_surviving_cell_ids(gobject, view, coordinator, cache)
        if (is.null(keep)) return(subobj)
        .narrow_subobject(subobj, cells = keep)
    }
)

#' @rdname resolveSubobject
#' @export
setMethod("resolveSubobject",
    signature(subobj = "dimObj", coordinator = "dataTableCoordinator"),
    function(subobj, gobject, view, space, coordinator, ...) {
        cache <- list(...)$.cache
        keep <- .cached_surviving_cell_ids(gobject, view, coordinator, cache)
        if (is.null(keep)) return(subobj)
        .narrow_subobject(subobj, cells = keep)
    }
)

#' @rdname resolveSubobject
#' @export
setMethod("resolveSubobject",
    signature(subobj = "spatEnrObj", coordinator = "dataTableCoordinator"),
    function(subobj, gobject, view, space, coordinator, ...) {
        cache <- list(...)$.cache
        keep <- .cached_surviving_cell_ids(gobject, view, coordinator, cache)
        if (is.null(keep)) return(subobj)
        .narrow_subobject(subobj, cells = keep)
    }
)

#' @rdname resolveSubobject
#' @export
setMethod("resolveSubobject",
    signature(subobj = "featMetaObj", coordinator = "dataTableCoordinator"),
    function(subobj, gobject, view, space, coordinator, ...) {
        # Feature metadata is feat-keyed, not cell-keyed. View filters that
        # carry `feat_ids` in their scope_args could narrow it; otherwise
        # featMetaObj passes through untouched.
        subobj
    }
)


# Spatial subobject methods (dataTableCoordinator) ####
# Spatial subobjects narrow by surviving cell_IDs (where cell-keyed) AND
# apply the space's transforms via existing eager GiottoClass dispatch.
# Crop is interpreted via the surviving cell_ID set (centroid-in-region
# semantics) for cell-keyed spatial subobjects; for non-cell-keyed
# (points, images), crop is applied geometrically at the subobject level.

#' @rdname resolveSubobject
#' @export
setMethod("resolveSubobject",
    signature(subobj = "spatLocsObj", coordinator = "dataTableCoordinator"),
    function(subobj, gobject, view, space, coordinator, ...) {
        cache <- list(...)$.cache
        keep <- .cached_surviving_cell_ids(gobject, view, coordinator, cache)
        if (!is.null(keep)) {
            subobj <- .narrow_subobject(subobj, cells = keep)
        }
        .apply_space_to_subobj(subobj, gobject, space, coordinator)
    }
)

#' @rdname resolveSubobject
#' @export
setMethod("resolveSubobject",
    signature(subobj = "giottoPolygon", coordinator = "dataTableCoordinator"),
    function(subobj, gobject, view, space, coordinator, ...) {
        cache <- list(...)$.cache
        keep <- .cached_surviving_cell_ids(gobject, view, coordinator, cache)
        if (!is.null(keep)) {
            # Polygon's poly_ID is conventionally aligned with cell_ID for
            # the cells spat_unit. (Unlinked-poly cascade is out of scope.)
            subobj <- .narrow_subobject(subobj, cells = keep)
            # Cached ID list, if present
            if (length(subobj@unique_ID_cache) > 0L) {
                subobj@unique_ID_cache <- intersect(
                    subobj@unique_ID_cache, keep)
            }
        }
        .apply_space_to_subobj(subobj, gobject, space, coordinator)
    }
)

#' @rdname resolveSubobject
#' @export
setMethod("resolveSubobject",
    signature(subobj = "giottoPoints", coordinator = "dataTableCoordinator"),
    function(subobj, gobject, view, space, coordinator, ...) {
        # Points are not cell-keyed; cell narrowing doesn't apply directly.
        # A crop applies geometrically at the subobject level, in the
        # post-transform frame: transform first, then crop in that frame.
        subobj <- .apply_space_to_subobj(subobj, gobject, space, coordinator)
        .apply_crops_geometrically(subobj, view)
    }
)

#' @rdname resolveSubobject
#' @export
setMethod("resolveSubobject",
    signature(subobj = "giottoLargeImage",
        coordinator = "dataTableCoordinator"),
    function(subobj, gobject, view, space, coordinator, ...) {
        subobj <- .apply_space_to_subobj(subobj, gobject, space, coordinator)
        .apply_crops_geometrically(subobj, view)
    }
)

#' @rdname resolveSubobject
#' @export
setMethod("resolveSubobject",
    signature(subobj = "giottoAffineImage",
        coordinator = "dataTableCoordinator"),
    function(subobj, gobject, view, space, coordinator, ...) {
        subobj <- .apply_space_to_subobj(subobj, gobject, space, coordinator)
        .apply_crops_geometrically(subobj, view)
    }
)

#' @rdname resolveSubobject
#' @export
setMethod("resolveSubobject",
    signature(subobj = "giottoImage", coordinator = "dataTableCoordinator"),
    function(subobj, gobject, view, space, coordinator, ...) {
        subobj <- .apply_space_to_subobj(subobj, gobject, space, coordinator)
        .apply_crops_geometrically(subobj, view)
    }
)
