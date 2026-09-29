#' @include classes-resolver.R
#' @include classes-view.R
#' @include classes-space.R
#' @include classes.R
NULL

# =============================================================================
# methods-resolver.R — the resolveRecipe() generic + dataTableCoordinator leaves
#
# `resolveRecipe()` spans both levels of one walk:
#
#   container   resolveRecipe(gobject, view = "roi", space = "layout")
#               evaluates the recipe ONCE — which cells survive, which frame
#               to return — then walks its slots. Methods live in
#               `methods-view.R`, beside the walk helpers.
#   leaf        resolveRecipe(subobj, coordinator, keep = k, space = s, view = v)
#               applies the context it was handed. Methods live below.
#
# The split used to be two generics (`materialize()` at the container,
# `resolveSubobject()` at the leaf) doing the same thing under two names, with
# every leaf reaching back up through the gobject to re-derive a value the
# container already had. Handing the leaf its context instead is what makes
# a leaf callable on its own — `resolveRecipe(myExprObj, co, keep = k)` is a unit
# test — and makes recomputation structurally impossible rather than merely
# memoised.
#
# The `coordinator` is a `viewCoordinator`-inheriting object that brokers
# IDs and joins between storage backings during resolution — see
# `R/classes-resolver.R` for the protocol notes.
# =============================================================================


# Generic ####

#' @title Resolve a view / space recipe
#' @name resolveRecipe
#' @description Apply a [giottoView] (and optional [giottoSpace]) recipe and
#' return the projected object.
#'
#' At a **container** (`giotto`, `giottoMulti`) this evaluates the recipe for
#' one `spat_unit` / `feat_type` scope and returns a gobject whose in-scope
#' subobjects have been projected through it. The input gobject is not
#' mutated.
#'
#' This is plumbing for code that consumes one scope of data, such as a plot
#' function, not a way to derive a new object to work on. Only the requested
#' scope is guaranteed: subobjects outside it are left untouched, so they
#' need not agree with the narrowed ones. The returned gobject is a carrier
#' for the narrowed data — read what you asked for from it and discard it.
#' To narrow a whole object, use [subsetGiotto()] or `subset()`.
#'
#' At a **leaf** (one subobject) this applies an already-evaluated context:
#' the surviving cell set, the output frame, the recipe itself.
#'
#' @details
#' Dispatch is on `(x, coordinator)`; everything past `x` is passed by name.
#' A container does not dispatch on the coordinator — it *picks* one from
#' `x@source` when none is given — so container methods register against
#' `ANY` and consume `coordinator` as an ordinary argument.
#'
#' `keep` reaches a leaf as a promise, so a leaf that does not narrow by cell
#' (points, images) never forces the ID computation. That preserves the
#' laziness the old per-leaf `.cache` bought, without the leaf having to know
#' a cache exists.
#'
#' `coordinator` defaults to `NULL` on the *generic*, not just on the
#' container methods. S4 propagates the generic's formals symbolically into
#' the method's `.local` call, so a default written only on the method is
#' never reached when the argument is omitted.
#'
#' @param x a `giotto` / `giottoMulti` container, or a giotto subobject leaf
#' @param coordinator a [viewCoordinator-class]-inheriting object brokering
#'   IDs and joins between storage backings. At a container, `NULL` (the
#'   default) selects one from `x@source` — in-memory for non-disk gobjects.
#' @param view at a container, a `character(1)` naming a slotted view, or
#'   `NULL`. At a leaf, the already-looked-up [giottoView] or `NULL` — a
#'   recipe is inert data, so a leaf needs no gobject to read one.
#' @param space at a container, a `character(1)` naming a slotted
#'   [giottoSpace]; at a leaf, the resolved object. `NULL` means the native
#'   frame. This is the OUTPUT frame, and it is deliberately independent of
#'   the frame a crop step names, which says only which frame that step's
#'   region coordinates were read in. Defaulting one to the other is the
#'   conflation that made a `space`-bound view silently return transformed
#'   coordinates from a plain getter.
#' @param spaces leaf only — the object's `@spaces` list, where a crop
#'   step's recorded frame is looked up by name. Handed over as data so that
#'   a leaf clipping geometry needs no gobject.
#' @param keep leaf only — the op's surviving cell set as a `viewKeep` (see
#'   [resolveKeep()]),
#'   or `NULL` for "no narrowing".
#' @param spat_unit container only — the spat_unit to resolve. The surviving
#'   cell set is computed in this unit's cell_ID vocabulary, and only
#'   subobjects of this unit are narrowed. `NULL` uses the active one.
#' @param feat_type container only — the feat_type to resolve. Subobjects of
#'   other feat_types are left untouched, and filter columns are read from
#'   this feat_type's metadata. `NULL` uses the active one.
#' @param slots container only — optional `character` vector of slot names to
#'   narrow. When `NULL` (default), all slot lists are walked
#'   (`cell_metadata`, `expression`, `dimension_reduction`,
#'   `spatial_enrichment`, `feat_metadata`, `spatial_locs`, `spatial_info`,
#'   `feat_info`, `images`). When supplied, only the listed slots are walked —
#'   the rest are left untouched on the returned object. Useful for internal
#'   helpers that consume a subset of slots and want to share one resolver
#'   pass without paying for irrelevant ones.
#' @param ... reserved for backend-specific args
#' @returns the projected object, same class as `x`
#' @export
setGeneric("resolveRecipe",
    function(x, coordinator = NULL, ...) standardGeneric("resolveRecipe"),
    signature = c("x", "coordinator"))


# Deprecated generic: resolveSubobject ####
#
# The leaf half of `resolveRecipe()` under its old name and its old five-argument
# signature. Kept for one release because downstream coordinators register
# against it and cannot be switched in the same release: {GiottoDisk} has 8
# registrations, no call sites, and tracks *released* GiottoClass, so dropping
# the generic here would unwire backed view resolution the moment this lands.
#
# It bridges both directions. A caller still using the old name reaches the new
# leaf methods through the generic's default below, while a downstream method
# registered on a concrete (subobj, coordinator) pair is more specific and
# still wins — which is what keeps {GiottoDisk} working unchanged.
# `.resolve_leaf()` closes the loop from the other side, falling back to this
# generic for any coordinator that has not yet registered `resolveRecipe` methods.

# Attached with `useAsDefault` rather than registered as an ANY,ANY
# `setMethod` so that a downstream method on a concrete (subobj, coordinator)
# pair is chosen ahead of it.
#' @keywords internal
#' @noRd
.resolveSubobject_default <- function(subobj, gobject, view, space,
                                      coordinator, ...) {
    deprecate_soft("0.7.3", "resolveSubobject()", "resolveRecipe()")
    # Reached from a container walk, `.cache` holds the op's `keep`. Called
    # directly, there is no op, so the subobject's own tags are the scope.
    cache <- list(...)$.cache %||% .new_resolver_cache(gobject, view,
        coordinator, spat_unit = .na_to_null(spatUnit(subobj)),
        feat_type = .na_to_null(featType(subobj)))
    # `keep` stays a promise across this call, so the non-cell-keyed leaves
    # still never trigger the ID computation.
    resolveRecipe(subobj, coordinator, keep = cache$keep, space = space,
        view = view, ...)
}

#' @title resolveSubobject
#' @name resolveSubobject
#' @description Deprecated in 0.7.3. Superseded by [resolveRecipe()], which is one
#' generic for both the container and the leaf, and which hands a leaf its
#' evaluated context instead of the parent gobject.
#'
#' @param subobj a giotto subobject (e.g. `cellMetaObj`, `spatLocsObj`, ...)
#' @param gobject the parent `giotto`
#' @param view a [giottoView] or `NULL`
#' @param space a [giottoSpace] or `NULL`
#' @param coordinator a [viewCoordinator-class]-inheriting object
#' @param ... reserved for backend-specific args
#' @returns the projected subobject (same class as `subobj`)
#' @keywords internal
#' @export
setGeneric("resolveSubobject",
    function(subobj, gobject, view, space, coordinator, ...)
        standardGeneric("resolveSubobject"),
    useAsDefault = .resolveSubobject_default)



# The surviving cell set: viewKeep + resolveKeep ####
#
# A resolve op computes ONE surviving cell set, in one spat_unit's ID
# vocabulary, and hands it to every leaf. Different coordinators want that
# set in different forms -- a character vector to `%in%` against in memory,
# an arrow Table to semi-join inside a backed store's lazy query -- so the
# set travels as a `viewKeep`: a named list holding each form under the
# coordinator's name for it.
#
# Coordinators extend one another (`parquetCoordinator` contains
# `dataTableCoordinator`, so an in-memory subobject inside a backed gobject
# falls through to the in-memory leaf). The rule that makes that fall-through
# safe is on `resolveKeep()`: a coordinator's method fills in its own form
# AND the forms of every coordinator it extends. Whichever leaf S4 lands on
# then finds the form it reads, and the set is evaluated once rather than
# once per coordinator.

#' @rdname resolveKeep
#' @param x a `viewKeep`
#' @export
print.viewKeep <- function(x, ...) {
    # names only: printing must not force a form that is still a promise
    cat(sprintf("<viewKeep> forms: %s\n",
        paste(sort(names(x)), collapse = ", ")))
    invisible(x)
}

#' @title resolveKeep
#' @name resolveKeep
#' @description Evaluate a view into the surviving cell set of one
#' [resolveRecipe()] op: the cells of `spat_unit` that pass every filter, crop and
#' sample step. Dispatches on the coordinator, which decides how the set is
#' computed and which forms it carries.
#'
#' The set is returned as a `viewKeep`: an environment holding one form of
#' the set per coordinator that reads it, built with
#' `structure(list2env(list(vector = ids), parent = emptyenv()),
#' class = "viewKeep")`. `dataTableCoordinator` reads `vector`, a character
#' vector of cell_IDs. A coordinator that extends another adds its own form
#' alongside, under its own name. It is an environment so that a form can be
#' installed with [delayedAssign()] and computed only if a leaf reads it —
#' a backed coordinator whose own form is a lazy query collects `vector` only
#' when an in-memory subobject asks for it. A view that narrows nothing returns `NULL` rather than an empty
#' `viewKeep`: `NULL` means "every cell survives", while a `viewKeep` holding
#' zero IDs means none do.
#'
#' A method must return every form that the coordinators its class extends
#' read, as well as its own. A coordinator that computes its own form can
#' derive the inherited ones from it; one that has no faster path can call
#' the inherited method with `callNextMethod()` and add its form to the
#' result.
#'
#' @param coordinator a [viewCoordinator-class]-inheriting object
#' @param gobject the `giotto` / `giottoMulti` the view is evaluated against
#' @param view a [giottoView], or `NULL`
#' @param spat_unit the spat_unit whose ID vocabulary the set is in. `NULL`
#'   uses the active one.
#' @param feat_type the feat_type whose metadata a filter's columns are read
#'   from. `NULL` uses the active one.
#' @param ... reserved for backend-specific args
#' @returns a `viewKeep`, or `NULL` when the view narrows nothing
#' @keywords internal
#' @export
setGeneric("resolveKeep",
    function(coordinator, gobject, view, ...) standardGeneric("resolveKeep"),
    signature = "coordinator")

#' @rdname resolveKeep
#' @export
setMethod("resolveKeep", signature(coordinator = "dataTableCoordinator"),
    function(coordinator, gobject, view, spat_unit = NULL, feat_type = NULL,
             ...) {
        ids <- .surviving_cell_ids(gobject, view, spat_unit = spat_unit,
            feat_type = feat_type)
        if (is.null(ids)) return(NULL)
        structure(list2env(list(vector = ids), parent = emptyenv()),
            class = "viewKeep")
    }
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
# multi-scope"); for giottoMulti, returns the selected children. Names go
# through the same resolver as `samples =` on a getter, so a group name
# expands at use time and an unknown name is an error rather than dropped.
#' @keywords internal
#' @noRd
.resolve_sample_select <- function(gobject, view) {
    ss_steps <- .view_steps_of(view, "samples")
    if (length(ss_steps) == 0L) return(NA)
    if (!inherits(gobject, "giottoMulti")) {
        warning(call. = FALSE,
            "view sample step ignored: parent is not a giottoMulti")
        return(NA)
    }
    # several sample steps intersect, like every other step kind
    sel <- NULL
    for (s in ss_steps) {
        r <- .gm_resolve_samples(gobject, s$samples, site = "view samples")
        sel <- if (is.null(sel)) r else intersect(sel, r)
    }
    sel
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
# `spat_unit` / `feat_type` are the resolve op's scope, and fill in whatever
# the step did not record. A step that recorded a DIFFERENT spat_unit asks a
# question in another ID vocabulary; mapping its answer across (cell <->
# nucleus via overlaps, say) is not implemented, so it is an error rather
# than an empty intersection.
#
# The predicate is stored deparsed, so it is parsed here. `globalenv()` is
# the evaluation enclosure: the step carries no environment by design (Q7),
# and functions resolve from there through the attached-package chain.
#' @keywords internal
#' @noRd
.eval_view_filter <- function(step, gobject, spat_unit = NULL,
                              feat_type = NULL) {
    pred <- str2lang(step$predicate)
    cols <- .predicate_column_refs(pred)
    if (length(cols) == 0L) {
        # purely constant predicate; pull all cell_IDs and let eval decide
        cell_ids <- spatIDs(gobject, spat_unit = spat_unit)
        keep <- eval(pred, envir = list(), enclos = globalenv())
        return(if (isTRUE(keep)) cell_ids else character())
    }
    scope <- step$scope_args %||% list()
    step_su <- scope$spat_unit
    if (!is.null(step_su) && !is.null(spat_unit) &&
        !identical(step_su, spat_unit)) {
        stop(sprintf(paste0("[resolve] filter step reads spat_unit '%s' ",
            "but this resolve is scoped to '%s'. Filtering one spat_unit ",
            "by another's values is not supported yet."), step_su,
            spat_unit), call. = FALSE)
    }
    scope$spat_unit <- step_su %||% spat_unit
    scope$feat_type <- scope$feat_type %||% feat_type
    sv_args <- c(list(gobject = gobject, feats = cols), scope)
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
#
# It used to carry `gobject` and `coordinator` on its signature and reference
# neither in its body -- the clearest single sign that the leaf contract was
# passing context that nothing consumed.
#' @keywords internal
#' @noRd
.apply_space_to_subobj <- function(subobj, space, sample = NA_character_) {
    if (is.null(space)) return(subobj)
    for (step in space[[sample]]) {
        subobj <- do.call(step$op, c(list(x = subobj), step$args))
    }
    subobj
}

#' @title Project a region between coordinate frames
#' @name project_region
#' @description Move a region drawn in one [giottoSpace]'s frame into
#' another's, through the native frame both are defined against. A crop step
#' records the frame its region was drawn in; this is what lets it clip
#' content returned in a different one.
#' @param y the region: a `SpatVector`, WKT `character`, `SpatExtent`, or a
#'   numeric `c(xmin, xmax, ymin, ymax)`
#' @param from_space,to_space [giottoSpace] objects scoped to one sample (or
#'   none), or `NULL` for the native frame
#' @returns `y` in `to_space`'s frame. When both frames are native it is
#'   returned as given, so a recorded WKT string stays WKT.
#' @keywords internal
#' @export
project_region <- function(y, from_space = NULL, to_space = NULL) {
    if (is.null(from_space) && is.null(to_space)) return(y)
    if (!inherits(y, "SpatVector")) {
        if (is.character(y)) y <- terra::vect(y)
        else if (is.numeric(y)) y <- terra::as.polygons(terra::ext(y))
        else if (inherits(y, "SpatExtent")) y <- terra::as.polygons(y)
    }
    m_from <- .space_composite_affine(from_space)
    m_to <- .space_composite_affine(to_space)
    # the same frame on both sides: skip the round trip and its float drift
    if (!is.null(m_from) && !is.null(m_to) &&
        isTRUE(all.equal(m_from, m_to))) {
        return(y)
    }
    if (!is.null(m_from)) y <- affine(y, m_from, inv = TRUE)
    if (!is.null(m_to)) y <- affine(y, m_to)
    y
}

# The 3x3 affine a space applies, measured by pushing three basis points
# through its steps, in the layout `affine()` reads: the linear part in
# `m[1:2, 1:2]`, post-multiplied (`[x, y] %*% m[1:2, 1:2]`), and the
# translation in COLUMN 3, `m[1:2, 3]`. A translation in row 3 -- the other
# common convention -- is silently ignored by `affine()`, which applies the
# linear part only. NULL when the space has no steps for this sample.
#
# The probe is a spatLocsObj rather than a bare SpatVector because every
# transform generic has a spatLocsObj method and `spatShift()` has none for a
# SpatVector; it also measures the matrix through the same methods real
# coordinates go through, rather than a parallel implementation.
#' @keywords internal
#' @noRd
.space_composite_affine <- function(space) {
    if (is.null(space)) return(NULL)
    steps <- space[[NA_character_]]
    if (length(steps) == 0L) return(NULL)
    probe <- createSpatLocsObj(
        data.table::data.table(cell_ID = c("o", "x", "y"),
            sdimx = c(0, 1, 0), sdimy = c(0, 0, 1)),
        name = "probe", verbose = FALSE)
    probe <- .apply_space_to_subobj(probe, space)
    p <- as.matrix(probe@coordinates[, c("sdimx", "sdimy")])
    m <- diag(3L)
    m[1L, 1:2] <- p[2L, ] - p[1L, ]
    m[2L, 1:2] <- p[3L, ] - p[1L, ]
    m[1:2, 3L] <- p[1L, ]
    m
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
.get_projected_polys <- function(gobject, space, spat_unit = NULL) {
    one <- function(g, samp) {
        gp <- tryCatch(
            getPolygonInfo(g, name = spat_unit,
                return_giottoPolygon = TRUE, verbose = FALSE),
            error = function(e) NULL)
        if (!inherits(gp, "giottoPolygon")) return(NULL)
        .apply_space_to_subobj(gp, space, sample = samp)
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
.cells_in_crop_step <- function(gobject, step, carriers,
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
.crop_carriers <- function(gobject, spat_unit = NULL) {
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
                .get_projected_spatlocs(gobject, sp, spat_unit = spat_unit)
            })
            if (is.null(out)) {
                warning("crop step skipped: no spatial locations available",
                    call. = FALSE)
            }
            out
        },
        polys = function(space_name) {
            memoised("poly", space_name, function(sp) {
                .get_projected_polys(gobject, sp, spat_unit = spat_unit)
            })
        }
    )
}

# Pull the gobject's spatLocs (`spat_unit`, else the active one) as a points
# `SpatVector` in the predicate frame, optionally applying the relevant
# space's transforms first. Consumed by `.cells_in_crop_step()`'s centroid arm.
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
.get_projected_spatlocs <- function(gobject, space, spat_unit = NULL) {
    sl <- .gm_fused_spatlocs(gobject, space, spat_unit = spat_unit)
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
.gm_fused_spatlocs <- function(gobject, space,
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
            child_sl <- .apply_space_to_subobj(child_sl, space, sample = nm)
            dt <- data.table::copy(child_sl[])
            dt[, cell_ID := .gm_global_cell_ids(gobject, nm, cell_ID)]
            child_sl[] <- dt
            child_sl
        })
        parts <- Filter(Negate(is.null), parts)
        if (length(parts) == 0L) return(NULL)
        sl <- Reduce(rbind2, parts)
    } else if (!is.null(space)) {
        sl <- .apply_space_to_subobj(sl, space)
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
    # vignettes/articles/design_view_space.Rmd for the reasoning.
    if (!is.null(view)) {
        checkmate::assert_string(view, .var.name = "view")
    }
    v <- if (is.null(view)) NULL else giottoView(gobject, view)
    # `space` here is the OUTPUT frame -- the predicate frame (the view's `space`)
    # is consulted independently by the crop step handlers below.
    s <- .resolve_space(gobject, space)
    co <- if (is.null(coordinator)) .default_view_coordinator(gobject)
        else coordinator
    # The getter has already picked its subobject, so the subobject's own
    # tags are the op's scope. `keep` is still a promise, which is what keeps
    # a points getter from computing a cell_ID set it will not use.
    cache <- .new_resolver_cache(gobject, v, co,
        spat_unit = .na_to_null(spatUnit(subobj)),
        feat_type = .na_to_null(featType(subobj)))
    .resolve_leaf(subobj, gobject, v, s, co, cache)
}

# `spatUnit()` / `featType()` answer NA for an axis a subobject does not
# carry; a resolve scope says the same thing with NULL.
#' @keywords internal
#' @noRd
.na_to_null <- function(x) if (length(x) == 0L || is.na(x[[1L]])) NULL else x


# Per-call resolver cache. Holds the op's one `keep` as a promise, installed
# by `.new_resolver_cache()`, so it is computed at most once per call and not
# at all when no leaf reads it.
#
# It is an environment rather than a closure variable because the deprecated
# `resolveSubobject` path forwards it as `.cache`: downstream coordinators
# that have not yet switched to `resolveRecipe()` memoise their own keys in it, and
# the deprecated default reads `keep` back out of it.
#
# Scope: per-container-call. Persistent caching across calls needs a
# version-stamp invalidation scheme (deferred).
#' @keywords internal
#' @noRd
.new_resolver_cache <- function(gobject, view, coordinator,
                                spat_unit = NULL, feat_type = NULL) {
    cache <- new.env(parent = emptyenv())
    delayedAssign("keep", resolveKeep(coordinator, gobject, view,
        spat_unit = spat_unit, feat_type = feat_type), assign.env = cache)
    cache
}

# Does a downstream package still register this (subobj, coordinator) pair
# under the deprecated name?
#
# It cannot be answered with `hasMethod("resolveRecipe", ...)`, and the reason is
# worth stating because getting it wrong is silent. Concrete coordinators
# INHERIT `dataTableCoordinator` -- {GiottoDisk}'s `parquetCoordinator` is
# declared `contains = "dataTableCoordinator"` so that in-memory subobjects
# inside an otherwise-backed gobject fall through to the in-memory path. That
# inheritance means every leaf class already "has" a `resolveRecipe` method for a
# backed coordinator, by inheritance from the in-memory one. Asking that
# question would answer "yes, use the new path" and quietly route backed data
# through the leaf that materialises it, losing every pushdown.
#
# So ask the question that actually decides it: has a real method been
# registered under the old name for this pair? A `@defined` signature that is
# not all-`ANY` means yes; all-`ANY` is the generic's own default. When a
# downstream package switches, it deletes that registration and this turns
# false on its own -- no version check, no flag.
#' @keywords internal
#' @noRd
.has_legacy_leaf_method <- function(subobj, coordinator) {
    m <- methods::selectMethod("resolveSubobject",
        c(class(subobj)[1L], "ANY", "ANY", "ANY", class(coordinator)[1L]),
        optional = TRUE)
    if (!methods::is(m, "MethodDefinition")) return(FALSE)
    !all(m@defined == "ANY")
}

# The one leaf call site. Prefers `resolveRecipe()`, and uses the deprecated
# `resolveSubobject()` when a downstream package still registers this pair
# under that name -- {GiottoDisk}'s parquetCoordinator registrations land
# here until its own switch ships. That arm passes the gobject and the
# `.cache` they were written against, unchanged.
#
# On the `resolveRecipe()` arm `keep` is read out of the cache as a promise. Leaves
# that never read it -- points, images, feature metadata -- never trigger the
# ID computation.
#' @keywords internal
#' @noRd
.resolve_leaf <- function(subobj, gobject, view, space, coordinator, cache,
                          spaces = gobject@spaces) {
    if (.has_legacy_leaf_method(subobj, coordinator)) {
        return(resolveSubobject(subobj, gobject, view, space, coordinator,
            .cache = cache))
    }
    resolveRecipe(subobj, coordinator, keep = cache$keep, space = space,
        view = view, spaces = spaces)
}

# Is this leaf inside the op's scope? A leaf is compared only on the schema
# tags it carries: images carry neither and are always in scope; points carry
# only a feat_type; spatial locations only a spat_unit.
#
# Out-of-scope leaves are left untouched rather than narrowed. `resolveRecipe()`
# answers for the scope it was asked about, so a unit nobody requested is not
# kept consistent with the one that was -- see the contract on `?resolveRecipe`.
#' @keywords internal
#' @noRd
.leaf_in_scope <- function(x, spat_unit, feat_type) {
    su <- spatUnit(x)
    ft <- featType(x)
    (is.null(spat_unit) || is.na(su) || identical(su, spat_unit)) &&
        (is.null(feat_type) || is.na(ft) || identical(ft, feat_type))
}

# Compute the cell_ID set that survives a view's filter + crop steps, in ONE
# spat_unit's ID vocabulary. Returns a character vector of cell_IDs; NULL
# means "no narrowing" (all cells survive).
#
# One vocabulary per call is the whole scoping rule. Two spat_units need not
# share cell_IDs (cells and nuclei, cells and bins), so a set computed in one
# cannot narrow the other -- `%in%` over disjoint sets answers "empty", not
# "error". A resolve op is therefore scoped to one spat_unit, and every
# source this reads -- the starting ID set, filter columns, crop carriers --
# is read in that unit. `feat_type` never splits the cell vocabulary; it only
# picks which feat_type's metadata a filter's columns are looked up in.
#
# The PREDICATE frame for crop steps is the view's `space` -- the frame the
# crop region was drawn in. This is independent of any output space the
# caller may have requested via the explicit `space=` arg, which is why
# this helper does not take a `space` argument.
#' @keywords internal
#' @noRd
.surviving_cell_ids <- function(gobject, view, spat_unit = NULL,
                                feat_type = NULL) {
    if (is.null(view)) return(NULL)

    filter_steps <- .view_steps_of(view, "filter")
    crop_steps   <- .view_steps_of(view, "crop")
    # On a multi, a sample step bounds the cell set too, so every path that
    # resolves a view -- per-child getters, joint slots, the gAny fall-through
    # -- agrees on it. A child being resolved on its own is a plain giotto
    # and has no samples to select, so the step is a no-op there.
    sel <- if (inherits(gobject, "giottoMulti")) {
        .resolve_sample_select(gobject, view)
    } else NA

    no_sel <- length(sel) == 1L && is.na(sel)
    if (length(filter_steps) == 0L && length(crop_steps) == 0L && no_sel) {
        return(NULL)
    }

    surviving <- if (no_sel) spatIDs(gobject, spat_unit = spat_unit) else
        spatIDs(gobject, spat_unit = spat_unit, object = sel)
    for (step in filter_steps) {
        keep <- .eval_view_filter(step, gobject, spat_unit = spat_unit,
            feat_type = feat_type)
        surviving <- intersect(surviving, keep)
    }
    if (length(crop_steps) > 0L) {
        # Carriers are built lazily and per frame, so an all-poly recipe on
        # a polygon-only object never asks for spatial locations and never
        # warns about their absence.
        carriers <- .crop_carriers(gobject, spat_unit = spat_unit)
        for (step in crop_steps) {
            keep <- .cells_in_crop_step(gobject, step, carriers)
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
# subobject (points, images) that is already in the OUTPUT frame `space`.
#
# A step's region was drawn in the frame the step names (`step$space`, NA for
# the native frame), which need not be the output frame, so the region is
# projected across before it clips. The step names its frame; `spaces` --
# the object's `@spaces` -- is where that name is looked up, handed over as
# data so the leaf needs no gobject.
#' @keywords internal
#' @noRd
.apply_crops_geometrically <- function(subobj, view, spaces = NULL,
                                       space = NULL) {
    for (step in .view_steps_of(view, "crop")) {
        region <- project_region(.materialize_crop_region(step$region),
            from_space = .step_frame(step, spaces), to_space = space)
        subobj <- crop(subobj, region)
    }
    subobj
}

# The frame a crop step's region was drawn in: its named space looked up in
# `spaces`, or NULL for the native frame.
#' @keywords internal
#' @noRd
.step_frame <- function(step, spaces) {
    nm <- step$space
    if (is.null(nm) || is.na(nm)) return(NULL)
    sp <- spaces[[nm]]
    if (is.null(sp)) {
        stop(sprintf(paste0("[resolve] a crop step was drawn in space ",
            "'%s', which is not registered on this object"), nm),
            call. = FALSE)
    }
    sp
}


# Tabular leaves (dataTableCoordinator) ####
# Tabular subobjects (cellMetaObj, exprObj, dimObj, spatEnrObj) narrow by the
# surviving cell_ID set and are otherwise untouched by space transforms, which
# are no-ops on non-spatial data.
#
# `keep` is a `viewKeep`; these leaves read its `vector` form, a character
# vector consumed via `%in%`. A backed coordinator's `resolveKeep()` method
# fills that form too, which is what lets its in-memory subobjects fall
# through to these methods unchanged.
#
# `keep` is read with `is.null()`, never `missing()`: an S4 method whose
# formals extend the generic's is wrapped in a `.local` call, and `missing()`
# on such a formal forces its promise at the wrong moment.

#' @rdname resolveRecipe
#' @export
setMethod("resolveRecipe",
    signature(x = "cellMetaObj", coordinator = "dataTableCoordinator"),
    function(x, coordinator, keep = NULL, space = NULL, view = NULL, ...) {
        if (is.null(keep)) return(x)
        .narrow_subobject(x, cells = keep$vector)
    }
)

#' @rdname resolveRecipe
#' @export
setMethod("resolveRecipe",
    signature(x = "exprObj", coordinator = "dataTableCoordinator"),
    function(x, coordinator, keep = NULL, space = NULL, view = NULL, ...) {
        if (is.null(keep)) return(x)
        .narrow_subobject(x, cells = keep$vector)
    }
)

#' @rdname resolveRecipe
#' @export
setMethod("resolveRecipe",
    signature(x = "dimObj", coordinator = "dataTableCoordinator"),
    function(x, coordinator, keep = NULL, space = NULL, view = NULL, ...) {
        if (is.null(keep)) return(x)
        .narrow_subobject(x, cells = keep$vector)
    }
)

#' @rdname resolveRecipe
#' @export
setMethod("resolveRecipe",
    signature(x = "spatEnrObj", coordinator = "dataTableCoordinator"),
    function(x, coordinator, keep = NULL, space = NULL, view = NULL, ...) {
        if (is.null(keep)) return(x)
        .narrow_subobject(x, cells = keep$vector)
    }
)

#' @rdname resolveRecipe
#' @export
setMethod("resolveRecipe",
    signature(x = "featMetaObj", coordinator = "dataTableCoordinator"),
    function(x, coordinator, keep = NULL, space = NULL, view = NULL, ...) {
        # Feature metadata is feat-keyed, not cell-keyed. View filters that
        # carry `feat_ids` in their scope_args could narrow it; otherwise
        # featMetaObj passes through untouched. `keep` is never forced.
        x
    }
)


# Spatial leaves (dataTableCoordinator) ####
# Spatial subobjects narrow by surviving cell_IDs (where cell-keyed) AND apply
# the space's transforms via existing eager GiottoClass dispatch. Crop is
# interpreted via the surviving cell_ID set (centroid-in-region semantics) for
# cell-keyed spatial subobjects; for non-cell-keyed (points, images), crop is
# applied geometrically at the subobject level.

#' @rdname resolveRecipe
#' @export
setMethod("resolveRecipe",
    signature(x = "spatLocsObj", coordinator = "dataTableCoordinator"),
    function(x, coordinator, keep = NULL, space = NULL, view = NULL, ...) {
        if (!is.null(keep)) x <- .narrow_subobject(x, cells = keep$vector)
        .apply_space_to_subobj(x, space)
    }
)

#' @rdname resolveRecipe
#' @export
setMethod("resolveRecipe",
    signature(x = "giottoPolygon", coordinator = "dataTableCoordinator"),
    function(x, coordinator, keep = NULL, space = NULL, view = NULL, ...) {
        if (!is.null(keep)) {
            # Polygon's poly_ID is conventionally aligned with cell_ID for
            # the cells spat_unit. (Unlinked-poly cascade is out of scope.)
            x <- .narrow_subobject(x, cells = keep$vector)
            # Cached ID list, if present
            if (length(x@unique_ID_cache) > 0L) {
                x@unique_ID_cache <- intersect(x@unique_ID_cache,
                    keep$vector)
            }
        }
        .apply_space_to_subobj(x, space)
    }
)

#' @rdname resolveRecipe
#' @export
setMethod("resolveRecipe",
    signature(x = "giottoPoints", coordinator = "dataTableCoordinator"),
    function(x, coordinator, keep = NULL, space = NULL, view = NULL,
             spaces = NULL, ...) {
        # Points are not cell-keyed; cell narrowing doesn't apply directly.
        # A crop applies geometrically at the subobject level, in the
        # post-transform frame: transform first, then crop in that frame.
        x <- .apply_space_to_subobj(x, space)
        .apply_crops_geometrically(x, view, spaces, space)
    }
)

#' @rdname resolveRecipe
#' @export
setMethod("resolveRecipe",
    signature(x = "giottoLargeImage", coordinator = "dataTableCoordinator"),
    function(x, coordinator, keep = NULL, space = NULL, view = NULL,
             spaces = NULL, ...) {
        x <- .apply_space_to_subobj(x, space)
        .apply_crops_geometrically(x, view, spaces, space)
    }
)

#' @rdname resolveRecipe
#' @export
setMethod("resolveRecipe",
    signature(x = "giottoAffineImage", coordinator = "dataTableCoordinator"),
    function(x, coordinator, keep = NULL, space = NULL, view = NULL,
             spaces = NULL, ...) {
        x <- .apply_space_to_subobj(x, space)
        .apply_crops_geometrically(x, view, spaces, space)
    }
)

#' @rdname resolveRecipe
#' @export
setMethod("resolveRecipe",
    signature(x = "giottoImage", coordinator = "dataTableCoordinator"),
    function(x, coordinator, keep = NULL, space = NULL, view = NULL,
             spaces = NULL, ...) {
        x <- .apply_space_to_subobj(x, space)
        .apply_crops_geometrically(x, view, spaces, space)
    }
)
