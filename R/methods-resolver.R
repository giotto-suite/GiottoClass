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
# A leaf is handed its context rather than the gobject, so it is callable on
# its own and nothing is recomputed per leaf. See `R/classes-resolver.R` for
# what a coordinator is.
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
#' (points, images) never forces the ID computation.
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
#'   frame. This is the OUTPUT frame, independent of the frame a crop step
#'   names, which says only which frame that step's region was drawn in.
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
# Kept for one release so coordinators registered against it elsewhere keep
# working: `.resolve_leaf()` routes to it for any pair still registered under
# this name, and its default forwards to `resolveRecipe()`. The default is
# attached with `useAsDefault`, not as an ANY,ANY method, so a concrete
# (subobj, coordinator) method is chosen ahead of it.
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
# One set per resolve op, carried in one form per coordinator. A
# coordinator's `resolveKeep()` fills in the forms of every coordinator it
# extends as well as its own, so whichever leaf S4 falls through to finds the
# form it reads.

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

# Free-var names in `predicate` that name data columns, to pull via
# spatValues. Env values were inlined at record time, so what remains is
# columns plus the odd function passed as a value, dropped here. A column
# named like a function (`c`, `mean`) is treated as the function.
#' @keywords internal
#' @noRd
.predicate_column_refs <- function(predicate) {
    Filter(
        function(v) !exists(v, envir = globalenv(), mode = "function",
            inherits = TRUE),
        all.vars(predicate)
    )
}

# Evaluate one filter step via spatValues and return the surviving cell_IDs.
# The op's `spat_unit` / `feat_type` fill in what the step did not record. A
# step recorded on a different spat_unit is an error rather than an empty
# intersection: mapping one unit's answer onto another is not implemented.
# The step carries no environment, so it evaluates in `globalenv()`.
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

# Normalise the OUTPUT `space` argument (NULL, a giottoSpace, or a slotted
# name) to a giottoSpace or NULL. A crop step's own frame is separate, and is
# read off the step.
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

# Apply a giottoSpace's transform steps to a subobject through the eager
# transform generics. `sample` is the subobject's sample identity (NA for a
# plain giotto); `[[` resolves it against the recipe.
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
# through its steps. Layout is what `affine()` reads: linear part in
# `m[1:2, 1:2]`, post-multiplied, translation in COLUMN 3 -- a translation in
# row 3 is silently ignored. The probe is a spatLocsObj because `spatShift()`
# has no SpatVector method. NULL when the space has no steps.
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

# Crop routing ####
#
# Which geometry stands for a cell is declared on the step as `geom`, never
# inferred from the relation or from storage: "centroid" tests the cell's
# spatial_locs row (cheap, approximate at the boundary), "poly" tests its
# polygon (exact, needs a polygon source). The resulting cell_ID set narrows
# every target the same way.

# The polygon source for `geom = "poly"`, in the predicate frame. Returned as
# a giottoPolygon so `spatRelate()` dispatches on it (terra here, a store's
# own engines when backed). On a multi, poly_IDs are prefixed to `sample::id`
# to match the joint cell vocabulary.
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

# Route one crop step by its declared `geom` and return its surviving
# cell_IDs. `geom = "poly"` with no polygon source is an error rather than a
# silent fall back to centroids.
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

# Carriers for the crop arms, built lazily and memoised per frame, since two
# crop steps may name different spaces. A NULL build ("no spatial locations")
# is cached too, so its warning fires once per frame.
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

# The spatLocs of `spat_unit` (else the active one) as a points SpatVector in
# the predicate frame, for the centroid arm. `cell_ID` rides along as an
# attribute, so survivors are read off by ID.
#' @keywords internal
#' @noRd
.get_projected_spatlocs <- function(gobject, space, spat_unit = NULL) {
    sl <- .gm_fused_spatlocs(gobject, space, spat_unit = spat_unit)
    if (is.null(sl)) return(NULL)
    as.points(sl)
}

# Every child's locations in one `spatLocsObj`: the space applied per child
# (each sample has its own chain), IDs promoted to `sample::id` (children
# share local IDs, so `rbind2()` rejects the fold otherwise), then one fold.
# The space is applied here rather than passed to the getter because a
# multi's frames live on the parent, which a child cannot look up.
# `samples =` narrows to a frame's members; multi-only (adr/0006).
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

# Apply `view` / `space` to one subobject at the tail of a getter.
#' @keywords internal
#' @noRd
.apply_view_space <- function(subobj, gobject, view = NULL, space = NULL,
                              coordinator = NULL) {
    if (is.null(view) && is.null(space)) return(subobj)
    # a view is passed by name, never inline (design_view_space.Rmd)
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


# Per-call resolver cache holding the op's one `keep` as a promise: computed
# at most once, and not at all if no leaf reads it. An environment because
# the deprecated `resolveSubobject` path forwards it as `.cache`.
#' @keywords internal
#' @noRd
.new_resolver_cache <- function(gobject, view, coordinator,
                                spat_unit = NULL, feat_type = NULL) {
    cache <- new.env(parent = emptyenv())
    delayedAssign("keep", resolveKeep(coordinator, gobject, view,
        spat_unit = spat_unit, feat_type = feat_type), assign.env = cache)
    cache
}

# Is this (subobj, coordinator) pair still registered under the deprecated
# name? Not `hasMethod("resolveRecipe", ...)`: a backed coordinator inherits
# the in-memory leaves, so that is always TRUE and would silently send backed
# data down the in-memory path. A selected method whose `@defined` is not
# all-`ANY` is a real registration.
#' @keywords internal
#' @noRd
.has_legacy_leaf_method <- function(subobj, coordinator) {
    m <- methods::selectMethod("resolveSubobject",
        c(class(subobj)[1L], "ANY", "ANY", "ANY", class(coordinator)[1L]),
        optional = TRUE)
    if (!methods::is(m, "MethodDefinition")) return(FALSE)
    !all(m@defined == "ANY")
}

# The one leaf call site: `resolveRecipe()`, or the deprecated
# `resolveSubobject()` for a pair still registered under that name. `keep`
# is passed as a promise, so leaves that never read it never compute it.
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

# Is this leaf inside the op's scope? Compared only on the tags it carries:
# images carry none, points only a feat_type, spatial locations only a
# spat_unit. Out-of-scope leaves are left untouched (see `?resolveRecipe`).
#' @keywords internal
#' @noRd
.leaf_in_scope <- function(x, spat_unit, feat_type) {
    su <- spatUnit(x)
    ft <- featType(x)
    (is.null(spat_unit) || is.na(su) || identical(su, spat_unit)) &&
        (is.null(feat_type) || is.na(ft) || identical(ft, feat_type))
}

# The cell_IDs that survive a view's filter, crop and sample steps, in ONE
# spat_unit's ID vocabulary; NULL means no narrowing. Spat_units need not
# share cell_IDs, so every source is read in that unit. `feat_type` only
# picks which metadata a filter's columns come from.
#' @keywords internal
#' @noRd
.surviving_cell_ids <- function(gobject, view, spat_unit = NULL,
                                feat_type = NULL) {
    if (is.null(view)) return(NULL)

    filter_steps <- .view_steps_of(view, "filter")
    crop_steps   <- .view_steps_of(view, "crop")
    # on a multi a sample step bounds the cell set too
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
        # lazy, so an all-poly recipe never asks for spatial locations
        carriers <- .crop_carriers(gobject, spat_unit = spat_unit)
        for (step in crop_steps) {
            keep <- .cells_in_crop_step(gobject, step, carriers)
            # NULL: the step could not be evaluated, a skip not an empty set
            if (is.null(keep)) next
            surviving <- intersect(surviving, keep)
        }
    }
    surviving
}

# Clip a non-cell-keyed subobject (points, images), already in the output
# frame `space`, by every crop step. Each region is projected from the frame
# its step names, looked up in `spaces` (the object's `@spaces`).
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
# Narrow by the `vector` form of `keep`. `keep` is tested with `is.null()`,
# never `missing()`, which forces the promise inside an S4 `.local` wrapper.

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
        # feat-keyed, so a cell set does not apply; `keep` is never forced
        x
    }
)


# Spatial leaves (dataTableCoordinator) ####
# Cell-keyed ones narrow by `keep`; points and images are clipped
# geometrically. Both then apply the output space.

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
        # transform first, then clip in that frame
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
