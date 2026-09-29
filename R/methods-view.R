#' @include classes-view.R
#' @include classes.R
#' @include generics.R
#' @include methods-resolver.R
NULL

# =============================================================================
# methods-view.R — the view recorder and accessors
#
# Views are subset/narrowing recipes only — no transforms. Transforms live on
# the space recipe (see methods-space.R). A crop step may name a slotted
# space; its region is then meaningful in that frame.
#
# There is no view-receiver surface: every op reaches a view through the
# gobject method that takes `view = `, which records the step onto the named
# slot (decision Q8). The recording verbs live with their generics —
# `subset()` in `methods-extract.R` (filter steps, and sample steps via
# `samples =`), `crop()` in `methods-crop.R`.
# =============================================================================


# Internal helper ####
.view_record_step <- function(view, step) {
    view@steps <- c(view@steps, list(step))
    view
}

# Append a filter step from an ALREADY-CAPTURED predicate.
#
# This is the one construction path for a filter step, and the reason it is
# a named helper rather than the `subset()` method itself: the gobject-side
# methods have a formal named `subset`, so a `subset(v, ...)` call in their
# body forces that promise -- the user's unevaluated predicate -- while R
# checks whether the binding is callable. Which is exactly the evaluation
# recording exists to avoid.
.view_record_filter <- function(view, predicate, negate = FALSE,
                                scope_args = list()) {
    # `negate` is folded into the predicate, exactly as the eager path does
    # it (`sub_s <- call("!", sub_s)`), so the step records the EFFECTIVE
    # predicate. Keeping it as a separate field would be a second way to
    # say the same thing, and `spatValues()` -- which the step's scope_args
    # are forwarded to -- has no concept of negation to hand it to.
    if (negate) predicate <- call("!", predicate)
    .view_record_step(view, .view_step_filter(predicate,
        scope_args = scope_args))
}


# Indirect-usage routing: lets generics like subset() / crop() on a
# `giotto` accept `view = <name>` and record the step rather than
# executing eagerly. Returns the gobject with the named view slotted /
# appended.
#
# `f` is the builder verb applied to the named view -- the SAME method a
# caller holding the view would use -- so a step has one construction path
# whichever surface asked for it.
.record_view_on_gobject <- function(gobject, view, f) {
    # view contract: character(1) name. Views are identified by name only —
    # passing a recipe inline was considered and rejected (see
    # vignettes/articles/design_view_space.Rmd), because it would make
    # the same call site sometimes return a gobject and sometimes a recipe.
    # A view that does not exist yet is created here, so recording is the
    # construction path.
    checkmate::assert_string(view, .var.name = "view")
    existing <- if (view %in% giottoViews(gobject)) {
        giottoView(gobject, view)
    } else {
        .new_view()
    }
    giottoView(gobject, view) <- f(existing)
    gobject
}

# Substitute env-resident scalar / vector values into `pred` so the
# predicate becomes self-contained. This is what lets the step store a
# deparsed string with no environment attached: after substitution the only
# free names left are data columns and functions, and functions resolve
# through the package chain at eval time.
#
# Functions and missing names are left alone.
.eager_substitute_env <- function(pred, env) {
    all_vars <- all.vars(pred)
    sub_list <- list()
    for (v in all_vars) {
        if (exists(v, envir = env, inherits = TRUE)) {
            val <- tryCatch(get(v, envir = env, inherits = TRUE),
                error = function(e) NULL)
            if (!is.null(val) && !is.function(val)) {
                sub_list[[v]] <- val
            }
        }
    }
    if (length(sub_list) == 0L) return(pred)
    do.call("substitute", list(pred, sub_list))
}

# Walk the call stack to find the user-level frame that holds the
# predicate's free variables. S4 dispatch, the pipe, and testthat wrappers
# each insert frames; `parent.frame()` alone lands on the dispatch frame,
# which usually has no user locals. Record-time only — the frame is used
# for substitution and then discarded.
.find_predicate_env <- function(pred, default) {
    vars <- all.vars(pred)
    if (length(vars) == 0L) return(default)
    for (i in seq_len(8L)) {
        f <- tryCatch(parent.frame(i), error = function(e) NULL)
        if (is.null(f)) break
        if (any(vapply(vars,
            function(v) exists(v, envir = f, inherits = FALSE),
            logical(1L)))) {
            return(f)
        }
    }
    default
}


# Crop region recording — WKT is the canonical form ####
#
# A recorded region is always a single WKT string. This mirrors the cascade
# GiottoDisk's `methods-spatRelate.R` already uses for its op chain rather
# than defining a second policy, and it buys three things:
#
#   * serializable: terra objects are C++ pointer-backed and do not survive
#     `saveRDS` or a trip to a parallel worker.
#   * the disk path receives WKT with no conversion at resolve time.
#   * the terra `(xmin, xmax, ymin, ymax)` convention is applied exactly
#     once, here, instead of being re-derived per substrate.
#
# WKT is geometry only — attributes are not carried through. CRS is
# deliberately NOT recorded: a recipe is re-resolved against current state
# by design, so a baked-in CRS could assert something the store it resolves
# against disagrees with. SRID stays authoritative on the store side
# (GiottoDisk's `.spatrelate_store_srid()`).

# Maximum features accepted for inline recording. A recipe is meant to be
# a compact description; a large query set belongs in a store.
.view_crop_inline_max <- function() {
    getOption("giotto.view_crop_inline_max", 1000L)
}

#' Normalize a crop region to a single WKT string.
#'
#' The WKT `character` method is the canonical entry; every other accepted
#' type coerces and recurses into it.
#' @keywords internal
#' @noRd
.normalize_crop_region <- function(y) {
    # canonical entry: WKT character
    if (is.character(y)) {
        checkmate::assert_character(y, min.len = 1L, any.missing = FALSE,
            .var.name = "region")
        if (length(y) > .view_crop_inline_max()) {
            stop(sprintf(paste0(
                "[crop] region has %d geometries, above the inline cap of ",
                "%d. Pass a single unioned geometry, or resolve against a ",
                "store instead of recording the query set inline ",
                "(see `giotto.view_crop_inline_max`)."),
                length(y), .view_crop_inline_max()), call. = FALSE)
        }
        if (length(y) > 1L) {
            # multi-feature: union into one geometry so the record is one
            # region rather than a set the resolver would have to fold
            return(.normalize_crop_region(terra::vect(y)))
        }
        # must parse as geometry
        ok <- tryCatch({
            terra::vect(y)
            TRUE
        }, error = function(e) FALSE)
        if (!ok) {
            stop("[crop] region is not valid WKT: ", y, call. = FALSE)
        }
        return(y)
    }
    if (is.numeric(y)) {
        checkmate::assert_numeric(y, len = 4L, any.missing = FALSE,
            .var.name = "region")
        # terra extent convention, applied exactly once
        return(.normalize_crop_region(terra::as.polygons(terra::ext(y))))
    }
    if (inherits(y, "SpatExtent")) {
        return(.normalize_crop_region(terra::as.polygons(y)))
    }
    if (inherits(y, "SpatVector")) {
        y_use <- if (nrow(y) > 1L) terra::aggregate(y) else y
        return(.normalize_crop_region(terra::geom(y_use, wkt = TRUE)))
    }
    if (inherits(y, c("sf", "sfc"))) {
        package_check("sf", repository = "CRAN")
        geom <- if (inherits(y, "sf")) sf::st_geometry(y) else y
        if (length(geom) > 1L) geom <- sf::st_union(geom)
        return(.normalize_crop_region(sf::st_as_text(geom)))
    }
    stop("[crop] region must be WKT character, numeric(4), SpatExtent, ",
        "SpatVector, or sf/sfc (got '", class(y)[[1L]], "')", call. = FALSE)
}


#' Deserialize a recorded crop region for substrate consumers.
#'
#' Called at the boundary where the recipe hands off to terra-backed crop
#' machinery or the in-memory relate path.
#' @keywords internal
#' @noRd
.materialize_crop_region <- function(r) {
    if (is.null(r)) return(NULL)
    terra::vect(r)
}



# Accessors ####

#' @title Slotted views on a giotto object
#' @name giottoView
#' @description
#' List, retrieve, attach, or remove the view recipes held in a
#' [giotto-class] object's `@view` slot.
#'
#' * `giottoView(g, "name")` — retrieve a view by name
#' * `giottoView(g, "name") <- v` — slot in (or replace) a view
#' * `giottoView(g, "name") <- NULL` — remove a view
#' * `giottoViews(g)` — list view names
#'
#' A view is a [giottoView-class]. There is no standalone constructor:
#' record onto a name with `subset(g, ..., view = "name")` or
#' `crop(g, ..., view = "name")` and the view is created on first use. The
#' setter exists to copy a recipe between objects, to slot one edited
#' through [giottoView-access], and to remove one. It also accepts the
#' plain nested `as.list()` form, so an exported recipe reads back in.
#'
#' Views are subset/narrowing recipes; for coordinate-frame recipes see
#' [giottoSpace].
#'
#' @param gobject a `giotto` object
#' @param name `character(1)`. The slot key.
#' @param value a `giottoView`, its `as.list()` form, or `NULL` to remove.
#' @param ... additional arguments, currently unused
#' @returns the view, an updated gobject, or a character vector of view names
#' @examples
#' g <- giotto()
#' g <- subset(g, samples = c("s1", "s2"), view = "demo")
#' giottoViews(g)
#' giottoView(g, "demo")
NULL

#' @rdname giottoView
#' @export
setGeneric("giottoView",
    function(gobject, name, ...) standardGeneric("giottoView"))

#' @rdname giottoView
#' @export
setGeneric("giottoView<-",
    function(gobject, name, ..., value) standardGeneric("giottoView<-"))

#' @rdname giottoView
#' @export
setGeneric("giottoViews",
    function(gobject, ...) standardGeneric("giottoViews"))

#' @rdname giottoView
#' @export
setMethod("giottoView", signature(gobject = "gAny", name = "character"),
    function(gobject, name, ...) {
        checkmate::assert_character(name, len = 1L)
        v <- gobject@view[[name]]
        if (is.null(v)) {
            stop("no view named '", name, "'. ",
                "Available: ", paste(giottoViews(gobject), collapse = ", "),
                call. = FALSE)
        }
        v
    }
)

#' @rdname giottoView
#' @export
setMethod("giottoView", signature(gobject = "gAny", name = "missing"),
    function(gobject, name, ...) {
        nm <- giottoViews(gobject)
        if (length(nm) == 0L) return(NULL)
        if (length(nm) == 1L) return(gobject@view[[nm]])
        stop("multiple views slotted; specify `name`. ",
            "Available: ", paste(nm, collapse = ", "), call. = FALSE)
    }
)

#' @rdname giottoView
#' @export
setMethod("giottoView<-",
    signature(gobject = "gAny", name = "character", value = "ANY"),
    function(gobject, name, ..., value) {
        checkmate::assert_character(name, len = 1L)
        # coerces the `as.list()` export form and re-checks a hand-edited
        # recipe, so the boundary where a recipe enters an object is also
        # where it is validated
        value <- .validate_view(value, .var.name = "value")
        # A slotted entry holds exactly the view it is keyed under, so the
        # name travels with the recipe and `setGiotto()` can place it.
        value@name <- name
        if (is.null(gobject@view)) gobject@view <- list()
        gobject@view[[name]] <- value
        gobject
    }
)

#' @rdname giottoView
#' @export
setMethod("giottoView<-",
    signature(gobject = "gAny", name = "character", value = "NULL"),
    function(gobject, name, ..., value) {
        if (is.null(gobject@view) || !name %in% names(gobject@view)) {
            return(gobject)
        }
        gobject@view[[name]] <- NULL
        gobject
    }
)

#' @rdname giottoView
#' @export
setMethod("giottoViews", signature(gobject = "gAny"),
    function(gobject, ...) {
        nm <- names(gobject@view)
        if (is.null(nm)) character() else nm
    }
)


# resolve — container methods ####
#
# A container evaluates the recipe once and walks its slots; the leaves
# (`methods-resolver.R`) apply what they are handed. See `?resolve`.
#
# Container methods register against `coordinator = "ANY"` rather than
# `"missing"`. A container does not dispatch on the coordinator -- it picks
# one from `@source` when none is given -- but it still accepts one, and a
# supplied coordinator must not change which method runs.


# All slots a container walks, in the canonical order (tabular -> spatial ->
# images). Used as the default slot set when `slots = NULL` and to validate
# caller-supplied slot names.
.resolve_default_slots <- c(
    "cell_metadata", "expression", "dimension_reduction",
    "spatial_enrichment", "feat_metadata",
    "spatial_locs", "spatial_info", "feat_info", "images"
)


# Validate and order a caller-supplied slot vector against the
# canonical walk order. NULL → all default slots. Unknown slot names
# error.
#' @keywords internal
#' @noRd
.resolve_slot_filter <- function(slots) {
    if (is.null(slots)) return(.resolve_default_slots)
    bad <- setdiff(slots, .resolve_default_slots)
    if (length(bad) > 0L) {
        stop(sprintf(
            "[resolve] unknown slot(s): %s. Available: %s",
            paste(bad, collapse = ", "),
            paste(.resolve_default_slots, collapse = ", ")
        ), call. = FALSE)
    }
    intersect(.resolve_default_slots, slots)  # canonical order
}

# Normalise the `view` argument at a container entry point.
#
# View contract: character(1) name of a slotted view, or NULL. Inline
# giottoView objects were considered and rejected — views are curated
# artifacts; build + slot via `giottoView(g, name) <- v` if programmatic
# composition is needed. See vignettes/articles/design_view_space.Rmd.
#
# The internal `.resolve_giotto()` / `.resolve_gmulti()` pair takes the
# looked-up object instead, which is how the gmulti per-child loop hands each
# child a recipe the child could not have looked up itself.
#' @keywords internal
#' @noRd
.resolve_view_arg <- function(gobject, view) {
    if (is.null(view)) return(NULL)
    checkmate::assert_string(view, .var.name = "view")
    giottoView(gobject, view)
}

# Internal implementation: resolve on a giotto with an already-resolved
# giottoView object. Called from the public method (after slot lookup) and
# from the giottoMulti per-child loop (where the view object is in hand).
#' @keywords internal
#' @noRd
.resolve_giotto <- function(gobject, view,
                            space = NULL,
                            coordinator = NULL,
                            slots = NULL,
                            spat_unit = NULL,
                            feat_type = NULL,
                            spaces = gobject@spaces,
                            ...) {
    if (is.null(coordinator)) {
        coordinator <- .default_view_coordinator(gobject)
    }
    scope <- .resolve_scope(gobject, spat_unit, feat_type)
    # Normalise output space to a giottoSpace (or NULL) once at the
    # entry point so per-subobject resolution doesn't re-look-up by
    # name. The predicate space (the view's `space`) is consulted independently
    # by the crop step handlers — it is not conflated with output here.
    space_obj <- .resolve_space(gobject, space)
    # The op's one surviving cell set, held as a promise: computed at most
    # once per call, and not at all when no leaf reads it.
    cache <- .new_resolver_cache(gobject, view, coordinator,
        spat_unit = scope$spat_unit, feat_type = scope$feat_type)

    out <- gobject

    # Walk the (possibly filtered) slot list in canonical order:
    # tabular → spatial → images. Slot names not in `slots`, and leaves
    # outside the op's spat_unit / feat_type, are left untouched.
    for (slot_name in .resolve_slot_filter(slots)) {
        out <- .resolve_walk(out, slot_name,
            view, space_obj, coordinator, cache, scope, spaces)
    }

    # Networks (spatial_network, nn_network) intentionally not walked:
    # they're built from a particular cell state and don't carry
    # spatial coords; view/space resolution would be misleading.

    out
}

#' @rdname resolve
#' @export
setMethod("resolve", signature(x = "giotto", coordinator = "ANY"),
    function(x, coordinator = NULL, view = NULL, space = NULL,
             slots = NULL, spat_unit = NULL, feat_type = NULL, ...) {
        .resolve_giotto(x, .resolve_view_arg(x, view),
            space = space, coordinator = coordinator,
            slots = slots, spat_unit = spat_unit, feat_type = feat_type, ...)
    }
)


# resolve on giottoMulti ####
# 1. Apply the samples step FIRST — narrow children before any per-child
#    work touches storage (matters at 4B-points-per-multi scale).
# 2. Per-surviving-child resolve with the child-scoped giottoSpace.
# 3. Narrow joint shared slots via the same leaf dispatch — spatValues works
#    on a multi, so joint-level predicates resolve against joint slots and the
#    surviving global cell_IDs narrow each joint subobject.

# Internal implementation: resolve on a giottoMulti with an already-resolved
# giottoView object.
#' @keywords internal
#' @noRd
.resolve_gmulti <- function(gobject, view,
                            space = NULL,
                            coordinator = NULL,
                            slots = NULL,
                            spat_unit = NULL,
                            feat_type = NULL,
                            ...) {
    if (is.null(coordinator)) {
        coordinator <- .default_view_coordinator(gobject)
    }
    scope <- .resolve_scope(gobject, spat_unit, feat_type)
    space_obj <- .resolve_space(gobject, space)
    cache <- .new_resolver_cache(gobject, view, coordinator,
        spat_unit = scope$spat_unit, feat_type = scope$feat_type)

    # Resolve the samples step FIRST — narrow children before any
    # per-child work touches storage.
    selected <- .resolve_sample_select(gobject, view)
    if (length(selected) == 1L && is.na(selected)) {
        selected <- names(gobject@objects)
    } else {
        selected <- intersect(selected, names(gobject@objects))
    }

    out <- gobject
    out@objects <- gobject@objects[selected]

    # Per-surviving-child resolve with the child-scoped space.
    # `slots` is forwarded so per-child narrowing matches the joint-level
    # scope.
    # The scope is in multi-level handles; @mapping may name them
    # differently in each child.
    su_map <- .gm_scope_map(gobject, "spat_unit", scope$spat_unit)
    ft_map <- .gm_scope_map(gobject, "feat_type", scope$feat_type)
    out@objects <- stats::setNames(lapply(selected, function(samp) {
        child <- out@objects[[samp]]
        child_su <- .gm_scope_child(su_map, samp)
        child_ft <- .gm_scope_child(ft_map, samp)
        # A child that does not carry the requested handle holds nothing in
        # scope, so it is left as it is.
        if (identical(child_su, NA) || identical(child_ft, NA)) return(child)
        # `[` owns the sample-resolution rule; the child then reads as a
        # single-sample object against the handle it is handed. The frames
        # a crop step names live on the parent, so the child is handed the
        # parent's, scoped to itself the same way.
        child_space <- if (is.null(space_obj)) NULL else space_obj[samp]
        child_spaces <- lapply(gobject@spaces, function(sp) sp[samp])
        .resolve_giotto(child, view, space = child_space,
            coordinator = coordinator, slots = slots,
            spat_unit = child_su, feat_type = child_ft,
            spaces = child_spaces, ...)
    }), selected)

    # Narrow joint shared slots. Only multi-level cell_metadata /
    # expression / dim_reduction / spatial_enrichment / feat_metadata are
    # legitimately joint, so intersect the filter with that subset.
    joint_candidates <- c("cell_metadata", "expression",
        "dimension_reduction", "spatial_enrichment", "feat_metadata")
    joint_slots <- intersect(.resolve_slot_filter(slots), joint_candidates)
    for (slot_name in joint_slots) {
        out <- .resolve_walk(out, slot_name,
            view, space_obj, coordinator, cache, scope, gobject@spaces)
    }

    out
}

#' @rdname resolve
#' @export
setMethod("resolve", signature(x = "giottoMulti", coordinator = "ANY"),
    function(x, coordinator = NULL, view = NULL, space = NULL,
             slots = NULL, spat_unit = NULL, feat_type = NULL, ...) {
        .resolve_gmulti(x, .resolve_view_arg(x, view),
            space = space, coordinator = coordinator,
            slots = slots, spat_unit = spat_unit, feat_type = feat_type, ...)
    }
)


# `view` and `space` are independent knobs, so asking for a frame without also
# naming a view is a normal request, not a degenerate one. Everything below
# the dispatch already treats a NULL view as "no narrowing"
# (`.view_steps_of()` returns an empty step list, `.surviving_cell_ids()`
# returns NULL), so `view = NULL` needs no method of its own — it is the
# default. Under `materialize()` this was a separate `view = "NULL"` S4
# method, because `view` sat in the dispatch signature; collapsing that split
# to a null check inside one method is what it always was, wearing dispatch.


# Walk one slot list (potentially nested by spat_unit / feat_type), calling
# the leaf dispatcher on each subobject. The slot is a `nullOrList`; structure
# is recursive — list of lists of subobjects. Apply leaf-wise.
#' @keywords internal
#' @noRd
.resolve_walk <- function(gobject, slot_name, view, space, coordinator,
                          cache, scope, spaces) {
    x <- methods::slot(gobject, slot_name)
    if (is.null(x) || length(x) == 0L) return(gobject)
    methods::slot(gobject, slot_name) <- .resolve_apply(
        x, gobject, view, space, coordinator, cache, scope, spaces)
    gobject
}

.resolve_apply <- function(node, gobject, view, space, coordinator,
                           cache, scope, spaces) {
    if (is.list(node) && !isS4(node)) {
        return(lapply(node, .resolve_apply, gobject = gobject,
            view = view, space = space, coordinator = coordinator,
            cache = cache, scope = scope, spaces = spaces))
    }
    if (isS4(node) && inherits(node, "giottoSubobject")) {
        if (!.leaf_in_scope(node, scope$spat_unit, scope$feat_type)) {
            return(node)
        }
        return(.resolve_leaf(node, gobject, view, space, coordinator, cache,
            spaces))
    }
    node
}

# Per-sample child names for a multi-level scope handle. `NULL` when the
# scope leaves that axis open.
#' @keywords internal
#' @noRd
.gm_scope_map <- function(gobject, axis, handle) {
    if (is.null(handle)) return(NULL)
    .gm_resolve_axis(gobject, axis, handle)$map
}

# One child's name for the handle: `NULL` for an open axis, `NA` when the
# child does not participate in it.
#' @keywords internal
#' @noRd
.gm_scope_child <- function(map, samp) {
    if (is.null(map)) return(NULL)
    if (samp %in% names(map)) map[[samp]] else NA
}

# The op's scope: one spat_unit (whose cell_ID vocabulary the surviving set
# is in) and one feat_type, defaulting to the active ones. `NULL` survives
# only when the object has no default to offer (an image-only object), and
# then scopes nothing on that axis.
#' @keywords internal
#' @noRd
.resolve_scope <- function(gobject, spat_unit = NULL, feat_type = NULL) {
    spat_unit <- suppressWarnings(set_default_spat_unit(gobject, spat_unit))
    feat_type <- suppressWarnings(
        set_default_feat_type(gobject, feat_type, spat_unit = spat_unit))
    list(spat_unit = spat_unit, feat_type = feat_type)
}


# Deprecated: materialize ####
#
# `materialize` is the wrong word for what this does, and the word is already
# load-bearing elsewhere: {GiottoDisk} uses it throughout for pulling lazy or
# backed data into memory. This function is backend-agnostic and returns a
# gobject whose subobjects are still stores, which is the one thing that
# reading makes you expect. `resolve` is the subsystem's own word — the
# resolver, the coordinators as resolver backends — so the verb now matches.
#
# (`.materialize_crop_region()` above keeps its name: turning a recorded WKT
# string into a concrete region really is materialisation in the usual sense.)

#' @title materialize a giottoView into a new gobject
#' @name materialize
#' @description Deprecated in 0.7.3. Superseded by [resolve()].
#' @param gobject a `giotto` object
#' @param view either `NULL` or a `character(1)` slot key
#' @param space `character(1)` optional — name of a slotted `giottoSpace`
#' @param coordinator a [viewCoordinator-class]-inheriting object
#' @param slots optional `character` vector of slot names to narrow
#' @param ... reserved
#' @returns a new `giotto` object reflecting the resolved view
#' @keywords internal
#' @export
setGeneric("materialize",
    function(gobject, view = NULL, ...) standardGeneric("materialize"))

#' @rdname materialize
#' @export
setMethod("materialize", signature(gobject = "gAny", view = "ANY"),
    function(gobject, view, space = NULL, coordinator = NULL,
             slots = NULL, ...) {
        deprecate_soft("0.7.3", "materialize()", "resolve()")
        resolve(gobject, coordinator = coordinator, view = view,
            space = space, slots = slots, ...)
    }
)


# Q8 removed `show(giottoView)` / `show(giottoSpace)` along with the
# classes, and with them `.view_step_label()` / `.space_step_label()` /
# `.wkt_label()`, which had no other callers. Recipes now print as the
# lists they are. Note a crop step holds a full WKT string, so a real
# polygon prints long -- a summary on `show(giotto)` is the natural
# replacement and is deliberately not part of this change.
