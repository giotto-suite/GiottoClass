#' @include classes-space.R
#' @include classes.R
#' @include generics.R
NULL

# =============================================================================
# methods-space.R — the space recorder and accessors
#
# Strategy: the existing GiottoClass spatial transform generics (`spin`,
# `spatShift`, `affine`, `flip`, `rescale`, `shear`, `zoom`) take
# `space = "<name>"` on a `giotto` / `giottoMulti` and RECORD the call as a
# transform step on that named space, scoped by `samples =`. Eager dispatch
# without `space` is unchanged.
#
# There is no space-receiver surface and no `+` (decision Q8) — `samples =`
# replaces both, and states the scope at the call site instead of inheriting
# it from the order the recipe was built in.
# =============================================================================


#' @rdname giottoSpace
#' @export
setGeneric("giottoSpace",
    function(gobject, name, ...) standardGeneric("giottoSpace"))

#' @rdname giottoSpace
#' @export
setGeneric("giottoSpace<-",
    function(gobject, name, ..., value) standardGeneric("giottoSpace<-"))

#' @rdname giottoSpace
#' @export
setGeneric("giottoSpaces",
    function(gobject, ...) standardGeneric("giottoSpaces"))

# Internal helpers ####

# Append a transform step to every frame a handle holds, scoped to
# `samples`.
#
# `samples = NULL` means "every sample already keyed in this frame", which
# for a fresh frame is the `:default:` sentinel alone. That covers the
# single-`giotto` case, the "move the whole layout" case, and -- because
# `sp[s, k]` returns a handle keyed on exactly `k` -- appending to one
# sample without naming it twice.
#
# Naming samples that are not yet keyed CREATES those keys, which is how a
# cross-sample layout is built one call at a time:
#
#   mg <- affine(mg, M_a, space = "atlas", samples = "sample_a")
#   mg <- spatShift(mg, dx = 8000, space = "atlas", samples = "sample_b")
#
# Q8 note on `+`: the pre-Q8 version appended to every keyed sample
# unconditionally, so the scope of a transform depended on how much of the
# recipe had been merged before it — invisible at the call site and
# unrecoverable from the recorded steps. Scope is stated per call now, and
# `+` merges two already-scoped handles rather than seeding scope.
.space_record <- function(space, op, args, samples = NULL) {
    step <- .space_step_transform(op, args)
    space@spaces <- lapply(space@spaces, function(keyed) {
        if (is.null(samples)) {
            if (length(keyed) == 0L) {
                return(stats::setNames(list(list(step)),
                    .space_default_sample))
            }
            return(lapply(keyed, function(steps) c(steps, list(step))))
        }
        for (samp in unique(samples)) {
            keyed[[samp]] <- c(keyed[[samp]], list(step))
        }
        keyed
    })
    space
}


# Indirect-usage routing: lets the transform generics (spin / spatShift /
# affine / flip / rescale / shear / zoom) on a `giotto` / `giottoMulti`
# accept `space = "<name>"` and record the transform as a step instead of
# applying it. Returns the gobject with the named space created or appended.
#
# Recording runs through `.space_record()`, the same builder the handle
# methods use, so a step has one construction path whichever surface asked
# for it. `samples` is passed straight through; see there for the scoping
# rules. It will also accept `@groups` names once those land (stage 7),
# resolved through `.gm_slice_to_samples()`.
.record_space_on_gobject <- function(gobject, space, op, args,
    samples = NULL) {
    if (!is.character(space)) {
        stop("`space` must be NULL (eager) or a character name, not a ",
            class(space)[[1L]], ". Spaces are identified by name; record ",
            "onto one with `space = \"<name>\"`.", call. = FALSE)
    }
    checkmate::assert_character(space, len = 1L, any.missing = FALSE)
    # `.space_record()` creates whatever key it is given, so a mistyped
    # sample name would record a chain that no child ever resolves against
    # -- a step that looks recorded and does nothing. Check it here, where
    # the children are known.
    if (!is.null(samples) && inherits(gobject, "giottoMulti")) {
        avail <- names(gobject@objects)
        bad <- setdiff(samples, avail)
        if (length(bad) > 0L) {
            stop(sprintf(
                "[%s] sample(s) '%s' are not children of this giottoMulti (have: %s)",
                op, paste(bad, collapse = ", "),
                paste(avail, collapse = ", ")), call. = FALSE)
        }
    }
    existing <- if (space %in% giottoSpaces(gobject)) {
        giottoSpace(gobject, space)
    } else {
        .new_space(stats::setNames(list(list()), space))
    }
    new_space <- .space_record(existing, op, args, samples = samples)
    giottoSpace(gobject, space) <- new_space
    gobject
}


# Record methods on the transform generics — giottoMulti ####
#
# `samples =` exists ONLY here. A `giotto` holds one sample, so the only key
# it could name is the `:default:` sentinel — scoping is meaningful only
# once there are children to scope to. The `giotto` methods keep their
# plain `space = "<name>"` arm (see methods-spin.R and friends).
#
# These are record-only. An eager per-child transform is a separate
# feature: it would have to walk every child's subobjects, and the
# cross-sample layout that motivates gmulti transforms in the first place
# is precisely what a recorded space expresses instead.

# Shared guard, so the five methods below stay one line each.
.assert_space_required <- function(space, op) {
    if (!is.null(space)) return(invisible(TRUE))
    stop(sprintf(paste0(
        "[%s] a giottoMulti transform requires `space = \"<name>\"`. ",
        "Eager per-child transforms are not implemented. Record onto a ",
        "named space (scope it with `samples = ` if it should apply to ",
        "some children only), then apply it with `materialize()`."),
        op), call. = FALSE)
}

#' @rdname spin
#' @export
setMethod("spin", signature(x = "giottoMulti"),
    function(x, angle, x0 = NULL, y0 = NULL, space = NULL,
             samples = NULL, ...) {
        .assert_space_required(space, "spin")
        .record_space_on_gobject(x, space, "spin",
            list(angle = angle, x0 = x0, y0 = y0), samples = samples)
    }
)

#' @rdname spatShift
#' @export
setMethod("spatShift", signature(x = "giottoMulti"),
    function(x, dx = 0, dy = 0, space = NULL, samples = NULL, ...) {
        .assert_space_required(space, "spatShift")
        .record_space_on_gobject(x, space, "spatShift",
            list(dx = dx, dy = dy), samples = samples)
    }
)

#' @rdname affine
#' @export
setMethod("affine", signature(x = "giottoMulti", y = "ANY"),
    function(x, y, inv = FALSE, space = NULL, samples = NULL, ...) {
        .assert_space_required(space, "affine")
        .record_space_on_gobject(x, space, "affine",
            c(list(y = y, inv = inv), list(...)), samples = samples)
    }
)

#' @rdname flip
#' @export
setMethod("flip", signature(x = "giottoMulti"),
    function(x, direction = "vertical", x0 = 0, y0 = 0, space = NULL,
             samples = NULL, ...) {
        .assert_space_required(space, "flip")
        .record_space_on_gobject(x, space, "flip",
            list(direction = direction, x0 = x0, y0 = y0),
            samples = samples)
    }
)

#' @rdname rescale
#' @export
setMethod("rescale", signature(x = "giottoMulti"),
    function(x, fx = 1, fy = fx, x0, y0, space = NULL, samples = NULL,
             ...) {
        .assert_space_required(space, "rescale")
        args <- list(fx = fx, fy = fy)
        if (!missing(x0)) args$x0 <- x0
        if (!missing(y0)) args$y0 <- y0
        .record_space_on_gobject(x, space, "rescale", args,
            samples = samples)
    }
)


# Record methods on the transform generics — giottoSpace ####
#
# The append half of the handle API: the same seven verbs, applied to a
# recipe rather than through a gobject. `samples = NULL` appends to every
# sample the handle is keyed on, so scoping with `[` first is how a single
# sample is targeted:
#
#   sp <- giottoSpace(mg, "atlas")
#   giottoSpace(mg, "atlas") <- sp + spin(sp["atlas", "sample_b"], 30)
#
# All seven route through `.space_record()`, which is also what the
# gobject-level `space = ` arms call.

#' @rdname spin
#' @export
setMethod("spin", signature(x = "giottoSpace"),
    function(x, angle, x0 = NULL, y0 = NULL, samples = NULL, ...) {
        .space_record(x, "spin", list(angle = angle, x0 = x0, y0 = y0),
            samples = samples)
    }
)

#' @rdname spatShift
#' @export
setMethod("spatShift", signature(x = "giottoSpace"),
    function(x, dx = 0, dy = 0, samples = NULL, ...) {
        .space_record(x, "spatShift", list(dx = dx, dy = dy),
            samples = samples)
    }
)

#' @rdname affine
#' @export
setMethod("affine", signature(x = "giottoSpace", y = "ANY"),
    function(x, y, inv = FALSE, samples = NULL, ...) {
        .space_record(x, "affine", c(list(y = y, inv = inv), list(...)),
            samples = samples)
    }
)

#' @rdname flip
#' @export
setMethod("flip", signature(x = "giottoSpace"),
    function(x, direction = "vertical", x0 = 0, y0 = 0, samples = NULL,
             ...) {
        .space_record(x, "flip",
            list(direction = direction, x0 = x0, y0 = y0),
            samples = samples)
    }
)

#' @rdname rescale
#' @export
setMethod("rescale", signature(x = "giottoSpace"),
    function(x, fx = 1, fy = fx, x0, y0, samples = NULL, ...) {
        args <- list(fx = fx, fy = fy)
        if (!missing(x0)) args$x0 <- x0
        if (!missing(y0)) args$y0 <- y0
        .space_record(x, "rescale", args, samples = samples)
    }
)

#' @rdname shear
#' @export
setMethod("shear", signature(x = "giottoSpace"),
    function(x, fx = 0, fy = 0, x0, y0, samples = NULL, ...) {
        args <- list(fx = fx, fy = fy)
        if (!missing(x0)) args$x0 <- x0
        if (!missing(y0)) args$y0 <- y0
        .space_record(x, "shear", args, samples = samples)
    }
)

#' @rdname zoom
#' @export
setMethod("zoom", signature(x = "giottoSpace"),
    function(x, f = 1, samples = NULL, ...) {
        .space_record(x, "zoom", list(f = f), samples = samples)
    }
)


# Accessors ####

#' @title Slotted spaces on a giotto object
#' @name giottoSpace
#' @description
#' List, retrieve, attach, or remove [giottoSpace] objects slotted into
#' a [giotto-class] object's `@spaces` slot.
#'
#' * `giottoSpace(g, "name")` — retrieve a space by name
#' * `giottoSpace(g, "name") <- s` — slot in (or replace) a space
#' * `giottoSpace(g, "name") <- NULL` — remove a space
#' * `giottoSpaces(g)` — list space names
#'
#' Multiple slotted spaces serve as named alternate coordinate frames of the
#' same gobject. Consumer functions opt into a frame via the `space =`
#' parameter.
#'
#' A space is a [giottoSpace-class]. There is no standalone constructor:
#' record onto a name with a transform verb, e.g.
#' `spatShift(g, dx = 10, space = "shifted")`, and the space is created on
#' first use. On a `giottoMulti`, `samples =` scopes the transform to named
#' children. The setter exists to copy a recipe between objects, to slot
#' one edited through [giottoSpace-access], and to remove one. It also
#' accepts the plain nested `as.list()` form, so an exported recipe reads
#' back in.
#'
#' `giottoSpace(g)` with no `name` returns every slotted frame in one
#' handle; `giottoSpace(g, "name")` returns just that one.
#'
#' @param gobject a `giotto` object
#' @param name `character(1)`. The slot key.
#' @param value a `giottoSpace`, its `as.list()` form, or `NULL` to remove.
#' @param ... additional arguments (none currently used)
#' @returns the space, an updated gobject, or a character vector of space names
#' @examples
#' g <- giotto()
#' g <- spatShift(g, dx = 10, space = "demo")
#' giottoSpaces(g)
#' giottoSpace(g, "demo")
NULL

#' @rdname giottoSpace
#' @export
setMethod("giottoSpace", signature(gobject = "gAny", name = "character"),
    function(gobject, name, ...) {
        checkmate::assert_character(name, len = 1L)
        s <- gobject@spaces[[name]]
        if (is.null(s)) {
            stop("no space named '", name, "'. ",
                "Available: ", paste(giottoSpaces(gobject), collapse = ", "),
                call. = FALSE)
        }
        s
    }
)

#' @rdname giottoSpace
#' @export
setMethod("giottoSpace", signature(gobject = "gAny", name = "missing"),
    function(gobject, name, ...) {
        nm <- giottoSpaces(gobject)
        if (length(nm) == 0L) return(NULL)
        # the whole collection in one handle -- `sp[name]` narrows it
        Reduce(`+`, gobject@spaces)
    }
)

#' @rdname giottoSpace
#' @export
setMethod("giottoSpace<-",
    signature(gobject = "gAny", name = "character", value = "ANY"),
    function(gobject, name, ..., value) {
        checkmate::assert_character(name, len = 1L)
        # coerces the `as.list()` export form and re-checks a hand-edited
        # recipe, so the boundary where a recipe enters an object is also
        # where it is validated
        value <- .validate_space(value, .var.name = "value")
        # A slotted entry holds exactly the frame it is keyed under; `[`
        # and `+` can produce a handle over several, and silently keeping
        # the extras would put a frame under a name that is not its own.
        if (length(value) > 1L) {
            stop("[space] `value` holds ", length(value), " frames (",
                paste(names(value), collapse = ", "),
                "); slot one at a time, e.g. `value[\"", name, "\"]`.",
                call. = FALSE)
        }
        if (length(value) == 1L) names(value@spaces) <- name
        if (is.null(gobject@spaces)) gobject@spaces <- list()
        gobject@spaces[[name]] <- value
        gobject
    }
)

#' @rdname giottoSpace
#' @export
setMethod("giottoSpace<-",
    signature(gobject = "gAny", name = "character", value = "NULL"),
    function(gobject, name, ..., value) {
        if (is.null(gobject@spaces) ||
            !name %in% names(gobject@spaces)) return(gobject)
        gobject@spaces[[name]] <- NULL
        gobject
    }
)

#' @rdname giottoSpace
#' @export
setMethod("giottoSpaces", signature(gobject = "gAny"),
    function(gobject, ...) {
        nm <- names(gobject@spaces)
        if (is.null(nm)) character() else nm
    }
)
