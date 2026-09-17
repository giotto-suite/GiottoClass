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

# Append a transform step to a space, carrying its own `samples` scope.
#
# Scope lives ON THE STEP, not on the list it joins. One ordered list then
# replays correctly for every sample, including a sample first named after
# a broadcast step was already recorded -- which a sample-keyed layout
# cannot do, because "append to every key so far" never reaches a key that
# appears later.
#
#   mg <- spin(mg, 30, space = "atlas")                      # everyone
#   mg <- spatShift(mg, dx = 8000, space = "atlas", samples = "b")
#   mg <- spin(mg, 10, space = "atlas")                      # everyone
#
# Sample "b" replays all three, in order, even though it was unknown to the
# space when the first was recorded.
#
# On a `combinedSpace`, naming samples also widens the membership -- they
# are in the layout by virtue of being placed in it -- and does so without
# a second slot, because membership IS what the steps name. A member that
# needs no transform of its own gets a membership step instead; see
# `.space_step_member()`.
#
# Q8 note on `+`: the pre-Q8 version appended to every keyed sample
# unconditionally, so the scope of a transform depended on how much of the
# recipe had been merged before it — invisible at the call site and
# unrecoverable from the recorded steps. Scope is stated per call now, and
# `+` merges two already-scoped handles rather than seeding scope.
.space_record <- function(space, op, args, samples = NULL) {
    space@steps <- c(space@steps,
        list(.space_step_transform(op, args, samples = samples)))
    space
}


# Indirect-usage routing: lets the transform generics (spin / spatShift /
# affine / flip / rescale / shear / zoom) on a `giotto` / `giottoMulti`
# accept `space = "<name>"` and record the transform as a step instead of
# applying it. Returns the gobject with the named space created or appended.
#
# Recording runs through `.space_record()`, the same builder the handle
# methods use, so a step has one construction path whichever surface asked
# for it. `samples` accepts child names or `@groups` names; see below for
# why groups expand here rather than at resolution.
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
    # -- a step that looks recorded and does nothing. Resolve here, where
    # the children are known.
    #
    # Group names expand NOW, unlike the `samples =` getters, which stay
    # late-bound. A space keys its step chains by sample, so a symbolic key
    # would leave a child reachable through both its own key and a group's
    # with two chains and no defined order between them. Expanding also
    # makes the recorded frame mean what the group meant when the user
    # asked for it: a later group edit does not silently reach back and
    # change which children a transform already applies to.
    if (!is.null(samples) && inherits(gobject, "giottoMulti")) {
        samples <- .gm_resolve_samples(gobject, samples, op)
    }
    # Recording onto an unused name DECLARES a per-sample space, and a
    # combined one has to say so:
    # `giottoSpace(g, nm) <- combinedSpace(...)`.
    #
    # The kind decides job size (adr/0006), and the two sizes are not
    # symmetric under get/set. A per-sample job writes one artifact per
    # child, which is the shape reading per child hands back, so content
    # can go out and come back. A combined job writes ONE artifact at the
    # parent, and there is deliberately nowhere to put per-sample content
    # back at the parent (federation §13), so that round trip does not
    # close. The kind you get for free should be the one that composes.
    #
    # Both kinds accept `samples =` -- scoping says which samples MOVE, not
    # whether they INTERACT, so it cannot be the signal that decides the
    # kind.
    existing <- if (space %in% giottoSpaces(gobject)) {
        giottoSpace(gobject, space)
    } else {
        .new_per_sample_space(space)
    }
    new_space <- .space_record(existing, op, args, samples = samples)
    giottoSpace(gobject, space) <- new_space
    gobject
}


# Record methods on the transform generics — giottoMulti ####
#
# `samples =` exists ONLY here. A `giotto` holds one sample, so there is no
# name a step could scope to — scoping is meaningful only once there are
# children to scope to, and an unscoped step already reaches the one sample
# there is. The `giotto` methods keep their plain `space = "<name>"` arm
# (see methods-spin.R and friends).
#
# These are record-only. An eager per-child transform is a separate
# feature: it would have to walk every child's subobjects, and the
# cross-sample layout that motivates gmulti transforms in the first place
# is precisely what a recorded space expresses instead.

# Is `space` a name this object can resolve? Raises if not.
#
# The membership test lives HERE rather than at the call site, with the
# resolvable list built here too.
#' @noRd
.assert_space_known <- function(gobject, space) {
    # check against default space and existing spaces
    known <- giottoSpaces(gobject)
    if (space %in% known) return(invisible(TRUE))

    msg <- sprintf("[space] '%s' is not a registered space.\n Available: %s",
        space, toString(known)
    )
    stop(msg, call. = FALSE)
}

# space = NULL means eager transform. This is not permitted in gmulti.
# this helper guards against that and points what to do instead
.assert_space_required <- function(space, op) {
    if (!is.null(space)) return(invisible(TRUE))
    stop(sprintf(paste0(
        "[%s] a giottoMulti transform requires `space = \"<name>\"`. ",
        "Eager per-child transforms are not implemented. Record onto a ",
        "named space (scope it with `samples = ` if it should apply to ",
        "some children only)"),
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
#' A handle holds exactly one frame, so `giottoSpace(g)` with no `name`
#' returns a named `list` of them; `giottoSpace(g, "name")` returns the one.
#'
#' The native frame — the one the data is already in — has no name.
#' `space = NULL` is it, everywhere. A sentinel name would be a second
#' spelling of a value R already has, and a transform cannot be recorded
#' onto the native frame anyway: the result would not be native.
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
        # The one place a space name is checked. Every caller that checks
        # goes on to fetch, so folding the two together means a consumer
        # cannot do one without the other -- and the diagnosis for a name
        # that is really a sample or a group is available everywhere
        # rather than at whichever call site remembered to ask for it.
        .assert_space_known(gobject, name)
        gobject@spaces[[name]]
    }
)

#' @rdname giottoSpace
#' @export
setMethod("giottoSpace", signature(gobject = "gAny", name = "missing"),
    function(gobject, name, ...) {
        sp <- gobject@spaces
        if (length(sp) == 0L) return(NULL)
        # A handle holds one frame, so a collection is a list of handles
        # rather than one wider handle.
        sp
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
        # A slotted entry holds exactly the frame it is keyed under.
        value@name <- name
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
