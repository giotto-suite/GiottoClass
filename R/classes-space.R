# =============================================================================
# giottoSpace — coordinate-frame recipe (parallel space opt-in)
# =============================================================================
#
# A space describes a coordinate frame for a `giotto` (single-sample) or
# `giottoMulti` (multi-sample) object. Slotted spaces are named alternate
# coordinate frames that consumer functions opt into via `space = "name"`.
#
# Unlike views (which are read-only narrowings), spaces are NOT subject to
# the read-only contract — analyses run in a non-native space are fine; the
# coordinate frame just differs. Mutations still target the underlying data
# in its native frame.
#
# Sample scope: transforms in a `giottoSpace` are SAMPLE-UNIFORM. Within a
# single sample, all spatial elements (cells, polys, points, image,
# spatlocs) move together. Per-element overrides are deliberately not
# supported here — sample-level is the granularity that matches the typical
# spatialomics alignment workflow.
#
# Shape — spaces are built through the gobject, by name:
#   # single-sample (giotto)
#   g <- affine(g, M, space = "tilted")
#
#   # multi-sample (giottoMulti). `samples =` scopes the transform to named
#   # children, which is what a cross-sample layout needs.
#   mg <- affine(mg, M_a, space = "atlas", samples = "sample_a")
#   mg <- spatShift(mg, dx = 8000, space = "atlas", samples = "sample_b")
#
# Recording twice against the same sample concatenates steps in order.
# `samples = NULL` records against every sample already keyed in the space,
# and on a new space makes it a `perSampleSpace` -- a frame that applies to
# each sample independently, which is what a single `giotto` always wants.
#
# Q8 replaced `+` and the sample-keyed constructor `giottoSpace("sample_a")`
# with `samples =`. The old form inherited scope from construction history:
# `.space_record()` appended to every sample keyed so far, so
# `(a + b) |> spin(30)` differed from `(a |> spin(30)) + b` with nothing at
# the call site to say which had happened.
#
# Step taxonomy — one type, `"transform"`: a deferred call to one of the
# GiottoClass spatial transform generics (`affine`, `spin`, `spatShift`,
# `flip`, `rescale`, `shear`, `zoom`). At resolution time the receiving
# gobject (or child) is spliced as the first argument and `do.call()`
# dispatches to the existing transform method.
#
# Steps are recorded individually and never folded at record time, which is
# what keeps a recipe hand-editable. Application is currently stepwise too
# -- one pass per step per subobject -- and should not be: all the ops
# except `zoom` are affine, and `affine2d` already composes them. Planned,
# with the shape and the exactness argument, in
# vignettes/articles/IMPLEMENTATION_viewspace.md section 5.
#
# A handle holds exactly ONE frame, named by `@name`. `giottoSpace` is
# virtual and the kind of frame is the class:
#
#   perSampleSpace  @steps    list(<step>, ...)
#   combinedSpace   @samples  list("<sample>" = list(<step>, ...))
#
# The split is the answer to "which samples participate, and do they
# interact". A `combinedSpace` names its members, so it declares a job over
# them; a `perSampleSpace` is structurally unable to name anyone, so it
# means "each sample, independently, in its own copy of this frame". That
# is what the old `:default:` sample key was expressing -- a key that stood
# for "no particular sample" and had to be checked for at every consumer.
# It is now the absence of the slot, so there is nothing to check.
#
# Which kind a frame is gets decided by the first recorded transform: with
# `samples =` it is combined, without it, per-sample. See `.space_record()`.
#
# The steps stay plain lists, for the reasons given at the top of
# `R/classes-view.R` (decisions Q7 and Q8). `args` is whitelisted to
# serializable types at record time so the recipe survives `saveRDS` and
# reaches a parallel worker.
#
# Storage on gobject:
#   `gobject@spaces` — named list of handles, one per frame name.
#
# The `:default:` name now names a FRAME rather than a sample: an
# always-present zero-step `perSampleSpace` standing for the native frame.
# It is resolvable on every object without being recorded on any, which
# lets a consumer take `space` unconditionally instead of branching on
# `is.null(space)`. Recording onto it is refused -- it means "the frame the
# data is already in", and a transform on that is a contradiction.
#
# See `R/classes-view.R` for the subset/narrowing recipe.
# See `R/methods-space.R` for the recorder and the accessors.
# =============================================================================


# Name of the always-present native frame. Not a sample key -- see above.
.space_default_name <- ":default:"

#' Is this `space =` argument the frame the data is already in?
#'
#' `NULL` and `":default:"` are the same request spelled two ways, and an
#' artifact generator must treat them identically or the two spellings
#' would write to different names. One predicate so no consumer decides
#' that separately.
#' @noRd
.is_native_space <- function(space) {
    if (is.null(space)) return(TRUE)
    if (inherits(space, "giottoSpace")) {
        return(identical(space@name, .space_default_name))
    }
    identical(space, .space_default_name)
}

# Transform generics a space step may defer to. A space step is executed
# by `do.call(op, ...)`, so this list is also the guard against an
# arbitrary function name reaching a recorded recipe.
.space_ops <- c("spin", "spatShift", "affine", "flip", "rescale", "shear",
    "zoom")


# space step constructor + validator ####

#' Construct a transform step.
#'
#' `args` are whitelisted to serializable types — see
#' `.assert_space_args_serializable()`. `spin` / `affine` get their
#' rotation origin defaulted here so the recorded step is a complete
#' description of the operation rather than one that depends on the
#' receiving object's extent at resolve time.
#' @noRd
.space_step_transform <- function(op, args = list()) {
    checkmate::assert_string(op, .var.name = "op")
    if (!op %in% .space_ops) {
        stop("[space step] unknown transform '", op, "'. Known: ",
            paste(.space_ops, collapse = ", "), call. = FALSE)
    }
    checkmate::assert_list(args, .var.name = "args")
    if (op %in% c("spin", "affine") && length(args) > 0L) {
        if (is.null(args[["x0"]])) args$x0 <- 0
        if (is.null(args[["y0"]])) args$y0 <- 0
    }
    .assert_space_args_serializable(op, args)
    .validate_space_step(list(type = "transform", op = op, args = args))
}

#' Whitelist transform arguments to types that survive serialization.
#'
#' The transform generics take numeric / character / logical scalars and
#' vectors, matrices (an affine matrix), and `affine2d` objects. Anything
#' else — most importantly a terra object, which is a C++ pointer — would
#' make the recipe unserializable, which is the whole thing Q7 fixes. Fail
#' at record time, where the call site is still in scope.
#' @noRd
.assert_space_args_serializable <- function(op, args) {
    ok_one <- function(a) {
        if (is.null(a)) return(TRUE)
        if (inherits(a, "affine2d")) return(TRUE)
        if (is.matrix(a) && is.numeric(a)) return(TRUE)
        is.atomic(a) && (is.numeric(a) || is.character(a) || is.logical(a))
    }
    bad <- !vapply(args, ok_one, logical(1L))
    if (any(bad)) {
        nms <- names(args)[bad]
        nms[!nzchar(nms) | is.na(nms)] <- "<unnamed>"
        stop(sprintf(paste0(
            "[space step] %s() argument(s) %s cannot be recorded: got %s. ",
            "A recipe must survive serialization, so transform arguments ",
            "are limited to atomic vectors, numeric matrices, and affine2d ",
            "objects. Convert a terra object to a matrix or affine2d ",
            "first."),
            op, paste(sprintf("`%s`", nms), collapse = ", "),
            paste(vapply(args[bad], function(a) class(a)[[1L]],
                character(1L)), collapse = ", ")), call. = FALSE)
    }
    invisible(TRUE)
}

#' Validate one space step, whatever produced it.
#' @noRd
.validate_space_step <- function(step) {
    if (!is.list(step) || is.null(step$type)) {
        stop("[space step] a step must be a list with a `type` element",
            call. = FALSE)
    }
    if (!identical(step$type, "transform")) {
        stop("[space step] unknown type '", step$type,
            "'. Known: transform", call. = FALSE)
    }
    checkmate::assert_string(step$op, .var.name = "step$op")
    if (!step$op %in% .space_ops) {
        stop("[space step] unknown transform '", step$op, "'. Known: ",
            paste(.space_ops, collapse = ", "), call. = FALSE)
    }
    checkmate::assert_list(step$args, .var.name = "step$args")
    step
}


# space recipe ####

#' @title Class for coordinate-frame recipes
#' @name giottoSpace-class
#' @description
#' A `giottoSpace` is a handle over one named coordinate frame: a deferred
#' set of spatial transforms that consumer functions opt into with
#' `space = "<name>"`. It is virtual, and the subclass says which samples
#' participate and whether they interact:
#'
#' * [combinedSpace-class] — named samples sharing one frame. It declares
#'   its membership, so it can size a cross-sample job.
#' * [perSampleSpace-class] — one frame applied to each sample
#'   independently. It has no sample keys at all, which is what makes it
#'   structurally unable to express membership.
#'
#' Access it with `[` (class-preserving, so the result stays editable) and
#' `[[` (extracts the frame body, or one sample's step list). Append to it
#' with the transform verbs -- [spin()], [spatShift()], [affine()],
#' [flip()], [rescale()], [shear()], [zoom()] -- or compose two of the same
#' kind with `+`. Export the plain nested form with [as.list()].
#'
#' @slot name `character(1)`. The frame's name, or `NA_character_` for a
#'   handle not yet slotted under one.
#' @returns a `giottoSpace` object
#' @seealso [giottoSpace()] for the gobject-level accessors;
#'   [spaceSamples()] for the membership a frame declares;
#'   [giottoView-class] for the subset/narrowing recipe
#' @examples
#' g <- spatShift(giotto(), dx = 10, space = "shifted")
#' sp <- giottoSpace(g, "shifted")
#' sp[["shifted"]]
#' as.list(sp)
#' @exportClass giottoSpace
setClass("giottoSpace",
    representation("VIRTUAL", name = "character"),
    prototype = prototype(name = NA_character_)
)

#' @title Class for a frame shared by named samples
#' @name combinedSpace-class
#' @description
#' A frame that several samples are laid out in together — the cross-sample
#' case. `@samples` names its members, so a job built in this frame has a
#' declared size (see `adr/0006`).
#'
#' @slot samples `list` sample name -> `list` of steps. Each step is a
#'   tagged plain list -- `list(type = "transform", op = , args = )` --
#'   carrying no closure and no external pointer, so a recipe survives
#'   `saveRDS()` and reaches a parallel worker.
#' @returns a `combinedSpace` object
#' @seealso [giottoSpace-class]
#' @exportClass combinedSpace
setClass("combinedSpace",
    contains = "giottoSpace",
    representation(samples = "list"),
    prototype = prototype(samples = list())
)

#' @title Class for a frame applied to each sample independently
#' @name perSampleSpace-class
#' @description
#' A frame with no sample identity: the same steps applied to every sample
#' in its own copy of the frame, which never interact. This is what a
#' single [giotto-class] object always records, and what the always-present
#' `":default:"` native frame is.
#'
#' `@steps` is a flat ordered list with no keys. That is deliberate rather
#' than incidental — it is what makes this class unable to declare
#' membership, so [spaceSamples()] answers `NA_character_` and nothing
#' downstream can mistake it for a sized job.
#'
#' @slot steps `list` of steps, in application order. Each step is a tagged
#'   plain list -- `list(type = "transform", op = , args = )`.
#' @returns a `perSampleSpace` object
#' @seealso [giottoSpace-class]
#' @exportClass perSampleSpace
setClass("perSampleSpace",
    contains = "giottoSpace",
    representation(steps = "list"),
    prototype = prototype(steps = list())
)

#' Construct a frame of each kind.
#'
#' The single place a space's shape is written down. A fresh frame is
#' per-sample: it has recorded nothing, so it has named no members, and
#' `.space_record()` promotes it to a `combinedSpace` on the first
#' `samples =`.
#' @noRd
.new_per_sample_space <- function(name = NA_character_, steps = list()) {
    new("perSampleSpace", name = name, steps = steps)
}

#' @noRd
.new_combined_space <- function(name = NA_character_, samples = list()) {
    new("combinedSpace", name = name, samples = samples)
}

#' Is a frame body a flat step list rather than a sample -> steps map?
#'
#' The two export forms are told apart by shape, not by a tag: a step is
#' always a list carrying `$type`, and a sample entry never is (it is
#' itself a list of steps). An empty body has neither, and reads as
#' per-sample -- the undecided state, which is also what a fresh frame is.
#' @noRd
.space_body_is_steps <- function(body) {
    if (length(body) == 0L) return(TRUE)
    all(vapply(body, function(s) is.list(s) && !is.null(s$type),
        logical(1L)))
}

#' Coerce whatever a caller supplied into a `giottoSpace`.
#'
#' Accepts the `as.list()` export form -- `list("<frame>" = <body>)`, where
#' the body is either a flat step list or a sample -> steps map -- so the
#' round-trip is lossless and the kind is recovered from the shape.
#' @noRd
.as_giotto_space <- function(space, name = NULL, .var.name = "space") {
    if (inherits(space, "giottoSpace")) return(space)
    if (!is.list(space)) {
        stop("[space] `", .var.name, "` must be a giottoSpace or a list ",
            "(got '", class(space)[[1L]], "')", call. = FALSE)
    }
    nm <- name %null% NA_character_
    if (length(space) == 0L) return(.new_per_sample_space(nm))
    # A handle holds exactly one frame, so an export form carrying several
    # has no single name to take; slotting it would put a frame under a
    # name that is not its own.
    if (length(space) > 1L) {
        stop("[space] `", .var.name, "` holds ", length(space), " spaces (",
            paste(names(space), collapse = ", "),
            "); slot one at a time, e.g. `", .var.name, "[\"",
            names(space)[[1L]] %null% "<name>", "\"]`.", call. = FALSE)
    }
    if (!is.null(names(space)) && nzchar(names(space)[[1L]])) {
        nm <- names(space)[[1L]]
    }
    body <- space[[1L]]
    if (!is.list(body)) {
        stop("[space] `", .var.name, "` space body must be a list (got '",
            class(body)[[1L]], "')", call. = FALSE)
    }
    if (.space_body_is_steps(body)) {
        return(.new_per_sample_space(nm, body))
    }
    .new_combined_space(nm, body)
}

#' Validate one frame's sample -> steps mapping.
#' @noRd
.validate_space_samples <- function(samples, .var.name = "samples") {
    checkmate::assert_list(samples, .var.name = .var.name)
    if (length(samples) == 0L) return(samples)
    nms <- names(samples)
    if (is.null(nms) || any(is.na(nms)) || any(!nzchar(nms))) {
        stop("[space] `", .var.name, "` must be a named list ",
            "(sample name -> step list)", call. = FALSE)
    }
    for (steps in samples) {
        checkmate::assert_list(steps,
            .var.name = paste0(.var.name, "[[i]]"))
        lapply(steps, .validate_space_step)
    }
    samples
}

#' Validate a whole space, whatever produced it.
#'
#' Shared by `setValidity()` and by the recorder, which runs it while the
#' user's call site is still in scope for the error message.
#' @noRd
.validate_space <- function(space, .var.name = "space") {
    space <- .as_giotto_space(space, .var.name = .var.name)
    checkmate::assert_character(space@name, len = 1L,
        .var.name = paste0(.var.name, "@name"))
    if (inherits(space, "combinedSpace")) {
        .validate_space_samples(space@samples,
            .var.name = paste0(.var.name, "@samples"))
    } else {
        checkmate::assert_list(space@steps,
            .var.name = paste0(.var.name, "@steps"))
        lapply(space@steps, .validate_space_step)
    }
    space
}

# One validity method on the virtual class, inherited by both subclasses.
setValidity("giottoSpace", function(object) {
    err <- tryCatch({
        .validate_space(object, .var.name = "object")
        NULL
    }, error = function(e) conditionMessage(e))
    err %null% TRUE
})
