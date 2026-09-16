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
#   combinedSpace   -- membership derived from @steps
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
#'
#' `samples` is the step's own scope: a character vector of sample names,
#' or `NULL` to broadcast. Scope is a property OF THE STEP rather than of
#' the list it sits in, which is what lets one ordered list replay
#' correctly for every sample. Keying by sample instead -- one step list
#' per name -- cannot express
#'
#'   spin(everyone) -> shift(a only) -> spin(everyone)
#'
#' for a sample first mentioned at step 2: a broadcast appended to "every
#' key so far" never reaches a key that appears later, so the same recipe
#' would replay differently depending on the order samples were named.
#' @noRd
.space_step_transform <- function(op, args = list(), samples = NULL) {
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
    if (!is.null(samples)) {
        checkmate::assert_character(samples, min.len = 1L,
            any.missing = FALSE, .var.name = "samples")
        samples <- unique(samples)
    }
    .validate_space_step(list(type = "transform", op = op, args = args,
        samples = samples))
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

#' Construct a membership step.
#'
#' A step that does nothing but put samples in the space. It exists
#' because membership derived from transforms cannot represent a member
#' with NO transform -- the sample sitting at a layout's origin, which is
#' in the layout precisely by not moving.
#'
#' Having it means membership is a pure function of the step list rather
#' than a second thing stored beside it. There is no slot left to disagree
#' with the steps, so the "step scopes to a non-member" check this file
#' used to carry is now unreachable by construction rather than policed.
#' @noRd
.space_step_member <- function(samples) {
    checkmate::assert_character(samples, min.len = 1L, any.missing = FALSE,
        .var.name = "samples")
    .validate_space_step(list(type = "member", samples = unique(samples)))
}

#' Validate one space step, whatever produced it.
#' @noRd
.validate_space_step <- function(step) {
    if (!is.list(step) || is.null(step$type)) {
        stop("[space step] a step must be a list with a `type` element",
            call. = FALSE)
    }
    if (!step$type %in% c("transform", "member")) {
        stop("[space step] unknown type '", step$type,
            "'. Known: transform, member", call. = FALSE)
    }
    if (identical(step$type, "member")) {
        checkmate::assert_character(step$samples, min.len = 1L,
            any.missing = FALSE, .var.name = "step$samples")
        return(step)
    }
    checkmate::assert_string(step$op, .var.name = "step$op")
    if (!step$op %in% .space_ops) {
        stop("[space step] unknown transform '", step$op, "'. Known: ",
            paste(.space_ops, collapse = ", "), call. = FALSE)
    }
    checkmate::assert_list(step$args, .var.name = "step$args")
    if (!is.null(step$samples)) {
        checkmate::assert_character(step$samples, min.len = 1L,
            any.missing = FALSE, .var.name = "step$samples")
    }
    step
}

#' Which recorded steps APPLY to sample `j`?
#'
#' THE resolution rule, for both kinds. A step with no scope broadcasts; a
#' scoped step applies only to the samples it names. `NA_character_` means
#' "no sample identity" -- a plain `giotto`, which holds one sample -- and
#' takes the broadcast steps, since there is no name for a scoped step to
#' have matched.
#'
#' Membership steps are dropped: they declare, they do not transform, and
#' the caller of this is about to `do.call()` what it gets back.
#' @noRd
.space_steps_for <- function(steps, j = NA_character_) {
    keep <- vapply(steps, function(s) {
        identical(s$type, "transform") &&
            (is.null(s$samples) || (!is.na(j) && j %in% s$samples))
    }, logical(1L))
    steps[keep]
}

#' Narrow a recipe to sample `j`, keeping it a recipe.
#'
#' Unlike `.space_steps_for()`, which extracts the steps to APPLY, this
#' rewrites the recipe so it still says who each step is for -- a step
#' scoped to `c("a", "b")` narrowed to `"a"` becomes scoped to `"a"`, not
#' unscoped. Erasing the scope instead would make the result a list of
#' steps that apply to everyone, so merging two narrowings would hand each
#' sample the other's transforms.
#'
#' An unscoped step gains `j`, because in a recipe that is only about `j`
#' that is what "applies to everyone" means. That is what makes
#' `sp[, "a"] + sp[, "b"]` reconstruct the original rather than double the
#' broadcast steps.
#'
#' `NA_character_` is "no sample identity", which nothing can be scoped
#' to: the scoped steps drop and the unscoped ones stay unscoped.
#'
#' Member steps narrow by the same rule -- they are steps with a scope and
#' nothing else, so membership follows the recipe without a special case.
#' @noRd
.space_narrow_steps <- function(steps, j) {
    if (is.na(j)) {
        keep <- vapply(steps, function(s) is.null(s$samples), logical(1L))
        return(steps[keep])
    }
    out <- lapply(steps, function(s) {
        if (is.null(s$samples)) {
            s$samples <- j
            return(s)
        }
        if (!j %in% s$samples) return(NULL)
        s$samples <- j
        s
    })
    out[!vapply(out, is.null, logical(1L))]
}

#' Every sample name any step mentions, in recorded order.
#'
#' This IS the membership of a `combinedSpace` -- derived, not stored, so
#' naming a sample in a transform and declaring it outright are the same
#' act recorded two ways.
#' @noRd
.space_step_samples <- function(steps) {
    unique(unlist(lapply(steps, function(s) s$samples))) %null% character()
}


# space recipe ####

#' @title Class for coordinate-frame recipes
#' @name giottoSpace-class
#' @description
#' A `giottoSpace` is a handle over one named coordinate frame: an ordered
#' list of deferred spatial transforms that consumer functions opt into
#' with `space = "<name>"`. Every step carries its own `samples` scope, so
#' one list replays correctly for each sample.
#'
#' It is virtual. The subclass says whether the samples INTERACT, which is
#' the only thing a job needs from a space (`adr/0006`):
#'
#' * [combinedSpace-class] — the samples are laid out relative to one
#'   another in one coordinate system, so cross-sample distances mean
#'   something and a job over it is ONE job. It declares its membership.
#' * [perSampleSpace-class] — each sample sits in its own copy of the
#'   frame and they never touch, so a job over it is N independent jobs.
#'   Steps may still be scoped per sample: two sections independently
#'   rotated upright are per-sample, not combined.
#'
#' Access it with `[` (class-preserving, so the result stays editable) and
#' `[[` (extracts the steps that apply). Append to it with the transform
#' verbs -- [spin()], [spatShift()], [affine()], [flip()], [rescale()],
#' [shear()], [zoom()] -- or compose two of the same kind with `+`. Export
#' the plain form with [as.list()].
#'
#' @slot name `character(1)`. The frame's name, or `NA_character_` for a
#'   handle not yet slotted under one.
#' @slot steps `list` of steps, in application order. Each step is a tagged
#'   plain list -- `list(type = "transform", op = , args = , samples = )` --
#'   carrying no closure and no external pointer, so a recipe survives
#'   `saveRDS()` and reaches a parallel worker. `samples = NULL` broadcasts.
#' @returns a `giottoSpace` object
#' @seealso [giottoSpace()] for the gobject-level accessors;
#'   [giottoSpace-access] for `names()`, which reports membership;
#'   [giottoView-class] for the subset/narrowing recipe
#' @examples
#' g <- spatShift(giotto(), dx = 10, space = "shifted")
#' sp <- giottoSpace(g, "shifted")
#' sp[["shifted"]]
#' as.list(sp)
#' @exportClass giottoSpace
setClass("giottoSpace",
    representation("VIRTUAL", name = "character", steps = "list"),
    prototype = prototype(name = NA_character_, steps = list())
)

#' @title Class for a frame shared by named samples
#' @name combinedSpace-class
#' @description
#' A frame that several samples are laid out in together — the cross-sample
#' case. Its membership is the whole difference from a
#' [perSampleSpace-class]: it says these samples occupy ONE coordinate
#' system, so a job built here is one job spanning them rather than one per
#' sample (`adr/0006`).
#'
#' Membership is DERIVED from the steps — every sample any step names, in
#' recorded order, readable with `names()`. It is not a slot, so
#' there is nothing that can disagree with the recipe. A member that needs
#' no transform of its own is declared with a membership step, which
#' [combinedSpace()] seeds and `samples =` on any transform verb adds to.
#'
#' @returns a `combinedSpace` object
#' @seealso [giottoSpace-class], [combinedSpace()]
#' @exportClass combinedSpace
setClass("combinedSpace", contains = "giottoSpace")

#' @title Class for a frame applied to each sample independently
#' @name perSampleSpace-class
#' @description
#' A frame whose samples never interact: each sits in its own copy of it,
#' so a job over this frame is N independent jobs. Steps may still be
#' scoped -- two sections each rotated upright by a different angle are
#' per-sample, because nothing about that puts them in a shared coordinate
#' system.
#'
#' Its membership is OPEN: `names()` reports the samples its steps mention,
#' but an unscoped step also reaches samples that appear nowhere in the
#' recipe. Coverage is therefore whatever the object holds, which is why a
#' consumer sizing a job reads it from the object rather than asking the
#' space.
#'
#' It is also the only kind that cannot be created by `space = "<name>"` on
#' a transform verb — recording onto an unused name declares a combined
#' frame, on the assumption that laying samples out together is the common
#' reason to name one. Build this with [perSampleSpace()] and slot it in.
#'
#' @returns a `perSampleSpace` object
#' @seealso [giottoSpace-class], [perSampleSpace()]
#' @exportClass perSampleSpace
setClass("perSampleSpace", contains = "giottoSpace")

#' @title Build a coordinate frame directly
#' @name space-constructors
#' @description
#' Build an empty frame of a given kind, to slot in with
#' `giottoSpace(g, "name") <- `. Transforms are recorded onto it afterwards
#' with the usual verbs.
#'
#' `perSampleSpace()` exists because recording onto an unused name creates
#' a [combinedSpace-class] — laying samples out together being the common
#' reason to name a frame. A frame whose samples stay independent has to
#' say so, and this is where it says it.
#'
#' `combinedSpace()` seeds membership up front, which matters for a member
#' that needs no transform of its own: a sample at the layout's origin is
#' still in the layout, and nothing would otherwise record it.
#'
#' @param samples `character`. Member sample names.
#' @param name `character(1)`. Optional; `giottoSpace<-` sets it on slotting.
#' @returns a `combinedSpace` or `perSampleSpace`
#' @examples
#' mg <- giotto()
#' giottoSpace(mg, "upright") <- perSampleSpace()
#' giottoSpace(mg, "upright")
#' @export
combinedSpace <- function(samples = character(), name = NA_character_) {
    checkmate::assert_character(samples, any.missing = FALSE)
    steps <- if (length(samples) == 0L) list() else
        list(.space_step_member(samples))
    new("combinedSpace", name = name, steps = steps)
}

#' @rdname space-constructors
#' @export
perSampleSpace <- function(name = NA_character_) {
    new("perSampleSpace", name = name)
}

#' Construct a frame of each kind, internally.
#'
#' Recording onto an unused name always produces a `combinedSpace`; see
#' [perSampleSpace-class] for why the other kind is declaration-only.
#' @noRd
.new_per_sample_space <- function(name = NA_character_, steps = list()) {
    new("perSampleSpace", name = name, steps = steps)
}

#' @noRd
.new_combined_space <- function(name = NA_character_, steps = list()) {
    new("combinedSpace", name = name, steps = steps)
}

#' Coerce whatever a caller supplied into a `giottoSpace`.
#'
#' Accepts the `as.list()` export form, which names its kind rather than
#' leaving it to be inferred from shape -- the two kinds hold the same
#' slots now, so there is no shape to infer from.
#' @noRd
.as_giotto_space <- function(space, name = NULL, .var.name = "space") {
    if (inherits(space, "giottoSpace")) return(space)
    if (!is.list(space)) {
        stop("[space] `", .var.name, "` must be a giottoSpace or a list ",
            "(got '", class(space)[[1L]], "')", call. = FALSE)
    }
    nm <- name %null% NA_character_
    if (length(space) == 0L) return(.new_combined_space(nm))
    # A handle holds exactly one frame, so an export form carrying several
    # has no single name to take.
    if (length(space) > 1L) {
        stop("[space] `", .var.name, "` holds ", length(space), " spaces (",
            paste(names(space), collapse = ", "),
            "); slot one at a time.", call. = FALSE)
    }
    if (!is.null(names(space)) && nzchar(names(space)[[1L]])) {
        nm <- names(space)[[1L]]
    }
    body <- space[[1L]]
    if (!is.list(body) || is.null(body$kind)) {
        stop("[space] `", .var.name, "` frame body must be a list with a ",
            "`kind` element ('combined' or 'perSample'), as `as.list()` ",
            "writes it.", call. = FALSE)
    }
    steps <- body$steps %null% list()
    switch(body$kind,
        combined = .new_combined_space(nm, steps),
        perSample = .new_per_sample_space(nm, steps),
        stop("[space] unknown kind '", body$kind,
            "'. Known: combined, perSample", call. = FALSE))
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
    checkmate::assert_list(space@steps,
        .var.name = paste0(.var.name, "@steps"))
    lapply(space@steps, .validate_space_step)
    # No membership check: a combinedSpace derives its members from these
    # same steps, so a step cannot scope to a non-member.
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
