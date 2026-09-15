#' @include classes-view.R
#' @include classes-space.R
#' @include generics.R
NULL

# =============================================================================
# methods-recipe.R — the access / append / export surface for the two
# recipe classes.
#
# Three things a handle has to cover, and one place each:
#
#   access   `[` (class-preserving, so the result stays editable) and `[[`
#            (extracts the plain form). Plus `length()`, `names()`, `show()`.
#   append   the builder verbs, which live with their generics
#            (methods-view.R, methods-space.R), and `+` here.
#   export   `as.list()`, the coercion back to the plain nested form the
#            `giotto` slots and any external consumer accept.
#
# Nothing else needs `@`: a consumer that wants steps asks `[[` for them,
# and a step carries everything needed to evaluate it -- a crop's frame
# included.
# =============================================================================


# giottoView ####

#' @title Access a view recipe
#' @name giottoView-access
#' @description
#' `[` selects steps and returns a [giottoView-class], so the result is
#' still appendable. `[[` extracts one step as the plain tagged list it is
#' recorded as; `[[<-` replaces or (with `NULL`) drops one.
#'
#' * `v[i]` — a `giottoView` holding steps `i`. Negative indices drop.
#' * `v[[i]]` — step `i`, a plain `list`
#' * `v[i, j]` — the value of attribute `j` on step `i`
#' * `length(v)` / `names(v)` — step count / step types, in recorded order
#' * `as.list(v)` — the plain nested form
#' * `v1 + v2` — concatenate steps. Unconditional: each crop step carries
#'   its own frame, so there is nothing to reconcile.
#'
#' @param x a `giottoView`
#' @param i step selector — `numeric` or `logical`
#' @param j `character(1)`. Step attribute to read.
#' @param value replacement step, or `NULL` to drop
#' @param e1,e2 `giottoView` objects to compose
#' @param ... additional arguments, currently unused
#' @returns a `giottoView` for `[` and `+`; a `list` for `[[` and
#'   `as.list()`; the attribute value for `v[i, j]`
#' @examples
#' g <- crop(giotto(), c(0, 10, 0, 10), view = "v")
#' g <- crop(g, c(0, 5, 0, 5), relation = "within", view = "v")
#' v <- giottoView(g, "v")
#'
#' length(v)
#' names(v)
#' v[[2L]]
#' v[2L, "relation"]
#' v[-1L]
NULL

#' @rdname giottoView-access
#' @export
setMethod("[", signature(x = "giottoView"), function(x, i, j, ...) {
    if (!missing(j)) {
        checkmate::assert_character(j, len = 1L, any.missing = FALSE,
            .var.name = "j")
        if (missing(i)) i <- seq_along(x@steps)
        vals <- lapply(x@steps[i], function(s) s[[j]])
        return(if (length(vals) == 1L) vals[[1L]] else vals)
    }
    if (missing(i)) return(x)
    x@steps <- x@steps[i]
    x
})

#' @rdname giottoView-access
#' @export
setMethod("[[", signature(x = "giottoView"), function(x, i, ...) {
    x@steps[[i]]
})

#' @rdname giottoView-access
#' @export
setMethod("[[<-", signature(x = "giottoView"),
    function(x, i, ..., value) {
        if (!is.null(value)) value <- .validate_view_step(value)
        x@steps[[i]] <- value
        x
    }
)

#' @rdname giottoView-access
#' @export
setMethod("length", signature(x = "giottoView"),
    function(x) length(x@steps))

#' @rdname giottoView-access
#' @export
setMethod("names", signature(x = "giottoView"),
    function(x) vapply(x@steps, function(s) s$type, character(1L)))

#' @rdname giottoView-access
#' @export
setMethod("as.list", signature(x = "giottoView"),
    function(x, ...) list(steps = x@steps))

#' @rdname giottoView-access
#' @export
setMethod("+", signature(e1 = "giottoView", e2 = "giottoView"),
    function(e1, e2) {
        # No reconciliation: a crop step names the frame its own region was
        # read in, so concatenating cannot reinterpret either side.
        e1@steps <- c(e1@steps, e2@steps)
        e1
    }
)

#' @rdname giottoView-access
#' @export
setMethod("show", signature(object = "giottoView"), function(object) {
    cat("An object of class giottoView\n")
    cat("steps :", length(object@steps), "\n")
    for (i in seq_along(object@steps)) {
        cat(sprintf("  [%d] %s\n", i, .view_step_line(object@steps[[i]])))
    }
    invisible(NULL)
})

# One-line summary of a step, for `show()`.
#' @noRd
.view_step_line <- function(step) {
    switch(step$type,
        filter = sprintf("filter  %s", step$predicate),
        crop = sprintf("crop    %s on %s%s", step$relation, step$geom,
            if (is.na(step$space)) "" else sprintf(" [%s]", step$space)),
        samples = sprintf("samples %s",
            paste(step$samples, collapse = ", ")),
        step$type
    )
}


# giottoSpace ####

#' @title Access a coordinate-frame recipe
#' @name giottoSpace-access
#' @description
#' `[` scopes a frame to a sample, returning a [giottoSpace-class] so the
#' result is still appendable; `[[` extracts the plain form.
#'
#' A handle holds exactly one frame, so `i` names or indexes that frame and
#' is checked rather than used to select — `sp["atlas", "a"]` and
#' `sp[[1L, "a"]]` both read as "in this frame, for sample a".
#'
#' * `sp[i, j]` — the frame scoped to sample `j`
#' * `sp[[i]]` — the frame body: a sample -> steps `list` for a
#'   [combinedSpace-class], a flat step `list` for a
#'   [perSampleSpace-class]
#' * `sp[[i, j]]` — the ordered step `list` that applies to sample `j`
#' * `length(sp)` / `names(sp)` — always `1`, and the frame name
#' * `as.list(sp)` — the plain nested form, `list("<frame>" = <body>)`
#' * `sp1 + sp2` — merge two handles on the same frame, concatenating steps
#'
#' @section Sample resolution:
#' `j` is resolved here and nowhere else, so no consumer re-implements the
#' rule. What it resolves to depends on the kind, and that is the point of
#' the split:
#'
#' * a [perSampleSpace-class] ignores `j` entirely. It has no sample keys,
#'   so every sample gets the same steps and `sp[, j]` is the identity.
#' * a [combinedSpace-class] matches `j` against its members exactly, and
#'   auto-vivifies an empty step list otherwise -- so
#'   `spin(sp["atlas", "new_sample"], 30)` works on a member that does not
#'   exist yet.
#' * `NA_character_` means "no sample identity" (a plain [giotto-class],
#'   which holds one sample). On a `combinedSpace` it takes the sole member
#'   if there is exactly one, else empty; two members and no name does not
#'   guess.
#'
#' @param x a `giottoSpace`
#' @param i frame selector — this handle's name, or `1`
#' @param j `character(1)`. Sample name, or `NA_character_` for none.
#' @param e1,e2 `giottoSpace` objects to compose
#' @param ... additional arguments, currently unused
#' @returns a `giottoSpace` for `[` and `+`; a `list` for `[[` and
#'   `as.list()`
#' @examples
#' g <- spatShift(giotto(), dx = 10, space = "shifted")
#' sp <- giottoSpace(g, "shifted")
#'
#' names(sp)
#' sp[["shifted"]]
#' sp[["shifted", NA_character_]]
NULL

# Which member key of `samples` answers for `k`? `NULL` if none does.
#
# combinedSpace only -- a perSampleSpace has no keys to pick from, which is
# why the `:default:` fallback this used to carry is gone: the case it
# covered is now a different class rather than a magic key.
#' @noRd
.space_pick_sample <- function(samples, k) {
    keys <- names(samples)
    if (length(keys) == 0L) return(NULL)
    if (!is.na(k)) return(if (k %in% keys) k else NULL)
    if (length(keys) == 1L) return(keys[[1L]])
    NULL
}

# `i` identifies the one frame a handle holds. It is checked, not used to
# select: a wrong name is a caller who thinks this handle holds a frame it
# does not, and silently answering with the frame it does hold would return
# the wrong transforms.
#' @noRd
.space_check_frame <- function(x, i) {
    nm <- x@name
    ok <- if (is.character(i)) {
        length(i) == 1L && !is.na(nm) && identical(i, nm)
    } else {
        length(i) == 1L && !is.na(i) && i == 1L
    }
    if (!ok) {
        stop("[space] no space named ",
            paste(sprintf("'%s'", i), collapse = ", "),
            ". This handle holds ",
            if (is.na(nm)) "an unnamed space"
            else sprintf("'%s'", nm), ".", call. = FALSE)
    }
    invisible(nm)
}

# The frame name a merge result carries. An unnamed handle -- one built
# directly rather than read off a gobject -- takes the other's name.
#' @noRd
.space_merged_name <- function(e1, e2) {
    n1 <- e1@name
    n2 <- e2@name
    if (is.na(n1)) return(n2)
    if (is.na(n2) || identical(n1, n2)) return(n1)
    stop("[space] cannot compose spaces '", n1, "' and '", n2,
        "': a handle holds one space, and `+` merges two views of the ",
        "same one.", call. = FALSE)
}

#' @noRd
.space_ops_str <- function(steps) {
    if (length(steps) == 0L) return("<no steps>")
    paste(vapply(steps, function(s) s$op, character(1L)), collapse = " -> ")
}

#' @rdname giottoSpace-access
#' @export
setMethod("[", signature(x = "combinedSpace"), function(x, i, j, ...) {
    if (!missing(i)) .space_check_frame(x, i)
    if (missing(j)) return(x)
    checkmate::assert_character(j, len = 1L, .var.name = "j")
    pick <- .space_pick_sample(x@samples, j)
    x@samples <- if (is.na(j)) {
        if (is.null(pick)) list() else x@samples[pick]
    } else {
        # keyed under `j` even when nothing matched, so the result is a
        # handle an append can land on
        stats::setNames(list(if (is.null(pick)) list() else x@samples[[pick]]),
            j)
    }
    x
})

#' @rdname giottoSpace-access
#' @export
setMethod("[", signature(x = "perSampleSpace"), function(x, i, j, ...) {
    if (!missing(i)) .space_check_frame(x, i)
    # No sample identity to scope to: the same steps apply to every sample,
    # in its own copy of the frame. This identity is what lets a caller
    # scope unconditionally -- `space[, samp]` -- without knowing the kind.
    x
})

#' @rdname giottoSpace-access
#' @export
setMethod("[[", signature(x = "combinedSpace"), function(x, i, j, ...) {
    .space_check_frame(x, i)
    if (missing(j)) return(x@samples)
    checkmate::assert_character(j, len = 1L, .var.name = "j")
    pick <- .space_pick_sample(x@samples, j)
    if (is.null(pick)) list() else x@samples[[pick]]
})

#' @rdname giottoSpace-access
#' @export
setMethod("[[", signature(x = "perSampleSpace"), function(x, i, j, ...) {
    .space_check_frame(x, i)
    x@steps
})

#' @rdname giottoSpace-access
#' @export
setMethod("length", signature(x = "giottoSpace"), function(x) 1L)

#' @rdname giottoSpace-access
#' @export
setMethod("names", signature(x = "giottoSpace"), function(x) x@name)

#' @rdname giottoSpace-access
#' @export
setMethod("as.list", signature(x = "combinedSpace"),
    function(x, ...) stats::setNames(list(x@samples), x@name))

#' @rdname giottoSpace-access
#' @export
setMethod("as.list", signature(x = "perSampleSpace"),
    function(x, ...) stats::setNames(list(x@steps), x@name))

#' @rdname giottoSpace-access
#' @export
setMethod("+", signature(e1 = "combinedSpace", e2 = "combinedSpace"),
    function(e1, e2) {
        e1@name <- .space_merged_name(e1, e2)
        for (samp in names(e2@samples)) {
            e1@samples[[samp]] <- c(e1@samples[[samp]], e2@samples[[samp]])
        }
        e1
    }
)

#' @rdname giottoSpace-access
#' @export
setMethod("+", signature(e1 = "perSampleSpace", e2 = "perSampleSpace"),
    function(e1, e2) {
        e1@name <- .space_merged_name(e1, e2)
        e1@steps <- c(e1@steps, e2@steps)
        e1
    }
)

# The kinds answer different questions -- one names members, the other
# declines to -- so there is no merge that preserves both. Refusing is what
# keeps `spaceSamples()` meaningful on the result.
#' @rdname giottoSpace-access
#' @export
setMethod("+", signature(e1 = "giottoSpace", e2 = "giottoSpace"),
    function(e1, e2) {
        stop("[space] cannot compose a ", class(e1)[[1L]], " with a ",
            class(e2)[[1L]], ": one declares which samples share the space ",
            "and the other applies to every sample independently, so the ",
            "merge has no membership. Slot them under separate names.",
            call. = FALSE)
    }
)

#' @rdname giottoSpace-access
#' @export
setMethod("show", signature(object = "combinedSpace"), function(object) {
    cat("An object of class combinedSpace\n")
    cat(sprintf("space '%s' | %d sample(s) share it\n", object@name,
        length(object@samples)))
    for (samp in names(object@samples)) {
        cat(sprintf("  %s : %s\n", samp,
            .space_ops_str(object@samples[[samp]])))
    }
    invisible(NULL)
})

#' @rdname giottoSpace-access
#' @export
setMethod("show", signature(object = "perSampleSpace"), function(object) {
    cat("An object of class perSampleSpace\n")
    cat(sprintf("space '%s' | applies to each sample independently\n",
        object@name))
    cat(sprintf("  %s\n", .space_ops_str(object@steps)))
    invisible(NULL)
})


#' @title Which samples a coordinate frame spans
#' @name spaceSamples
#' @description
#' The membership a frame declares — the thing an artifact generator reads
#' to size its job (`adr/0006`).
#'
#' * [combinedSpace-class] — the member names, in recording order.
#' * [perSampleSpace-class] — `NA_character_`, meaning "no sample identity":
#'   the frame applies to each sample independently, so the job size comes
#'   from the object rather than from the frame.
#'
#' `NA_character_` rather than `character(0)`: an empty vector reads as
#' "nobody participates", which is a real and different answer (a
#' `combinedSpace` that has been emptied). A caller that branches on
#' `length()` alone would run zero jobs for the frame that should run one
#' per sample.
#'
#' @param x a [giottoSpace-class]
#' @param ... additional arguments, currently unused
#' @returns `character` of sample names, or `NA_character_`
#' @examples
#' g <- spatShift(giotto(), dx = 10, space = "shifted")
#' spaceSamples(giottoSpace(g, "shifted"))
#' @export
setGeneric("spaceSamples", function(x, ...) standardGeneric("spaceSamples"))

#' @rdname spaceSamples
#' @export
setMethod("spaceSamples", signature(x = "combinedSpace"),
    function(x, ...) names(x@samples) %null% character())

#' @rdname spaceSamples
#' @export
setMethod("spaceSamples", signature(x = "perSampleSpace"),
    function(x, ...) NA_character_)
