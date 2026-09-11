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
#' `[` selects and scopes frames, returning a [giottoSpace-class] so the
#' result is still appendable; `[[` extracts the plain form.
#'
#' * `sp[i]` — a `giottoSpace` holding frame(s) `i`, samples untouched
#' * `sp[i, j]` — a `giottoSpace` scoped to sample `j`
#' * `sp[[i]]` — frame `i` as a plain `list`, sample name -> steps
#' * `sp[[i, j]]` — the ordered step `list` for sample `j` of frame `i`
#' * `length(sp)` / `names(sp)` — frame count / frame names
#' * `as.list(sp)` — the plain nested form
#' * `sp1 + sp2` — merge frames, concatenating steps on collision
#'
#' @section Sample resolution:
#' `j` is resolved here and nowhere else, so no consumer re-implements the
#' rule:
#'
#' * a named sample matches exactly, else falls back to the `:default:`
#'   sentinel, else auto-vivifies as an empty step list -- so
#'   `spin(sp["atlas", "new_sample"], 30)` works on a sample that does not
#'   exist yet.
#' * `NA_character_` means "no sample identity" (a plain [giotto-class],
#'   which holds one sample): the `:default:` sentinel, else the sole key
#'   if there is exactly one, else empty. Two keys and no sentinel does not
#'   guess.
#'
#' @param x a `giottoSpace`
#' @param i frame selector — `character` or `numeric`
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

# Which sample key of `samples` answers for `k`? `NULL` if none does.
#' @noRd
.space_pick_sample <- function(samples, k) {
    keys <- names(samples)
    if (length(keys) == 0L) return(NULL)
    if (!is.na(k)) {
        if (k %in% keys) return(k)
        if (.space_default_sample %in% keys) return(.space_default_sample)
        return(NULL)
    }
    if (.space_default_sample %in% keys) return(.space_default_sample)
    if (length(keys) == 1L) return(keys[[1L]])
    NULL
}

# Frame selector -> the names it picks. Unknown frames error rather than
# materializing as NULL entries.
#' @noRd
.space_frame_keys <- function(x, i) {
    keys <- names(x@spaces)
    if (is.character(i)) {
        bad <- setdiff(i, keys)
        if (length(bad) > 0L) {
            stop("[space] no frame named ",
                paste(sprintf("'%s'", bad), collapse = ", "),
                ". Available: ",
                if (length(keys) == 0L) "<none>"
                else paste(keys, collapse = ", "),
                call. = FALSE)
        }
        return(i)
    }
    keys[i]
}

#' @rdname giottoSpace-access
#' @export
setMethod("[", signature(x = "giottoSpace"), function(x, i, j, ...) {
    if (!missing(i)) x@spaces <- x@spaces[.space_frame_keys(x, i)]
    if (missing(j)) return(x)
    checkmate::assert_character(j, len = 1L, .var.name = "j")
    x@spaces <- lapply(x@spaces, function(samples) {
        pick <- .space_pick_sample(samples, j)
        key <- if (is.na(j)) (pick %null% .space_default_sample) else j
        steps <- if (is.null(pick)) list() else samples[[pick]]
        stats::setNames(list(steps), key)
    })
    x
})

#' @rdname giottoSpace-access
#' @export
setMethod("[[", signature(x = "giottoSpace"), function(x, i, j, ...) {
    frame <- x@spaces[[.space_frame_keys(x, i)]]
    if (missing(j)) return(frame)
    checkmate::assert_character(j, len = 1L, .var.name = "j")
    pick <- .space_pick_sample(frame, j)
    if (is.null(pick)) list() else frame[[pick]]
})

#' @rdname giottoSpace-access
#' @export
setMethod("length", signature(x = "giottoSpace"),
    function(x) length(x@spaces))

#' @rdname giottoSpace-access
#' @export
setMethod("names", signature(x = "giottoSpace"),
    function(x) names(x@spaces) %null% character())

#' @rdname giottoSpace-access
#' @export
setMethod("as.list", signature(x = "giottoSpace"),
    function(x, ...) x@spaces)

#' @rdname giottoSpace-access
#' @export
setMethod("+", signature(e1 = "giottoSpace", e2 = "giottoSpace"),
    function(e1, e2) {
        out <- e1
        for (nm in names(e2@spaces)) {
            add <- e2@spaces[[nm]]
            cur <- out@spaces[[nm]]
            if (is.null(cur)) {
                out@spaces[[nm]] <- add
                next
            }
            for (samp in names(add)) {
                cur[[samp]] <- c(cur[[samp]], add[[samp]])
            }
            out@spaces[[nm]] <- cur
        }
        out
    }
)

#' @rdname giottoSpace-access
#' @export
setMethod("show", signature(object = "giottoSpace"), function(object) {
    cat("An object of class giottoSpace\n")
    for (nm in names(object@spaces)) {
        cat(sprintf("frame '%s'\n", nm))
        samples <- object@spaces[[nm]]
        for (samp in names(samples)) {
            ops <- vapply(samples[[samp]], function(s) s$op, character(1L))
            cat(sprintf("  %s : %s\n", samp,
                if (length(ops) == 0L) "<no steps>"
                else paste(ops, collapse = " -> ")))
        }
    }
    invisible(NULL)
})
