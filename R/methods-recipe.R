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
    function(x, ...) stats::setNames(list(list(steps = x@steps)), x@name))

#' @rdname giottoView-access
#' @export
setMethod("+", signature(e1 = "giottoView", e2 = "giottoView"),
    function(e1, e2) {
        # No reconciliation of STEPS: a crop step names the frame its own
        # region was read in, so concatenating cannot reinterpret either
        # side. The NAME does need reconciling -- inheriting `e1`'s would
        # place the composed view over the one it was built from.
        e1@name <- .merged_recipe_name(e1, e2)
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
#' A handle holds one space, so the index is the SAMPLE — `sp["a"]` and
#' `sp[["a"]]` both read as "this space, for sample a". `[` narrows and
#' returns a [giottoSpace-class], so the result is still a recipe and
#' stays appendable; `[[` extracts the plain step list.
#'
#' A numeric index is a step position instead, following `l[["a"]]` vs
#' `l[[1]]`: character selects by name, numeric by position.
#'
#' * `sp[i]` — character: narrowed to sample `i`, every surviving step
#'   rescoped to it rather than stripped, so `sp["a"] + sp["b"]`
#'   reconstructs the original. Numeric: the space holding steps `i`.
#' * `sp[[i]]` — character: the ordered step `list` that applies to sample
#'   `i`. Numeric: step `i`, raw.
#' * `names(sp)` — the samples this recipe mentions
#' * `length(sp)` — step count
#' * `as.list(sp)` — the plain export form
#' * `sp1 + sp2` — merge two handles on the same space, concatenating steps
#'
#' @section Sample resolution:
#' One rule, both kinds, resolved here and nowhere else: a step with no
#' scope applies to every sample; a scoped step applies only to the samples
#' it names.
#'
#' `NA_character_` means "no sample identity" — a plain [giotto-class],
#' which is one sample that has no name. If the whole recipe mentions
#' exactly one sample, that is who the handle is about and it resolves for
#' it, which is how `sp["a"][[NA_character_]]` reads its steps back without
#' being told the name twice. Two or more and it does not guess; none, and
#' the unscoped steps are the whole answer.
#'
#' `sp[NA_character_]` is an error: narrowing needs a name, and "narrow to
#' nobody" has no meaning.
#'
#' @section Membership:
#' `names()` says which samples the recipe MENTIONS. Whether that is the
#' whole story is the class's job, not the accessor's:
#'
#' * [combinedSpace-class] — closed. These are the members, and an unscoped
#'   step reaches exactly them.
#' * [perSampleSpace-class] — open. An unscoped step also reaches samples
#'   that appear nowhere in the recipe, so coverage is whatever the object
#'   holds. A consumer sizing a job must read it from the object, which is
#'   why nothing asks a `perSampleSpace` how many samples there are.
#'
#' @param x a `giottoSpace`
#' @param i `character(1)` sample name, or `numeric` step position
#' @param j not used — spaces are indexed on one axis
#' @param e1,e2 `giottoSpace` objects to compose
#' @param ... additional arguments, currently unused
#' @returns a `giottoSpace` for `[` and `+`; a `list` for `[[` and
#'   `as.list()`
#' @examples
#' g <- spatShift(giotto(), dx = 10, space = "shifted")
#' sp <- giottoSpace(g, "shifted")
#'
#' names(sp)
#' length(sp)
#' sp[[NA_character_]]
NULL

# Spaces index on one axis. A second index is almost certainly code
# written against the old frame-then-sample form, so say that rather than
# letting it pass silently as `...`.
#' @noRd
.space_assert_one_index <- function(j) {
    if (missing(j)) return(invisible(TRUE))
    stop("[space] a space is indexed on one axis -- the sample, or a ",
        "numeric step position. Use `sp[\"a\"]` / `sp[[\"a\"]]`, not ",
        "`sp[<space>, \"a\"]`.", call. = FALSE)
}

# The space name a merge result carries. An unnamed handle -- one built
# directly rather than read off a gobject -- takes the other's name.
#' @noRd
# A composed recipe inherits a name only when that name is unambiguous.
#
# Two differently-named operands describe two destinations, so the result has
# none: `+` says how to build a recipe, not where it belongs, and deriving a
# key from the operands would have `setGiotto()` write somewhere the user
# never named -- silently overwriting on a repeat, since the derivation is
# deterministic. NA is not a gap here, it is the honest answer, and the
# setters turn it into an error that names the way through.
#
# One named operand is different: extending a named recipe with an unnamed
# one is still that recipe.
.merged_recipe_name <- function(e1, e2) {
    n1 <- objName(e1)
    n2 <- objName(e2)
    if (is.na(n1)) return(n2)
    if (is.na(n2) || identical(n1, n2)) return(n1)
    NA_character_
}

#' @noRd
.space_ops_str <- function(steps) {
    if (length(steps) == 0L) return("<no steps>")
    paste(vapply(steps, function(s) {
        # a member step has no `op` -- it declares rather than transforms
        if (identical(s$type, "member")) {
            return(sprintf("member{%s}", paste(s$samples, collapse = ",")))
        }
        if (is.null(s$samples)) s$op
        else sprintf("%s[%s]", s$op, paste(s$samples, collapse = ","))
    }, character(1L)), collapse = " -> ")
}

#' @rdname giottoSpace-access
#' @export
setMethod("[", signature(x = "giottoSpace"), function(x, i, j, ...) {
    .space_assert_one_index(j)
    if (missing(i)) return(x)
    if (!is.character(i)) {
        x@steps <- x@steps[i]
        return(x)
    }
    checkmate::assert_character(i, len = 1L, .var.name = "i")
    if (is.na(i)) {
        stop("[space] cannot narrow to NA: narrowing needs a sample name, ",
            "and \"narrow to nobody\" has no meaning. To read the steps of ",
            "a handle with no sample identity, use `sp[[NA_character_]]`.",
            call. = FALSE)
    }
    # Narrow, do not erase: every surviving step still says it is for `i`.
    # Membership follows for free, because a member step is a step with a
    # scope and nothing else.
    x@steps <- .space_narrow_steps(x@steps, i)
    x
})

#' @rdname giottoSpace-access
#' @export
setMethod("[[", signature(x = "giottoSpace"), function(x, i, j, ...) {
    .space_assert_one_index(j)
    if (!is.character(i)) return(x@steps[[i]])
    checkmate::assert_character(i, len = 1L, .var.name = "i")
    # `NA` is "no sample identity". If exactly one sample is mentioned
    # across the whole recipe, that is who this handle is about -- the case
    # after `sp["a"]` -- so the two-call seam reads its steps back without
    # being told the name twice. Two or more and it does not guess; none
    # and there is nothing to guess, so the unscoped steps are the answer.
    if (is.na(i)) {
        named <- .space_step_samples(x@steps)
        if (length(named) == 1L) i <- named
    }
    .space_steps_for(x@steps, i)
})

#' @rdname giottoSpace-access
#' @export
setMethod("length", signature(x = "giottoSpace"),
    function(x) length(x@steps))

# The samples the recipe MENTIONS -- the keys `[[` accepts. Whether that
# is the whole coverage is the class's question, not this one; see the
# Membership section above.
#' @rdname giottoSpace-access
#' @export
setMethod("names", signature(x = "giottoSpace"),
    function(x) .space_step_samples(x@steps))

#' @rdname giottoSpace-access
#' @export
setMethod("as.list", signature(x = "combinedSpace"),
    function(x, ...) stats::setNames(
        list(list(kind = "combined", steps = x@steps)), x@name))

#' @rdname giottoSpace-access
#' @export
setMethod("as.list", signature(x = "perSampleSpace"),
    function(x, ...) stats::setNames(
        list(list(kind = "perSample", steps = x@steps)), x@name))

#' @rdname giottoSpace-access
#' @export
setMethod("+", signature(e1 = "combinedSpace", e2 = "combinedSpace"),
    function(e1, e2) {
        e1@name <- .merged_recipe_name(e1, e2)
        e1@steps <- c(e1@steps, e2@steps)
        e1
    }
)

#' @rdname giottoSpace-access
#' @export
setMethod("+", signature(e1 = "perSampleSpace", e2 = "perSampleSpace"),
    function(e1, e2) {
        e1@name <- .merged_recipe_name(e1, e2)
        e1@steps <- c(e1@steps, e2@steps)
        e1
    }
)

# The kinds answer different questions -- whether the samples share one
# coordinate system -- so there is no merge that preserves both. Refusing
# is what keeps the job size readable off the result.
#' @rdname giottoSpace-access
#' @export
setMethod("+", signature(e1 = "giottoSpace", e2 = "giottoSpace"),
    function(e1, e2) {
        stop("[space] cannot compose a ", class(e1)[[1L]], " with a ",
            class(e2)[[1L]], ": one lays its samples out in a shared ",
            "coordinate system and the other keeps them independent, so ",
            "the merge would have no job size. Slot them separately.",
            call. = FALSE)
    }
)

#' @rdname giottoSpace-access
#' @export
setMethod("show", signature(object = "combinedSpace"), function(object) {
    cat("An object of class combinedSpace\n")
    members <- names(object)
    cat(sprintf("space '%s' | %d sample(s) share one coordinate system\n",
        object@name, length(members)))
    if (length(members) > 0L) {
        cat("  members :", paste(members, collapse = ", "), "\n")
    }
    cat("  steps   :", .space_ops_str(object@steps), "\n")
    invisible(NULL)
})

#' @rdname giottoSpace-access
#' @export
setMethod("show", signature(object = "perSampleSpace"), function(object) {
    cat("An object of class perSampleSpace\n")
    cat(sprintf("space '%s' | each sample in its own copy\n", object@name))
    scoped <- names(object)
    if (length(scoped) > 0L) {
        cat("  scoped  :", paste(scoped, collapse = ", "), "\n")
    }
    cat("  steps   :", .space_ops_str(object@steps), "\n")
    invisible(NULL)
})

