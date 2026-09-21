#' @title Set giotto subobjects into giotto object
#' @name setGiotto
#' @param gobject giotto object
#' @param x giottoSubobject to set
#' @param verbose be verbose
#' @param \dots additional params to pass to specific Giotto setter functions
#' @family functions to set data in giotto object
#' @returns giottoSubobject
#' @examples
#' g <- createGiottoObject()
#' g_expression <- GiottoData::loadSubObjectMini("exprObj")
#'
#' setGiotto(gobject = g, x = g_expression)
NULL



# methods ####

#' @rdname setGiotto
#' @export
setMethod(
    "setGiotto", signature("giotto", "list"),
    function(gobject, x, verbose = TRUE, ...) {
        # suspend init and checking until all items are added
        a <- list(...)
        init <- !isFALSE(a$initialize)

        init_opt <- getOption("giotto.init", TRUE)
        cv_opt <- getOption("giotto.check_valid", TRUE)

        .reset_opts <- function() {
            options("giotto.init" = init_opt)
            options("giotto.check_valid" = cv_opt)
        }

        on.exit(.reset_opts, add = TRUE)
        options("giotto.init" = FALSE)
        options("giotto.check_valid" = FALSE)

        for (item in x) {
            gobject <- setGiotto(gobject, item, verbose = verbose, ...)
        }

        .reset_opts()
        if (init) gobject <- initialize(gobject)
        return(gobject)
    }
)

#' @rdname setGiotto
#' @export
setMethod(
    "setGiotto", signature("giotto", "cellMetaObj"),
    function(gobject, x, ...) {
        gobject <- setCellMetadata(gobject = gobject, x = x, ...)
        gobject
    }
)

#' @rdname setGiotto
#' @export
setMethod(
    "setGiotto", signature("giotto", "featMetaObj"),
    function(gobject, x, ...) {
        gobject <- setFeatureMetadata(gobject = gobject, x = x, ...)
        gobject
    }
)

#' @rdname setGiotto
#' @export
setMethod(
    "setGiotto", signature("giotto", "exprObj"),
    function(gobject, x, ...) {
        gobject <- setExpression(gobject = gobject, x = x, ...)
        gobject
    }
)

#' @rdname setGiotto
#' @export
setMethod(
    "setGiotto", signature("giotto", "giottoPoints"),
    function(gobject, x, ...) {
        gobject <- setFeatureInfo(gobject = gobject, x = x, ...)
        gobject
    }
)

#' @rdname setGiotto
#' @export
setMethod(
    "setGiotto", signature("giotto", "giottoPolygon"),
    function(gobject, x, ...) {
        gobject <- setPolygonInfo(gobject = gobject, x = x, ...)
        gobject
    }
)

#' @rdname setGiotto
#' @export
setMethod(
    "setGiotto", signature("giotto", "dimObj"),
    function(gobject, x, ...) {
        gobject <- setDimReduction(gobject = gobject, x = x, ...)
        gobject
    }
)

#' @rdname setGiotto
#' @export
setMethod(
    "setGiotto", signature("giotto", "spatLocsObj"),
    function(gobject, x, ...) {
        gobject <- setSpatialLocations(gobject = gobject, x = x, ...)
        gobject
    }
)

#' @rdname setGiotto
#' @export
setMethod(
    "setGiotto", signature("giotto", "spatEnrObj"),
    function(gobject, x, ...) {
        gobject <- setSpatialEnrichment(gobject = gobject, x = x, ...)
        gobject
    }
)

#' @rdname setGiotto
#' @export
setMethod(
    "setGiotto", signature("giotto", "nnNetObj"),
    function(gobject, x, ...) {
        gobject <- setNearestNetwork(gobject = gobject, x = x, ...)
        gobject
    }
)

#' @rdname setGiotto
#' @export
setMethod(
    "setGiotto", signature("giotto", "spatialNetworkObj"),
    function(gobject, x, ...) {
        gobject <- setSpatialNetwork(gobject = gobject, x = x, ...)
        gobject
    }
)

# TODO update this
#' @rdname setGiotto
#' @export
setMethod(
    "setGiotto", signature("giotto", "giottoLargeImage"),
    function(gobject, x, ...) {
        gobject <- setGiottoImage(
            gobject = gobject,
            x = x,
            name = x@name,
            ...
        )
        gobject
    }
)

#' @rdname setGiotto
#' @export
setMethod(
    "setGiotto", signature("giotto", "giottoImage"),
    function(gobject, x, ...) {
        gobject <- setGiottoImage(
            gobject = gobject,
            x = x,
            name = x@name,
            ...
        )
        gobject
    }
)

# recipes ####

# A recipe is keyed by name in `@view` / `@spaces`, so it can place itself
# the way every other named subobject does. The name is not optional here:
# `setGiotto()` has nowhere to put a recipe that does not say where it goes,
# and inventing a key would put a frame somewhere nothing resolves against.
# Use `giottoView(x, "<name>") <- ` to name and place in one step.
.recipe_set_name <- function(x, what) {
    nm <- objName(x)
    if (length(nm) != 1L || is.na(nm) || !nzchar(nm)) {
        stop("[setGiotto] this ", what, " has no name, so there is no slot ",
            "to place it in. Name it with `objName(x) <- \"<name>\"`, or ",
            "place it directly with `", what, "(gobject, \"<name>\") <- x`.",
            call. = FALSE)
    }
    nm
}

#' @rdname setGiotto
#' @export
setMethod(
    "setGiotto", signature("gAny", "giottoView"),
    function(gobject, x, ...) {
        giottoView(gobject, .recipe_set_name(x, "giottoView")) <- x
        gobject
    }
)

#' @rdname setGiotto
#' @export
setMethod(
    "setGiotto", signature("gAny", "giottoSpace"),
    function(gobject, x, ...) {
        giottoSpace(gobject, .recipe_set_name(x, "giottoSpace")) <- x
        gobject
    }
)
