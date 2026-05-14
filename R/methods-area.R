# docs ----------------------------------------------------------- #
#' @title Get the area of individual polygons
#' @name expanse
#' @aliases expanse
#' @description Compute the area covered by polygons
#' @details
#' Giotto's `expanse()` method dispatches on terra's generic. When the
#' underlying data is a `SpatVector`, `transform` defaults to `FALSE` and
#' CRS warnings are suppressed — both appropriate for biological coordinate
#' systems. When the underlying data is a non-terra representation (e.g. a
#' disk-backed store), the call is forwarded to a method for that class.
#'
#' @param x `giottoPolygon`
#' @param output one of `"data.table"` (default), `"named"`, or `"vector"`.
#' `"data.table"` returns a `data.table` with columns `cell_ID` and `area`.
#' `"named"` returns a named numeric vector with `cell_ID` as names.
#' `"vector"` returns a plain unnamed numeric vector (terra-compatible).
#' @inheritDotParams terra::expanse
#' @returns depends on `output`: a `data.table`, named `numeric`, or `numeric`
#' @examples
#' sl <- GiottoData::loadSubObjectMini("spatLocsObj")
#' gpoly <- GiottoData::loadSubObjectMini("giottoPolygon")
#' gpoints <- GiottoData::loadSubObjectMini("giottoPoints")
#'
#' # area of polygons
#' head(expanse(gpoly))
#'
#' # area of the convex hull
#' expanse(hull(sl))
#' feature_hulls <- hull(gpoints, by = "feat_ID")
#' head(expanse(feature_hulls))
#'
#' # output formats
#' expanse(gpoly, output = "named")
#' expanse(gpoly, output = "vector")
#'
NULL
# ---------------------------------------------------------------- #

#' @rdname expanse
#' @export
setMethod("expanse", signature("giottoPolygon"), function(x, output = c("data.table", "named", "vector"), ...) {
    output <- match.arg(output)
    data <- x[]
    if (inherits(data, "SpatVector")) {
        terra::crs(data) <- "local"
        args <- list(data, ...)
        args$transform <- args$transform %null% FALSE
        areas <- do.call(terra::expanse, args)
        switch(output,
            vector = areas,
            named = stats::setNames(areas, data$poly_ID),
            data.table = data.table::data.table(cell_ID = data$poly_ID, area = areas)
        )
    } else {
        expanse(data, output = output, ...)
    }
})

#' @rdname expanse
#' @export
setMethod("area", signature("giottoPolygon"), function(x, ...) {
    deprecate_warn("0.5.1", "area()", "expanse()")
    expanse(x, ...)
})
