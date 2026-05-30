# docs ----------------------------------------------------------- #
#' @title Spatial relationships between geometries
#' @name relate
#' @description `relate()` returns a logical matrix indicating the presence or
#'  absence of a specific spatial relationships between the geometries in
#'  x and y.
#' @param x spatial object with records to test
#' @param y spatial object records to test relations against
#' @param ... additional args to pass
#' @param output character. `"data.table"` or `"matrix"`. `"data.table"` is
#' only possible when `pairs=TRUE`
#' @param use_names logical. If `TRUE`, `pairs=TRUE`, and `output="data.table"`
#' the IDs of the geometries will be used.
#' @returns `data.table` if `output="data.table"`. `matrix` if `output="matrix"`
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#' activeSpatUnit(g) <- "aggregate"
#' sl <- g[["spatial_locs"]][[1]]
#' gpoints <- g[["feat_info"]][[1]]
#' gpoly <- g[["spatial_info"]][[1]]
#'
#' res1 <- relate(gpoints, gpoly, relation = "intersects")
#' res2 <- relate(gpoints, gpoly, relation = "intersects", use_names = FALSE)
#'
#' selection <- system.file("extdata/viz_interactive_select.csv",
#'     package = "GiottoClass"
#' )
#' select_polys <- createGiottoPolygon(
#'     # we don't want the rownumber column.
#'     data.table::fread(selection)[, c("x", "y", "name")]
#' )
#' res <- relate(g, select_polys, relation = "intersects")
#' g[, res[y == "polygon1", x]]
#' g[, res[y == "polygon2", x]]
#' g[, res[y == "polygon3", x]]
NULL
# ---------------------------------------------------------------- #

#' @rdname relate
#' @inheritParams terra::relate
#' @export
setMethod(
    "relate", signature(x = "giottoSpatial", y = "giottoSpatial"),
    function(
        x, y, relation,
        pairs = TRUE,
        na.rm = TRUE,
        output = c("data.table", "matrix"),
        use_names = TRUE,
        ...) {
        output <- match.arg(output, choices = c("data.table", "matrix"))

        if (inherits(x, "spatLocsObj")) x_use <- as.points(x)
        if (inherits(y, "spatLocsObj")) y_use <- as.points(y)
        if (inherits(x, "giottoSpatial")) x_use <- x[]
        if (inherits(x, "giottoSpatial")) y_use <- y[]

        res <- relate(x_use, y_use, relation, pairs, na.rm, ...)

        if (pairs && output == "data.table") {
            res <- data.table::as.data.table(res)
            data.table::setnames(res, new = c("x", "y"))

            if (use_names) {
                x_ids <- .get_ids(x, res$x)
                y_ids <- .get_ids(y, res$y)
                res[, x := x_ids]
                res[, y := y_ids]
            }
        }

        return(res)
    }
)

#' @rdname relate
#' @param what character. Which type of spatial data in the `giotto` object to
#' relate. One of "polygon", "spatlocs", "points"
#' @param spat_unit spatial unit
#' @param feat_type feature type
#' @param spat_locs_name name of spatlocs to use if what = "spatlocs"
#' @export
setMethod(
    "relate", signature(x = "giotto", y = "giottoSpatial"),
    function(
        x, y, ...,
        what = c("polygon", "spatlocs", "points"),
        spat_unit = NULL,
        feat_type = NULL,
        spat_locs_name = NULL) {
        what <- match.arg(what, c("polygon", "spatlocs", "points"))

        spat_unit <- set_default_spat_unit(x, spat_unit = spat_unit)
        feat_type <- set_default_feat_type(
            x,
            spat_unit = spat_unit, feat_type = feat_type
        )

        x <- switch(what,
            "polygon" = {
                getPolygonInfo(x,
                    polygon_name = spat_unit,
                    return_giottoPolygon = TRUE
                )
            },
            "points" = {
                getFeatureInfo(x,
                    feat_type = feat_type,
                    return_giottoPoints = TRUE
                )
            },
            "spatlocs" = {
                getSpatialLocations(x,
                    spat_unit = spat_unit,
                    output = "spatLocsObj",
                    name = spat_locs_name
                )
            }
        )

        res <- relate(x, y, ...)
        return(res)
    }
)






# spatRelate ####

# TODO: audit internal `relate()` call sites across the suite and swap to
# `spatRelate()` where the pattern is "narrow x by predicate against y"
# rather than "consume the relation table/matrix".

#' @title Spatial relationship as a filter
#' @name spatRelate
#' @description
#' Narrow `x` to features that satisfy a spatial predicate against any feature
#' of `y`. Returns an object of the same class as `x` rather than a relation
#' matrix -- the "filter form" complement to [relate()].
#'
#' This generic exists to support **lazy** spatial filtering on backed
#' (on-disk) representations, where materializing a full relation matrix as an
#' intermediate would be wasteful. Methods on in-memory `giottoSpatial`
#' classes evaluate eagerly via [relate()] + subset; the GiottoDisk package
#' adds methods for on-disk `parquetGeomBase`-inheriting stores that queue
#' the predicate as a lazy op, evaluated at `storeRead()` time.
#'
#' @param x spatial object to be narrowed (rows kept where predicate holds
#'   against any feature of `y`)
#' @param y query geometry; the form depends on the method (giottoSpatial,
#'   SpatVector, sf, character WKT, or an on-disk store via GiottoDisk)
#' @param relation `character`. Spatial predicate. One of `"intersects"`,
#'   `"touches"`, `"crosses"`, `"overlaps"`, `"within"`, `"contains"`,
#'   `"covers"`, `"covered_by"`, `"disjoint"`. Default `"intersects"`.
#' @param ... additional args to pass
#' @returns an object of the same class as `x`, narrowed to features
#'   satisfying the predicate against any feature of `y`
#' @seealso [relate()] for the relation-matrix / pairs form;
#'   [spatQuery()] for the gobject-level multi-filter pipeline.
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#' gpoly <- g[["spatial_info"]][[1]]
#' gpoints <- g[["feat_info"]][[1]]
#'
#' # narrow points to those that intersect at least one polygon
#' pts_in_polys <- spatRelate(gpoints, gpoly, relation = "intersects")
NULL

#' @rdname spatRelate
#' @export
setMethod(
    "spatRelate", signature(x = "giottoSpatial", y = "giottoSpatial"),
    function(x, y, relation = "intersects", ...) {
        # Eager: use relate() to get pairs, then subset x to features with
        # any match. The pairs data.table is intermediate but in-memory --
        # acceptable. On-disk stores avoid this intermediate by queuing the
        # predicate as a lazy op (see GiottoDisk's parquetGeomBase methods).
        res <- relate(
            x, y,
            relation = relation,
            pairs = TRUE,
            output = "data.table",
            use_names = FALSE,
            ...
        )
        if (nrow(res) == 0L) {
            return(x[integer(0L)])
        }
        keep_idx <- sort(unique(res$x))
        x[keep_idx]
    }
)


# internals ####

.get_ids <- function(x, idx) {
    ids <- x[idx]$cell_ID
    ids <- ids %null% x[idx]$feat_ID
    ids <- ids %null% x[idx]$poly_ID
    if (is.null(ids)) {
        stop("no ids found for an object. `use_names` might not work",
            call. = FALSE
        )
    }
    return(ids)
}
