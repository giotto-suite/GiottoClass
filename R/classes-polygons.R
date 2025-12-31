# * definition ####
# giottoPolygon class

#' @title S4 giotto polygon Class
#' @description Giotto class to store and operate on polygon-like data
#' @concept giotto polygon class
#' @slot name name of polygon shapes
#' @slot spatVector terra spatVector to store polygon shapes
#' @slot spatVectorCentroids centroids of polygon shapes
#' @slot overlaps information about overlapping points and polygons
#' @slot unique_ID_cache cached unique spatial IDs that should match the
#' spatVector slot
#' @details holds polygon data
#' @returns giottoPolygon
#' @examples
#' giottoPolygon()
#' @export
giottoPolygon <- setClass(
    Class = "giottoPolygon",
    contains = c("nameData", "terraVectData", "giottoSubobject"),
    slots = c(
        spatVectorCentroids = "ANY",
        overlaps = "ANY",
        unique_ID_cache = "character"
    ),
    prototype = list(
        spatVectorCentroids = NULL,
        overlaps = NULL,
        unique_ID_cache = NA_character_
    )
)

#' @title Update giotto polygon object
#' @name updateGiottoPolygonObject
#' @param gpoly giotto polygon object
#' @returns GiottoPolygonObject
#' @examples
#' g <- GiottoData::loadSubObjectMini("giottoPolygon")
#'
#' updateGiottoPolygonObject(g)
#' @export
updateGiottoPolygonObject <- function(gpoly) {
    if (!inherits(gpoly, "giottoPolygon")) {
        stop("This function is only for giottoPolygon")
    }

    # 3.2.X adds cacheing of IDs
    if (is.null(attr(gpoly, "unique_ID_cache"))) {
        attr(gpoly, "unique_ID_cache") <- unique(
            as.list(gpoly@spatVector)$poly_ID
        )
    }

    # 0.4.7 changes overlaps representation
    # intersection `SpatVector` -> `overlapInfo`-inheriting classes
    gpoly <- .update_overlaps(gpoly)

    gpoly
}




# * packed classes ####

# for use with wrap() generic
setClass("packedGiottoPolygon",
    contains = c("nameData", "giottoSubobject"),
    slots = c(
        packed_spatVector = "ANY",
        packed_spatVectorCentroids = "ANY",
        packed_overlaps = "ANY",
        unique_ID_cache = "character"
    ),
    prototype = list(
        packed_spatVector = NULL,
        packed_spatVectorCentroids = NULL,
        packed_overlaps = NULL,
        unique_ID_cache = NA_character_
    )
)