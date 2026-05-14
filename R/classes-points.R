#' @include classes-virtuals.R
NULL

## * definition ####
# giottoPoints class

#' @title S4 giotto points Class
#' @description Giotto class to store and operate on points data
#' @concept giotto points class
#' @slot feat_type name of feature type
#' @slot spatVector terra spatVector to store point shapes
#' @slot networks feature networks
#' @slot unique_ID_cache cached unique feature IDs that should match the
#' spatVector slot
#' @details Contains vector-type feature data
#' @returns giottoPoints
#' @examples
#' giottoPoints()
#' @export
giottoPoints <- setClass(
    Class = "giottoPoints",
    contains = c("featData", "terraVectData", "giottoSubobject"),
    slots = c(
        networks = "ANY",
        unique_ID_cache = "character"
    ),
    prototype = list(
        networks = NULL,
        unique_ID_cache = NA_character_
    )
)


#' @title S4 giotto feature network Class
#' @description Giotto class to store and operate on feature network
#' @concept giotto points network class
#' @slot name name of feature network
#' @slot network_datatable feature network in data.table format
#' @slot network_lookup_id table mapping numeric network ID to unique
#' feature numerical IDs
#' @slot full fully connected network
#' @details contains feature network information
#' @returns featureNetwork
#' @examples
#' featureNetwork()
#' @export
featureNetwork <- setClass(
    Class = "featureNetwork",
    contains = c("nameData", "giottoSubobject"),
    slots = c(
        network_datatable = "ANY",
        network_lookup_id = "ANY",
        full = "ANY"
    ),
    prototype = list(
        network_datatable = NULL,
        network_lookup_id = NULL,
        full = NULL
    )
)

#' @title Update giotto points object
#' @name updateGiottoPointsObject
#' @param gpoints giotto points object
#' @returns GiottoPointsObject
#' @examples
#' g <- GiottoData::loadSubObjectMini("giottoPoints")
#'
#' updateGiottoPointsObject(g)
#' @export
updateGiottoPointsObject <- function(gpoints) {
    if (!inherits(gpoints, "giottoPoints")) {
        stop("This function is only for giottoPoints")
    }

    # 3.2.X adds cacheing of IDs
    if (is.null(attr(gpoints, "unique_ID_cache"))) {
        attr(gpoints, "unique_ID_cache") <- unique(
            as.list(gpoints@spatVector)$feat_ID
        )
    }

    gpoints
}













# * packed classes ####

# for use with wrap() generic
setClass(
    "packedGiottoPoints",
    slots = c(
        feat_type = "character",
        packed_spatVector = "ANY",
        networks = "ANY",
        unique_ID_cache = "character"
    ),
    prototype = list(
        feat_type = NA_character_,
        packed_spatVector = NULL,
        networks = NULL,
        unique_ID_cache = NA_character_
    )
)