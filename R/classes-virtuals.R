
# OLDCLASS ####
setOldClass("giottoInstructions")

# MISC ####
## * Define class unions ####

setClassUnion("nullOrChar", c("NULL", "character"))
setClassUnion("nullOrList", c("NULL", "list"))
setClassUnion("nullOrInstructions", c("nullOrList", "giottoInstructions"))
setClassUnion("nullOrDatatable", c("NULL", "data.table"))
setClassUnion("nullOrLogical", c("NULL", "logical"))
# see zzz.R for allMatrix

#' @title gIndex
#' @description
#' class for handling indices similar to `index` class from \pkg{Matrix}
#' simple class union (setClassUnion) of "numeric", "logical" and "character".
#' @keywords internal
#' @noRd
setClassUnion("gIndex", c("numeric", "logical", "character"))

# ** gsource class ####
#' @name gsource
#' @title Giotto Sources
#' @description
#' Object defining a data backend for a Giotto object. The `gsource`
#' class is VIRTUAL. Specific backend types should extend from this class.
#' @export
setClass("gsource", contains = "VIRTUAL")

# ** giottoSubobject Class ####
#' @keywords internal
#' @noRd
setClass(
    "giottoSubobject",
    contains = "VIRTUAL"
)

# ** gdtData Class ####
#' @description
#' umbrella class for referring to Giotto's normal data.table-based slots for
#' extraction purposes
#' @keywords internal
#' @noRd
setClass(
    "gdtData",
    contains = "VIRTUAL"
)


# ** nameData Class ####
#' @keywords internal
#' @noRd
setClass("nameData",
    contains = "VIRTUAL",
    slots = list(name = "character"),
    prototype = prototype(name = NA_character_)
)

# ** exprData Class ####
#' Basic class for classes with expression information
#' @keywords internal
#' @noRd
setClass("exprData",
    contains = "VIRTUAL",
    slots = list(exprMat = "ANY"),
    prototype = prototype(exprMat = NULL)
)



# ** coordData Class ####
#' Basic class for classes with coordinate information
#'
#' @description
#' coordDataDT is the specific flavor that deals with objects where the
#' coordinate information is stored within data.table objects and should work
#' similarly to data.table when interacting with some basic generic operators
#' for data retreival and setting.
#'
#' @keywords internal
#' @noRd
setClass("coordDataDT",
    contains = c("VIRTUAL", "gdtData"),
    slots = list(coordinates = "data.table"),
    prototype = prototype(coordinates = data.table::data.table())
)





# setClass('coordDataMT',
#          slots = list(coordinates = 'matrix'),
#          prototype = prototype(coordinates = matrix()))


# ** metaData Class ####
#' Basic class for classes with metadata information
#'
#' @description
#' Classes that inherit from this class will contain a metadata slot that
#' stores information in a data.table and should work similarly to data.table
#' when interacting with some basic generic operators for data retrieval and
#' setting
#' @keywords internal
#' @noRd
setClass("metaData",
    contains = c("VIRTUAL", "gdtData"),
    slots = list(
        metaDT = "data.table",
        col_desc = "character"
    ),
    prototype = methods::prototype(
        metaDT = data.table::data.table(),
        col_desc = NA_character_
    )
)





# ** enrData ####
#' enrData
#' @keywords internal
#' @noRd
setClass("enrData",
    contains = c("VIRTUAL", "gdtData"),
    slots = list(
        method = "character",
        enrichDT = "nullOrDatatable"
    ),
    prototype = methods::prototype(
        method = NA_character_,
        enrichDT = NULL
    )
)





# ** nnData ####
#' @keywords internal
#' @noRd
setClass("nnData",
    contains = "VIRTUAL",
    slots = list(
        nn_type = "character",
        igraph = "ANY"
    ),
    prototype = methods::prototype(
        nn_type = NA_character_,
        igraph = NULL
    )
)


# ** spatNetData ####
#' @keywords internal
#' @noRd
setClass("spatNetData",
    contains = "VIRTUAL",
    slots = list(
        method = "character",
        parameters = "ANY",
        outputObj = "ANY",
        networkDT = "nullOrDatatable",
        networkDT_before_filter = "nullOrDatatable",
        cellShapeObj = "ANY"
    ),
    prototype = methods::prototype(
        method = NA_character_,
        parameters = NULL,
        outputObj = NULL,
        networkDT = NULL,
        networkDT_before_filter = NULL,
        cellShapeObj = NULL
    )
)




# ** spatGridData ####
#' @keywords internal
#' @noRd
setClass("spatGridData",
    contains = "VIRTUAL",
    slots = list(
        method = "character",
        parameters = "ANY",
        gridDT = "nullOrDatatable"
    ),
    prototype = prototype(
        method = NA_character_,
        parameters = NULL,
        gridDT = NULL
    )
)





# ** provData Class ####
#' Basic class for classes with provenance information.
#'
#' @description
#' This kind of information is necessary when generating data that is
#' aggregated from multiple original sources of raw information. This could
#' refer to situations such as when producing cellxfeature expression matrices
#' from subcellular transcript information and polygons that are provided as
#' multiple z layers. Provenance is Giotto's method of mapping this aggregated
#' information back to the original z layers that were used in its generation.
#'
#' @keywords internal
#' @noRd
setClass("provData",
    contains = "VIRTUAL",
    slots = list(provenance = "ANY"),
    prototype = prototype(provenance = NULL)
)



# ** spatData Class ####
#' Basic class for classes with spatial information
#'
#' @description
#' Classes that inherit from this class will contain a spat_unit slot that
#' describes which spatial unit the data belongs to. This is most relevant
#' to aggregated information. Subcellular information such as poly data
#' in \code{spatial_info} slot essentially define their own spatial units.
#' Within slots that deal with classes that contain spatData,
#' there is a nesting structure that first nests by spatial unit.
#' @keywords internal
#' @noRd
setClass("spatData",
    contains = c("provData", "VIRTUAL"),
    slots = list(spat_unit = "character"), # not allowed to be NULL
    prototype = prototype(spat_unit = NA_character_)
)



# ** featData Class ####
#' @title Basic class for classes with feature information
#'
#' @description
#' Features in Giotto are a blanket term for any features that are detected,
#' covering modalities such as, but not limited to rna, protein, ATAC, and
#' even QC probes. Classes that inherit from this class will contain a
#' feat_type slot that describes which feature type the data is. Within slots
#' that deal with classes that contain featData, there is a nesting structure
#' that usually first nests by spatial unit and then by feature type.
#' @keywords internal
#' @noRd
setClass("featData",
    contains = "VIRTUAL",
    slots = list(feat_type = "character"), # not allowed to be NULL
    prototype = prototype(feat_type = NA_character_)
)

# ** spatFeatData ####
#' @description Superclass for classes that contain both spatial and feature
#' data
#' @keywords internal
#' @noRd
setClass("spatFeatData",
         contains = c("spatData", "featData", "VIRTUAL")
)

# ** miscData Class ####
#' @title Basic class for additional miscellaneous information
#'
#' @description
#' Classes (such as dimObj) that can hold information from multiple types of
#' methods use the misc slot to hold additional information specific to each
#' method. Information may be stored within as S3 structures.
#' @returns slot for miscellaneous information
#' @examples
#' g <- GiottoData::loadSubObjectMini("dimObj")
#'
#' slot(g, "misc")
setClass("miscData",
    contains = "VIRTUAL",
    slots = list(misc = "ANY"),
    prototype = prototype(misc = NULL)
)


# ** terraVectData Class ####
#' @title Basic class for terra SpatVector-based objects
#' @description
#' Classes that inherit from this class will contain a spatVector slot meant to
#' hold and work with terra SpatVector objects
#' @returns object with spatVector slot
terraVectData <- setClass(
    "terraVectData",
    contains = "VIRTUAL",
    slots = list(spatVector = "ANY"),
    prototype = prototype(spatVector = NULL)
)
