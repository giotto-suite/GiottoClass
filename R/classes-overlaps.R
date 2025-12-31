
# * definitions ####
setClass("overlapInfo",
    contains = c("spatFeatData", "VIRTUAL"),
    slots = list(data = "ANY")
)
setClass("overlapPoint", contains = c("overlapInfo", "VIRTUAL"))
setClass("overlapIntensity", contains = c("overlapInfo", "VIRTUAL"))

#' @name overlapPointDT-class
#' @title Polygon and Point Relationships
#' @description
#' Utility class for storing overlaps relationships between polygons and points
#' in a sparse `data.table` format. Retrieve the unique ID index of overlapped
#' points `[i, ]`. Get indices of which polys are overlapping specific feature
#' species using `[, j]`.
#'
#' Subsetting with `ids = FALSE` and `[i, j]` indexing is also supported.
#'
#' Supports `as.matrix` for conversion to `dgCMatrix`. Contained poly and
#' feature names simplify rownames/colnames and empty row/col creation.
#'
#' @slot data data.table. Table containing 3 integer cols:
#'
#'   * `poly` - polygon index. Maps to `spat_ids` slot.
#'   * `feat` - feat_ID_uniq (unique integer identifier) of a point detection
#'   * `feat_id_index` - index of feature name mapping in `@feat_ids` slot.
#' @slot spat_unit character. Spatial unit (usually name of polygons information)
#' @slot feat_type character. Feature type (usually name of points information)
#' @slot provenance character. provenance information
#' @slot spat_ids character. Polygon names
#' @slot feat_ids character. Feature names
#' @slot nfeats integer (optional metadata). How many feature points were
#'  used in overlap operation. Gives an idea of sparsity, but has no effect on
#'  processing.
#'
#' @param x object
#' @param i numeric, character, logical. Index of or name of poly in overlapping
#' polygons
#' @param j numeric, character, logical. Index of or name of feature being
#' overlapped.
#' @param use_names logical (default = `FALSE`). Whether to return as integer
#' indices or with character ids.
#' @param ids logical (default = `TRUE`). Whether to return the requested
#' integer indices (`TRUE`) or the subset overlap object (`FALSE`).
#' @param drop not used.
#' @param \dots additional params to pass (none implemented)
#' @returns integer or character if only `i` or `j` provided, depending on
#' `use_names`. A subset `overlapPointDT` if both `i` and `j` are used.
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#' poly <- g[["spatial_info", "z0"]][[1]]
#' ovlp <- overlaps(poly, "rna")
#' ovlp
#'
#' as.matrix(ovlp)
#'
#' dim(ovlp)
#' nrow(ovlp) # number of relationships
#'
#' # get feature unique IDs overlapped by nth poly
#' ovlp[1] # check one (no overlaps returns integer(0))
#' ovlp[1:5] # check multiple
#' ovlp[1:5, use_names = TRUE] # returns feature names, but no longer unique
#'
#' # get integer index of poly(s) overlapping particular feature species
#' ovlp[, 1]
#' ovlp[, "Mlc1"] # this is the same
#'
#' # get a subset of overlap object
#' ovlp[1:10, ids = FALSE] # subset to first 10 polys
#' ovlp[, 1:10, ids = FALSE] # subset to first 10 feature species
#' ovlp[1:10, 1:10] # subset to first 10 polys and first 10 features species
#' @exportClass overlapPointDT
setClass("overlapPointDT",
    contains = "overlapPoint",
    slots = list(
        spat_ids = "character", # spat_ids are unique, no need to record npoly
        feat_ids = "character",
        nfeats = "integer"
    )
)
setClass("overlapIntensityDT",
    contains = "overlapIntensity",
    slots = list(
        nfeats = "integer", # skip spat_ids/feat_ids. This data is not sparse
        fun = "character"
    )
)

# * internals ####

# update old overlaps information to new `overlapInfo`
.update_overlaps <- function(x, ...) {

    if (inherits(x, "giottoPolygon")) {
        res <- .update_overlaps(x@overlaps,
            poly_ids = x$poly_ID,
            spat_unit = spatUnit(x),
            ...
        )
        x@overlaps <- res
        return(x)
    }
    if (inherits(x, "list")) {
        list_names <- names(x)
        list_res <- lapply(list_names, function(ovlp_name) {
            if (ovlp_name == "intensity") {
                # recurse over list of intensity overlaps (can't set feat_type)
                intensity_res <- .update_overlaps(x[["intensity"]], ...)
                return(intensity_res)
            } else {
                updated_res <- .update_overlaps(x[[ovlp_name]],
                    feat_type = ovlp_name,
                    ...
                )
                return(updated_res)
            }
        })
        names(list_res) <- list_names
        return(list_res)
    }
    if (inherits(x, "SpatVector")) {
        return(.update_overlaps_points(x, ...))
    }
    if (inherits(x, "data.table")) {
        return(.update_overlaps_intensity(x, ...))
    }
    x # allow passthrough if not matching either signature
}

#' @param x (SpatVector) the old overlaps representation to convert
#' @param poly_ids the `spatIDs()` of the giottoPolygon. This is since the
#' ordering of the polygon IDs within the overlaps data usually does not match.
#' @param spat_unit,feat_type spat_unit / feat_type
#' @noRd
.update_overlaps_points <- function(x, poly_ids,
    spat_unit = NA_character_, feat_type = NA_character_, ...) {
    checkmate::assert_character(poly_ids)
    data <- terra::as.data.frame(x)
    data.table::setDT(data)
    odt <- new("overlapPointDT",
        spat_unit = spat_unit,
        feat_type = feat_type,
        provenance = spat_unit,
        nfeats = as.integer(nrow(data))
    )
    odt@feat_ids <- unique(data$feat_ID)
    odt@spat_ids <- unique(poly_ids)
    data[, feat := as.integer(feat_ID_uniq)]
    data[, feat_ID_uniq := NULL]
    data.table::setnames(data,
        old = c("poly_ID", "feat_ID"),
        new = c("poly", "feat_id_index")
    )
    data <- data[!is.na(poly) & !is.na(feat),]    # drop NAs
    # Ensure data is stored as integer-based mapping
    data[, poly := match(poly, odt@spat_ids)]
    data[, feat_id_index := match(feat_id_index, odt@feat_ids)]
    data.table::setkeyv(data, "feat")
    data.table::setindexv(data, "poly")
    data.table::setcolorder(data, c("poly", "feat", "feat_id_index"))
    # add to object
    odt@data <- data
    odt
}

.update_overlaps_intensity <- function(x,
    spat_unit = NA_character_, feat_type = NA_character_, ...) {
    odt <- new("overlapIntensityDT",
        spat_unit = spat_unit,
        feat_type = feat_type,
        provenance = spat_unit,
        nfeats = as.integer(ncol(x) - 1)
    )
    fids <- setdiff(names(x), "poly_ID")
    x <- x[, lapply(.SD, sum), by = "poly_ID", .SDcols = fids]
    odt@data <- x
    odt
}