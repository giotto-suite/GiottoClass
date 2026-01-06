#' @include classes-virtuals.R
NULL

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

#' @param overlap_data `data.table` of extracted intensity values per poly_ID
#' @noRd
.create_overlap_intensity_dt <- function(overlap_data) {
    odt <- new("overlapIntensityDT", data = overlap_data)
    odt@nfeats <- ncol(overlap_data) - 1L
    odt
}

#' @param x from data (SpatVector) - need just the poly_ID info
#' @param y to data (SpatVector) - need meta info extracted from cols by overlap info + nrow
#' @param overlap_data relationships (data.frame). Expected to be numeric row
#' indices between x and y
#' @param keep additional col(s) in `y` to keep
#' @examples
#' d <- data.frame(a = sort(rep(1:2, 2)), b = 1:4)
#'
#' @noRd
.create_overlap_point_dt <- function(x, y,
        overlap_data, keep = NULL, feat_ids) {
    poly <- feat_idx <- feat <- feat_id_index <- NULL # NSE vars
    # cleanup input overlap_data
    checkmate::assert_data_frame(overlap_data)
    data.table::setDT(overlap_data)
    cnames <- colnames(overlap_data)
    data.table::setnames(overlap_data,
        old = c(cnames[[2]], cnames[[1]]),
        new = c("poly", "feat_idx")
    )
    # make relationships table sparse by removing non-overlapped features
    # these results are indexed by all features, so no need to filter
    # non-overlapped polys
    overlap_data <- overlap_data[!is.na(poly)]

    # extract needed info from y
    keep <- c("feat_ID", keep)
    ytab <- terra::as.data.frame(y[overlap_data$feat_idx, keep])

    # initialize overlap object and needed ids
    sids <- x$poly_ID
    fids <- unique(ytab$feat_ID)
    odt <- new("overlapPointDT",
        spat_ids = sids,
        feat_ids = feat_ids,
        nfeats = as.integer(nrow(y))
    )

    # Ensure data is stored as integer or integer-based mapping
    ## - if poly/feat_idx contents are NOT integer coercible, establish a map #
    if (!overlap_data[, checkmate::test_integerish(head(poly, 100))]) {
        overlap_data[, poly := match(poly, sids)]
    }
    if (!overlap_data[, checkmate::test_integerish(head(feat_idx, 100))]) {
        overlap_data[, feat_idx := match(feat_idx, fids)]
    }
    ## -- if still not integer, coerce to integer --------------------------- #
    if (!is.integer(overlap_data$poly[1])) {
        overlap_data[, poly := as.integer(poly)]
    }
    if (!is.integer(overlap_data$feat_idx[1])) {
        overlap_data[, feat_idx := as.integer(feat_idx)]
    }

    # append y attribute info
    overlap_data <- cbind(overlap_data, ytab)
    data.table::setnames(overlap_data,
        old = c("feat_ID_uniq", "feat_ID"),
        new = c("feat", "feat_id_index")
    )
    if (!is.integer(overlap_data$feat[1])) {
        overlap_data[, feat := as.integer(feat)]
    }
    # add feat_ID map
    overlap_data[, feat_id_index := match(feat_id_index, odt@feat_ids)]
    # remove feat_idx which may not be reliable after feature subsets
    overlap_data[, feat_idx := NULL]
    # set indices
    data.table::setkeyv(overlap_data, "feat")
    data.table::setindexv(overlap_data, "poly")
    data.table::setcolorder(overlap_data, c("poly", "feat", "feat_id_index"))
    # add to object
    odt@data <- overlap_data

    odt
}

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