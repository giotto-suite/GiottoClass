
# * definitions ####
setClass("giottoBinPoints",
    contains = c("featData", "giottoSubobject"),
    slots = list(
        spatial = "ANY", # spatvector etc.
        counts = "data.table",
        bid = "character", # bin ID (static)
        pmap = "integer", # point IDs by mapping onto bid
        fid = "character",
        compact = "logical" # state of compaction
    ),
    prototype = list(
        compact = TRUE
    )
)

# * methods ####
setMethod("show", signature("giottoBinPoints"), function(object) {
    cat(sprintf("An object of class %s\n", class(object)))
    .show_feat(object)
    plist <- c(
        dimensions = toString(dim(object)),
        compact = as.character(object@compact)
    )
    print_list(plist)
    print(as.data.table(head(object)))
    cat("\n")
})

setMethod("dim", signature("giottoBinPoints"), function(x) {
    c(nrow(x@counts), 1L)
})

setMethod("nrow", signature("giottoBinPoints"), function(x) nrow(x@counts))

setMethod("ncol", signature("giottoBinPoints"), function(x) 1L)

setMethod("[", signature(x = "giottoBinPoints", i = "numeric", j = "missing", drop = "missing"), function(x, i, j, ..., compact = "auto", drop) {
    i <- as.integer(i)
    x@counts <- x@counts[i,]
    if (identical(compact, "auto")) compact <- .gbp_compact_auto(x)
    if (!compact) {
        x@compact <- FALSE
        return(x)
    }
    .gbp_compact(x)
})

setMethod("[", signature(x = "giottoBinPoints", i = "logical", j = "missing", drop = "missing"), function(x, i, j, ..., compact = "auto", drop) {
    if (length(i) != nrow(x)) {
        i <- rep(i, length.out = nrow(x))
    }
    i <- which(i)
    x[i, ..., compact = compact]
})

# feature subsetting
setMethod("[", signature(x = "giottoBinPoints", i = "character", j = "missing", drop = "missing"), function(x, i, j, ..., compact = "auto", drop) {
    keep_feat_idx <- which(x@fid %in% i) # i is feature whitelist
    i <- which(x@counts$i %in% keep_feat_idx)
    x[i, ..., compact = compact]
})

setMethod("objName", signature("giottoBinPoints"), function(x) x@feat_type)
setMethod("objName<-", signature("giottoBinPoints", "character"),
    function(x, value) {
    x@feat_type <- value
        x
    })

setMethod("plot", signature("giottoBinPoints", "missing"), function(x,
    point_size = 0.2, feats = NULL, dens = FALSE, dens_transform = NULL, raster = TRUE, raster_size = 600, ...) {
    checkmate::assert_function(dens_transform, null.ok = TRUE)
    if (nrow(x@counts) == 0L) {
        stop(wrap_txt("No geometries to plot"), call. = FALSE)
    }

    # get points to plot
    spatial <- .gbp_get_spatial(x, counts = dens)
    a <- list(x = spatial, ...)
    a$cex <- point_size
    cmap <- "white"
    if (dens) {
        cmap <- NULL # use terra default gradient map
        a$y <- "count"
        if (!is.null(dens_transform)) {
            a$x$count <- dens_transform(a$x$count)
        }
    }
    a$background <- a$background %null% "black"
    a$col <- a$col %null% cmap
    do.call(terra::plot, a)
})

setMethod("calculateOverlap", signature("giottoPolygon", "giottoBinPoints"),
    function(x, y,
        name_overlap = NULL,
        poly_subset_ids = NULL,
        return_gpolygon = TRUE,
        verbose = NULL,
        ...) {
        checkmate::assert_character(poly_subset_ids, null.ok = TRUE)
        if (!is.null(poly_subset_ids)) {
            x <- x[x$poly_ID %in% poly_subset_ids]
        }
        overlap_data <- terra::extract(x[], y@spatial)
        # res <- terra::relate(x[], y@spatial,
        #     relation = "intersects", pairs = TRUE) |>
        #     data.table::as.data.table()
        # names(res) <- c("poly_ID", "b")
        # res <- res[, .gbp_spatial_select_counts(y, b), by = "poly_ID"]

        res <- .gbp_create_overlap_point_dt(x, y, overlap_data)

        if (isTRUE(return_gpolygon)) {
            # update schema metadata in overlap object
            if (is.null(name_overlap)) name_overlap <- objName(y)
            prov(res) <- spatUnit(x)
            spatUnit(res) <- spatUnit(x)
            featType(res) <- name_overlap

            # ensure centroids calculated
            if (is.null(centroids(x))) {
                x <- centroids(x, append_gpolygon = TRUE)
            }

            x@overlaps[[name_overlap]] <- res
            return(x)
        } else {
            return(res)
        }
    })

setMethod("ext", signature("giottoBinPoints"), function(x, ...) {
    ext(x@spatial, ...)
})

setMethod("head", signature("giottoBinPoints"), function(x, n = 6L, ...) {
    n <- min(nrow(x), n)
    x[seq_len(n)]
})

setMethod("tail", signature("giottoBinPoints"), function(x, n = 6L, ...) {
    nr <- nrow(x)
    begin <- nr - n + 1L
    begin <- max(1, begin)
    x[begin:nr]
})

#' @rdname as.data.table
#' @method as.data.table giottoBinPoints
#' @export
as.data.table.giottoBinPoints <- function(x, ...) {
    cn <- colnames(x@counts)
    get_cols <- cn[!cn == "j"]
    d <- x@counts[, get_cols, with = FALSE]
    d$i <- x@fid[d$i]
    names(d)[1:2] <- c("feat_ID", "count")
    d
}


# * constructor ####

createGiottoBinPoints <- function(expr_values, spatial_locs,
    feat_type = "rna") {
    ids_p <- spatial_locs$cell_ID
    spatial_locs[] <- spatial_locs[][, c("sdimx", "sdimy")] # drop point IDs col
    sl_points <- as.points(spatial_locs)

    # get counts information as `d`
    M <- expr_values[]
    if (!inherits(M, "Matrix")) stop("expr_values should be Matrix")
    d <- data.table::as.data.table(Matrix::summary(M))
    ids_m <- colnames(M) |> as.character()

    # keep only existing points geoms so it matches `d` content
    exist_j <- sort(unique(d$j))
    keep_ids <- ids_m[exist_j] # ids that actually exist (character)
    keep_index <- which(ids_p %in% keep_ids) # i integer values for spatial
    if (length(keep_index) == 0L) warning("No bin points exist\n")
    # remove non-existing from spatial info
    ids_p <- ids_p[keep_index]
    sl_points <- sl_points[keep_index]

    x <- new("giottoBinPoints",
        spatial = sl_points,
        counts = d,
        bid = ids_m,
        pmap = match(ids_p, ids_m),
        fid = rownames(M),
        feat_type = feat_type,
        compact = TRUE
    )
    .gbp_compact(x, ids_p = ids_p, exist_j = exist_j)
}

# internals

# pids are mapped 1:1 with spatial row number
.gbp_get_ids_p <- function(x) {
    x@bid[x@pmap]
}

.gbp_get_existing_bid <- function(x, sort = TRUE) {
    existing_j <- unique(x@counts$j)
    if (sort) {
        existing_j <- sort(existing_j)
    }
    x@bid[existing_j]
}

# update and remap values for existing bids only
# also drop any nonexisting spatial points
.gbp_compact <- function(x, ids_p = NULL, exist_j = NULL) {
    if (is.null(exist_j)) {
        exist_j <- sort(unique(x@counts$j))
    }
    exist_ids <- x@bid[exist_j]

    if (is.null(ids_p)) {
        ids_p <- .gbp_get_ids_p(x)
    }

    x@bid <- exist_ids
    x@counts$j <- match(x@counts$j, exist_j)
    ids_p_mapping <- match(ids_p, x@bid)
    spatial_keep_bool <- !is.na(ids_p_mapping)
    x@pmap <- ids_p_mapping[spatial_keep_bool]
    x@spatial <- x@spatial[spatial_keep_bool]
    x@compact <- TRUE
    x
}

# determine whether to compact based on bloat ratio
.gbp_compact_auto <- function(x) {
    bloat_ratio <- length(x@bid) / length(unique(x@counts$j))
    bloat_ratio > 10 # arbitrary setting
}

# get spatial points from GBP (only existing ones)
.gbp_get_spatial <- function(x, counts = FALSE) {
    if (counts) return(.gbp_get_spatial_sum_counts(x))
    if (x@compact) {
        return(x@spatial)
    }
    # these are both mappings of IDs in @bid, so they can be directly compared
    ids_select <- which(x@pmap %in% x@counts$j)
    x@spatial[ids_select]
}
# get spatial + count col with sum of all features per point
.gbp_get_spatial_sum_counts <- function(x) {
    counts <- x@counts[, .("count" = sum(x)), keyby = "j"]
    idx <- match(counts$j, x@pmap)
    # NA in idx should be impossible. `counts` is a subset of `spatial`
    spatial <- x@spatial[idx] # select spatial matching order of counts
    spatial$count <- counts$count # append count information
    spatial
}

# For the ith spatial point(s), get the contained feature counts as a 2 column
# `data.table` of "feat_ID" and "count"
# param i is row number(s) of spatial
.gbp_spatial_select_counts <- function(x, i) {
    i <- as.integer(i)
    if (max(i) > length(x@pmap) || min(i) < 1) {
        stop(sprintf("[giottoBinPoints] point %d does not exist", i),
             call. = FALSE)
    }
    selected_j <- x@pmap[i]
    counts <- x@counts[j %in% selected_j]
    fids <- x@fid
    counts[, "i" := fids[i]]
    counts <- counts[, .("count" = sum(x)), keyby = "i"]
    data.table::setnames(counts, old = "i", new = "feat_ID")
    counts
}

#' @description
#' Internal constructor for point overlap objects backed by `data.table`.
#' Contains all information needed to construct a matrix or [exprObj].
#' Internally represented as a 2+ column `data.table` of `integers` mapped to
#' poly_ID and feat_ID lookup vectors for efficiency.
#' @param x from data (SpatVector) - need just the poly_ID info
#' @param y `giottoBinPoints` object
#' @param overlap_data relationships (data.frame). Expected to be numeric row
#' indices between x and y
#' @param keep additional col(s) in `y` to keep
#' @examples
#' # load test data
#' sl <- GiottoData::loadSubObjectMini("spatLocsObj")
#' m <- GiottoData::loadSubObjectMini("exprObj")
#' gpoly <- GiottoData::loadSubObjectMini("giottoPolygon")
#'
#' # create a giottoBinPoints object
#' gbp <- createGiottoBinPoints(m, sl)
#' # find overlapped points
#' o <- terra::extract(gpoly[], gbp@spatial)
#' odt <- .gbp_create_overlap_point_dt(gpoly, gbp, o)
#'
#' overlapToMatrix(odt, feat_count_column = "count")
#' @noRd
.gbp_create_overlap_point_dt <- function(x, y,
    overlap_data, keep = NULL) {
    poly <- feat_idx <- feat <- feat_id_index <- NULL # NSE vars
    # cleanup input overlap_data
    checkmate::assert_data_frame(overlap_data)
    data.table::setDT(overlap_data)
    cnames <- colnames(overlap_data)
    data.table::setnames(overlap_data,
        old = c(cnames[[2]], cnames[[1]]),
        new = c("poly", "feat_idx") # feat_idx is the idx of gbp@spatial
    )

    # initialize overlap object and needed ids
    odt <- new("overlapPointDT",
        spat_ids = x$poly_ID,
        feat_ids = as.character(y@fid),
        nfeats = nrow(y)
    )

    # make relationships table sparse by removing non-overlapped features
    # these results are indexed by all features, so no need to filter
    # non-overlapped polys
    overlap_data <- overlap_data[!is.na(poly)]
    # change poly to map against spat_ids
    overlap_data[, poly := match(poly, odt@spat_ids)]
    # append a feature index col to counts info in gbp
    y@counts$feat_ID_uniq <- seq_len(nrow(y))
    # add col matched to gbp counts j col in overlap info
    overlap_data[, count_j := y@pmap[feat_idx]]
    # left join counts info into overlap_data.
    overlap_data <- y@counts[overlap_data, on = .(j = count_j)]
    overlap_data[, feat_idx := NULL] # no longer needed.
    overlap_data[, j := NULL] # j is from y@spatial mapping. No longer needed.
    # current expected cols:
    # i (feat name map), x (count), feat_ID_uniq, poly
    data.table::setnames(overlap_data,
        old = c("i", "x", "feat_ID_uniq"),
        new = c("feat_id_index", "count", "feat")
    )

    # extract needed info from y
    keep <- unique(c("feat_id_index", "count", "feat", "poly", keep))
    overlap_data <- overlap_data[, keep, with = FALSE]

    # set indices
    data.table::setkeyv(overlap_data, "feat")
    data.table::setindexv(overlap_data, "poly")
    data.table::setcolorder(overlap_data, c("poly", "feat", "feat_id_index"))
    # add to object
    odt@data <- overlap_data

    odt
}
