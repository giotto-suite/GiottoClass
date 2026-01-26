
# * definitions ####

#' @title Binned point class
#' @name giottoBinPoints-class
#' @exportClass giottoBinPoints
#' @description
#' S4 class allowing point detection-like access patterns for binned spatial
#' values. Implemented more efficiently by only representing the spatial points
#' once and mapping the sparse values information against the points.
#' @slot spatial ANY (currently `SpatVector` only). Row-indexed spatial points
#' @slot counts `data.table` with integer cols `i` and `j` mapping to `@fid`
#' and `@bid` respectively. `x` is a numeric col standing for count or value of
#' a feature for this bin point. Row indexing of this object is based on this
#' slot.
#' @slot bid `character`. Bin IDs to map against
#' @slot pmap `integer`. For each spatial point, gives the index into `@bid`.
#' Length equals `length(spatial)`. Forms a bridge between spatial and counts:
#' both `pmap` and `counts$j` are indices into `@bid`. **Invariant**: every bin
#' ID in `counts$j` must appear in `pmap` (i.e., counts is always a subset of
#' spatial). Allows subsetting `counts` without modifying the more expensive
#' `spatial` representation until compaction.
#' @slot fid `character`. Feature IDs to map against
#' @slot compact `logical`. State of compaction. When `TRUE`, `@bid`, `@pmap`,
#' and `@spatial` contain only bins that appear in `@counts` (bidirectional
#' relationship). When `FALSE`, they may contain bins not present in `@counts`
#' (unidirectional: every bin in counts has spatial, but not every spatial has
#' counts).
#' @export
setClass("giottoBinPoints",
    contains = c("featData", "giottoSubobject"),
    slots = list(
        spatial = "ANY", # spatvector etc.
        counts = "data.table",
        bid = "character", # bin ID (static)
        pmap = "integer", # point IDs by mapping onto bid
        fid = "character", # feature ID
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
    c(nrow(x@counts), 2L)
})

setMethod("nrow", signature("giottoBinPoints"), function(x) nrow(x@counts))

setMethod("ncol", signature("giottoBinPoints"), function(x) 1L)

setMethod("[", signature(x = "giottoBinPoints", i = "numeric", j = "missing", drop = "missing"), function(x, i, j, compact = "auto", ...,  drop) {
    i <- as.integer(i)
    x@counts <- x@counts[i,]
    if (identical(compact, "auto")) compact <- .gbp_compact_auto(x)
    if (!compact) {
        x@compact <- FALSE
        return(x)
    }
    .gbp_compact(x)
})

#' @rdname subset_bracket
#' @param compact `character` or `logical` (default = "auto"). Whether to
#' compact object. See [giottoBinPoints-class]. `"auto"` will perform a
#' compaction when number of spatial points referenced in `@counts` is 1/10 of
#' that existing in `@spatial`
#' @export
setMethod("[", signature(x = "giottoBinPoints", i = "logical", j = "missing", drop = "missing"), function(x, i, j, compact = "auto", ..., drop) {
    if (length(i) != nrow(x)) {
        i <- rep(i, length.out = nrow(x))
    }
    i <- which(i)
    x[i, ..., compact = compact]
})

# feature subsetting
setMethod("[", signature(x = "giottoBinPoints", i = "character", j = "missing", drop = "missing"), function(x, i, j, compact = "auto", ..., drop) {
    keep_feat_idx <- which(x@fid %in% i) # i is feature whitelist
    i <- which(x@counts$i %in% keep_feat_idx)
    x[i, ..., compact = compact]
})

setMethod("objName", signature("giottoBinPoints"), function(x) x@feat_type)
setMethod("objName<-", signature("giottoBinPoints", "ANY"),
    function(x, value) {
    x@feat_type <- as.character(value)
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

#' @rdname crop
#' @param ext `logical`. When `TRUE`, the extent of y will be used instead of y
#' @param compact `character` or `logical` (default = "auto"). Whether to
#' compact object. See [giottoBinPoints-class]. `"auto"` will perform a
#' compaction when number of spatial points referenced in `@counts` is 1/10 of
#' that existing in `@spatial`
#' @export
setMethod("crop", signature("giottoBinPoints", "ANY"), function(x, y,
    ext = FALSE, compact = "auto", ...) {
    if (inherits(y, "SpatExtent")) ext <- TRUE
    if (isTRUE(ext)) {
        y <- terra::as.polygons(ext(y))
    } else {
        y <- y[]
        if (!inherits(y, "SpatVector")) {
            stop("[crop] y of class ", class(y)[[1L]], " not supported.\n",
                 call. = FALSE)
        }
    }
    crop(x, y, ext = FALSE, compact = compact, ...)
})
setMethod("crop", signature("giottoBinPoints", "SpatVector"), function(x, y,
    ext = FALSE, compact = "auto", ...) {
    if (isTRUE(ext)) {
        return(crop(x, y = terra::as.polygons(ext(y)), compact = compact, ...))
    }
    keep_spat_idx <- which(terra::relate(x@spatial, y, relation = "intersects"))
    keep_j <- x@pmap[keep_spat_idx]
    keep_count_idx <- which(x@counts$j %in% keep_j)
    x[keep_count_idx, compact = compact]
})

#' @title Head and tail
#' @name headtail
#' @description
#' Get the head (first values) or tail (last values) of an object
#' @param x object
#' @param n `integerlike` how many to get
#' @param ... additional arguments to pass to other methods
#' @returns the same class as `x`
#' @export
setMethod("head", signature("giottoBinPoints"), function(x, n = 6L, ...) {
    n <- min(nrow(x), n)
    x[seq_len(n)]
})

#' @rdname headtail
#' @export
setMethod("tail", signature("giottoBinPoints"), function(x, n = 6L, ...) {
    nr <- nrow(x)
    begin <- nr - n + 1L
    begin <- max(1, begin)
    x[begin:nr]
})

#' @rdname setGiotto
#' @export
setMethod("setGiotto", signature("giotto", "giottoBinPoints"),
    function(gobject, x, ...) {
        gobject <- setFeatureInfo(gobject = gobject, x = x, ...)
        gobject
})

#' @rdname spatIDs-generic
#' @export
setMethod("featIDs", signature(x = "giottoBinPoints"), function(x, uniques = TRUE, ...) {
    if (uniques) {
        return(x@fid[unique(x@counts$i)])
    }
    x@fid[x@counts$i]
})

#' @rdname as.data.table
#' @method as.data.table giottoBinPoints
#' @export
as.data.table.giottoBinPoints <- function(x, geom, ...) {
    d <- data.table::copy(x@counts)
    data.table::setnames(d, old = c("i", "x"), new = c("feat_ID", "count"))
    if (!missing(geom)) {
        sv <- x@spatial[match(d$j, x@pmap),]
        geom_info <- data.table::as.data.table(sv, geom = geom)
        d <- cbind(d, geom_info)
    }

    cn <- colnames(d)
    get_cols <- cn[!cn == "j"]
    d <- d[, get_cols, with = FALSE]
    d$feat_ID <- x@fid[d$feat_ID]
    d
}


# * constructor ####

#' @describeIn giottoBinPoints constructor function
#' @param expr_values `exprObj` Bin counts/values
#' @param spatial_locs `spatLocsObj` Spatial locations of bins
#' @param feat_type `character` (default = "rna"). Feature type of the data
#' @examples
#' ids <- sprintf("bin_%d", 1:50)
#' sl <- createSpatLocsObj(rnorm(100))
#' sl$cell_ID <- ids
#' m <- matrix(floor(runif(500) * 3),
#'     ncol = 50,
#'     dimnames = list(letters[1:10], ids)
#' )
#' ex <- createExprObj(m)
#' gbp <- createGiottoBinPoints(ex, sl)
#'
#' # basics -------------------------------------------------------- #
#' force(gbp)
#' nrow(gbp)
#' dim(gbp)
#' data.table::as.data.table(gbp)
#' head(gbp)
#' tail(gbp)
#' objName(gbp)
#' featType(gbp)
#'
#' # subsetting ---------------------------------------------------- #
#' gbp[50:100]
#' gbp["a"] # get only points for feature "a"
#' gbp[letters[1:4]] # get only points for features "a", "b", "c", "d"
#'
#' # plotting ------------------------------------------------------ #
#' plot(gbp, dens = TRUE) # will take a long time on large datasets
#' plot(gbp["a"]) # plot feature "a" only
#' plot(gbp[c("a", "d")]) # plot features "a" and "d" together
#'
#' # spatial ------------------------------------------------------- #
#' ext(gbp) # spatial extent
#'
#' d <- Giotto::hexVertices(1)
#' d$poly_ID <- "a"
#' hex <- createGiottoPolygon(d)
#' plot(gbp, col = "blue")
#' plot(hex, add = TRUE, border = "red")
#' plot(crop(gbp, hex), add = TRUE, col = "green") # cropping
#'
#' hex2 <- tessellate(ext(gbp), shape_size = 1)
#' res <- calculateOverlap(hex2, gbp) # overlapped feature calculation
#' m <- overlapToMatrix(res) # overlap info to expression matrix
#' force(m)
#' @export
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
    if (nrow(x@counts) == 0L) return(TRUE) # no data left anyways
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
