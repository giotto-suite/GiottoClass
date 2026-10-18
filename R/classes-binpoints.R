
# * definitions ####

#' @title Binned point class
#' @name giottoBinPoints-class
#' @exportClass giottoBinPoints
#' @description
#' S4 class allowing point detection-like access patterns for binned spatial
#' values. Implemented more efficiently by only representing the spatial points
#' once and mapping the sparse values information against the points.
#' @slot spatial ANY. Row-indexed spatial points: a `SpatVector`, or on a
#' backed object a GiottoDisk points store keyed by the bin's row in `@counts`
#' @slot counts `data.table` with integer cols `i` and `j` mapping to `@fid`
#' and `@bid` respectively. `x` is a numeric col standing for count or value of
#' a feature for this bin point. Row indexing of this object is based on this
#' slot. On a backed object, a GiottoDisk expression store of features x bins,
#' with `@bid` and `@pmap` left empty since its cell IDs are the bin IDs.
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
        counts = "ANY", # data.table, or a store on a backed object
        bid = "character", # bin ID (static)
        pmap = "integer", # point IDs by mapping onto bid
        fid = "character", # feature ID
        compact = "logical" # state of compaction
    ),
    prototype = list(
        counts = data.table::data.table(),
        compact = TRUE
    )
)

# Points and counts held as GiottoDisk stores (backed object) rather than a
# SpatVector and a data.table
.gbp_is_backed <- function(x) {
    !is.null(x@spatial) && !inherits(x@spatial, "SpatVector")
}

.gbp_backed_stop <- function(what) {
    stop(sprintf("[giottoBinPoints] %s is not supported for store-backed bin points",
        what), call. = FALSE)
}

# * methods ####
setMethod("show", signature("giottoBinPoints"), function(object) {
    cat(sprintf("An object of class %s\n", class(object)))
    .show_feat(object)
    if (.gbp_is_backed(object)) {
        print_list(c(bins = format(nrow(object), big.mark = ","),
            features = format(length(object@fid), big.mark = ",")))
        print(object@spatial)
        return(invisible())
    }
    plist <- c(
        dimensions = toString(dim(object)),
        compact = as.character(object@compact)
    )
    print_list(plist)
    print(as.data.table(head(object)))
    cat("\n")
})

# rows are count records in memory; for store-backed bin points, the bins
setMethod("dim", signature("giottoBinPoints"), function(x) {
    c(nrow(x), 2L)
})

setMethod("nrow", signature("giottoBinPoints"), function(x) {
    if (.gbp_is_backed(x)) return(nrow(x@spatial))
    nrow(x@counts)
})

setMethod("ncol", signature("giottoBinPoints"), function(x) 1L)

setMethod("[", signature(x = "giottoBinPoints", i = "numeric", j = "missing", drop = "missing"), function(x, i, j, compact = "auto", ...,  drop) {
    if (.gbp_is_backed(x)) .gbp_backed_stop("row indexing")
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
    if (.gbp_is_backed(x)) {
        x@counts <- x@counts[intersect(i, x@fid), ]
        x@fid <- rownames(x@counts)
        return(x)
    }
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
    if (.gbp_is_backed(x)) return(plot(x@spatial, ...))
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

        if (.gbp_is_backed(y)) {
            # store-backed bins: GiottoDisk overlaps the polygons with the
            # bins and keeps their counts for overlapToMatrix()
            res <- calculateOverlap(x[], y, ...)
        } else {
            # On a project-managed giotto the polygons are store-backed, and
            # terra::extract() has no method for a store. Materialize them
            # into a working copy for the terra call: segmentation polygon
            # counts are modest (~10^4-10^5) next to the bin1 points they are
            # overlapped with (~10^6-10^7), which stay untouched in `y`. `x`
            # itself is left alone so the object handed back keeps its store.
            px <- x
            if (!inherits(px[], "SpatVector")) {
                GiottoUtils::package_check(
                    "GiottoDisk",
                    repository = "github:giotto-suite/GiottoDisk"
                )
                px[] <- GiottoDisk::storeRead(px[], output = "terra")
            }
            overlap_data <- terra::extract(px[], y@spatial)
            # res <- terra::relate(x[], y@spatial,
            #     relation = "intersects", pairs = TRUE) |>
            #     data.table::as.data.table()
            # names(res) <- c("poly_ID", "b")
            # res <- res[, .gbp_spatial_select_counts(y, b), by = "poly_ID"]

            # built from the materialized copy: it reads `poly_ID` off the
            # payload, which terra provides and a store need not
            res <- .gbp_create_overlap_point_dt(px, y, overlap_data)
        }

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
    if (.gbp_is_backed(x)) {
        x@spatial <- crop(x@spatial, if (isTRUE(ext)) ext(y) else y[], ...)
        return(x)
    }
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
    if (.gbp_is_backed(x)) {
        x@spatial <- crop(x@spatial, y, ...)
        return(x)
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
#' @exportS3Method utils::head
head.giottoBinPoints <- function(x, n = 6L, ...) {
    n <- min(nrow(x), n)
    x[seq_len(n)]
}

#' @rdname headtail
#' @exportS3Method utils::tail
tail.giottoBinPoints <- function(x, n = 6L, ...) {
    nr <- nrow(x)
    begin <- nr - n + 1L
    begin <- max(1, begin)
    x[begin:nr]
}

#' @rdname setGiotto
#' @export
setMethod("setGiotto", signature("gAny", "giottoBinPoints"),
    function(gobject, x, ...) {
        gobject <- setFeatureInfo(gobject = gobject, x = x, ...)
        gobject
})

#' @rdname spatIDs-generic
#' @export
setMethod("featIDs", signature(x = "giottoBinPoints"), function(x, uniques = TRUE, ...) {
    if (.gbp_is_backed(x)) return(x@fid)
    if (uniques) {
        return(x@fid[unique(x@counts$i)])
    }
    x@fid[x@counts$i]
})

#' @rdname rbind-generic
#' @export
setMethod("rbind2", signature("giottoBinPoints", "giottoBinPoints"),
    function(x, y, ...) {
        if (.gbp_is_backed(x) || .gbp_is_backed(y)) .gbp_backed_stop("rbind")
        # 1. force compaction on both
        if (isFALSE(x@compact)) x <- .gbp_compact(x)
        if (isFALSE(y@compact)) y <- .gbp_compact(y)

        # 2. check conflicting bin IDs
        conflicts <- y@bid %in% x@bid

        if (any(conflicts)) {

            warning("[rbind] giottoBinPoints have the same bin IDs.",
                    "\n One set will be renamed.")

            conflict_ids <- y@bid[conflicts]
            # Rename conflicting IDs in y
            # pmap and counts$j are integer indices, so they stay valid
            suffix_num <- 1
            repeat {
                candidate_names <- paste0(y@bid[conflicts], "_", suffix_num)
                # Check against both x@bid and non-conflict y@bid
                if (!any(candidate_names %in% c(x@bid, y@bid[!conflicts]))) {
                    y@bid[conflicts] <- candidate_names
                    break
                }
                suffix_num <- suffix_num + 1
                if (suffix_num > 1000) {
                    stop("Could not generate unique bin IDs after 1000 attempts")
                }
            }
        }

        # 3. Concatenation (no conflicts now)
        x_counts <- data.table::copy(x@counts)
        y_counts <- data.table::copy(y@counts)

        offset <- length(x@bid)
        y_counts[, j := j + offset]

        # Handle feature IDs - union and remap
        fid_x <- x@fid
        fid_y <- y@fid
        all_fid <- union(fid_x, fid_y)
        x_counts[, i := match(fid_x[i], all_fid)]
        y_counts[, i := match(fid_y[i], all_fid)]

        new("giottoBinPoints",
            spatial = rbind(x@spatial, y@spatial),
            counts = rbind(x_counts, y_counts),
            bid = c(x@bid, y@bid),
            pmap = c(x@pmap, y@pmap + offset),
            fid = all_fid,
            feat_type = x@feat_type,  # take from x
            compact = TRUE
        )
    })

#' @rdname as.data.table
#' @method as.data.table giottoBinPoints
#' @export
as.data.table.giottoBinPoints <- function(x, geom, ...) {
    if (.gbp_is_backed(x)) {
        GiottoUtils::package_check(
            "GiottoDisk",
            repository = "github:giotto-suite/GiottoDisk"
        )
        return(GiottoDisk::binPointsTable(x,
            geom = if (missing(geom)) "" else geom))
    }
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
.gbp_create_overlap_point_dt <- function(x, y, overlap_data) {
    poly <- feat_idx <- count_j <- NULL # NSE vars
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

    # Look up each bin's polygon and index the counts rows directly, instead
    # of copying and joining the whole counts table. Only bins lying in more
    # than one polygon (shared borders) go through a join.
    multi_idx <- overlap_data$feat_idx[duplicated(overlap_data$feat_idx)]
    in_multi <- overlap_data$feat_idx %in% multi_idx
    single <- overlap_data[!in_multi]
    bin_poly <- integer(length(y@bid))
    bin_poly[y@pmap[single$feat_idx]] <- single$poly
    rows <- which(bin_poly[y@counts$j] > 0L)
    mres <- NULL
    if (any(in_multi)) {
        multi <- overlap_data[in_multi]
        multi[, count_j := y@pmap[feat_idx]]
        mrows <- which(y@counts$j %in% multi$count_j)
        mc <- data.table::data.table(feat = mrows, j = y@counts$j[mrows],
            feat_id_index = y@counts$i[mrows], count = y@counts$x[mrows])
        mres <- mc[multi[, list(j = count_j, poly)], on = "j",
            allow.cartesian = TRUE, nomatch = NULL]
    }
    # allocated once at the final length and filled, so appending the
    # shared-bin rows does not copy the table
    n1 <- length(rows)
    n <- n1 + NROW(mres)
    res <- data.table::setDT(list(poly = integer(n), feat = integer(n),
        feat_id_index = integer(n), count = numeric(n)))
    at <- seq_len(n1)
    data.table::set(res, at, "poly", bin_poly[y@counts$j[rows]])
    data.table::set(res, at, "feat", rows)
    data.table::set(res, at, "feat_id_index", y@counts$i[rows])
    data.table::set(res, at, "count", y@counts$x[rows])
    rm(bin_poly, rows, at)
    if (!is.null(mres) && nrow(mres)) {
        at <- n1 + seq_len(nrow(mres))
        for (cn in c("poly", "feat", "feat_id_index", "count")) {
            data.table::set(res, at, cn, mres[[cn]])
        }
    }

    # set indices
    data.table::setkeyv(res, "feat")
    data.table::setindexv(res, "poly")
    # add to object
    odt@data <- res

    odt
}
