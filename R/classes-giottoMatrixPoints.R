createGiottoBinPoints <- function(expr_values, spatial_locs) {
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
        compact = TRUE
    )
    .gbp_compact(x, ids_p = ids_p, exist_j = exist_j)
}

setClass("giottoBinPoints",
    contains = c("featData", "giottoSubobject"),
    slots = list(
        spatial = "ANY", # spatvector etc.
        counts = "data.frame",
        bid = "character", # bin ID (static)
        pmap = "integer", # point IDs by mapping onto bid
        fid = "character",
        compact = "logical" # state of compaction
    ),
    prototype = list(
        compact = TRUE
    )
)

setMethod("show", signature("giottoBinPoints"), function(object) {
    cat(sprintf("An object of class %s\n", class(object)))
    .show_feat(object)
    plist <- c(
        dimensions = toString(dim(object)),
        compact = as.character(object@compact)
    )
    print_list(plist)
    preview <- head(object@counts)[, c("i", "x")]
    preview$i <- object@fid[preview$i]
    names(preview) <- c("feat_ID", "count")
    print(preview)
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

setMethod("relate", signature(x = "giottoSpatial", y = "giottoBinPoints"),
    function(x, y, relation,
        pairs = TRUE,
        na.rm = TRUE,
        output = c("data.table", "matrix"),
        use_names = TRUE,
        ...
    ) {
        output <- match.arg(output, choices = c("data.table", "matrix"))
})



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

.gbp_get_spatial <- function(x, counts = FALSE) {
    if (counts) return(.gbp_get_spatial_sum_counts(x))
    if (x@compact) {
        return(x@spatial)
    }
    # these are both mappings of @bid, so they can be directly compared
    ids_select <- which(x@pmap %in% x@counts$j)
    x@spatial[ids_select]
}

.gbp_get_spatial_sum_counts <- function(x) {
    counts <- x@counts[, .("count" = sum(x)), keyby = "j"]
    idx <- match(counts$j, x@pmap)
    # NA in idx should be impossible. `counts` is a subset of `spatial`
    spatial <- x@spatial[idx] # select spatial matching order of counts
    spatial$count <- counts$count # append count information
    spatial
}

# `i` is row number(s) of spatial
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


# .plot_giotto_points_raster <- function(data, feats = NULL, ...) {
#     args_list <- list(...)
#
#     # raster size
#     if (is.null(args_list$size)) {
#         args_list$size <- c(600, 600)
#     } else if (length(args_list$size) == 1L) {
#         # if size provided as single value, replicate to give a square window
#         args_list$size <- rep(args_list$size, 2L)
#     }
#
#     # axis font size
#     if (is.null(args_list$cex.axis)) args_list$cex.axis <- 0.7
#
#     args_list$ann <- FALSE
#
#     if (is.null(feats)) {
#         include_values <- FALSE
#     } else {
#         include_values <- TRUE
#     }
#
#     dataDT <- data.table::as.data.table(
#         x = data,
#         geom = "XY",
#         include_values = include_values
#     )
#
#
#     if (length(feats) == 0L) {
#         do.call(.plot_giotto_points_all, args = c(list(x = data), args_list))
#     } else if (length(feats) == 1L) {
#         .plot_giotto_points_one(
#             dataDT = dataDT,
#             feats = feats,
#             args_list = args_list
#         )
#     } else {
#         .plot_giotto_points_several(
#             dataDT = dataDT,
#             feats = feats,
#             args_list = args_list
#         )
#     }
# }