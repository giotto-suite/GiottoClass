# Assigning bins to polygons: "inside" (the bin's point) and "area" (the
# bin's square), the bin geometry slots, and binOverlaps().

.sq <- function(xmin, xmax, ymin, ymax, id) {
    v <- terra::vect(sprintf(
        "POLYGON ((%1$s %3$s, %2$s %3$s, %2$s %4$s, %1$s %4$s, %1$s %3$s))",
        xmin, xmax, ymin, ymax))
    v$poly_ID <- id
    v
}

.grid_bins <- function(n = 10, ...) {
    xy <- expand.grid(x = seq_len(n) - 0.5, y = seq_len(n) - 0.5)
    ids <- sprintf("bin%03d", seq_len(nrow(xy)))
    m <- Matrix::sparseMatrix(i = rep(1:2, length.out = nrow(xy)),
        j = seq_len(nrow(xy)), x = 1, dims = c(2L, nrow(xy)),
        dimnames = list(c("g1", "g2"), ids))
    sl <- data.table::data.table(sdimx = xy$x, sdimy = xy$y, cell_ID = ids)
    createGiottoBinPoints(createExprObj(m), createSpatLocsObj(sl), ...)
}

test_that("inside is terra::extract()", {
    polys <- rbind(.sq(0, 3, 0, 3, "a"), .sq(2, 5, 2, 5, "b"))
    bins <- terra::vect(cbind(c(0.5, 2.5, 4.5, 7), c(0.5, 2.5, 4.5, 7)),
        type = "points")
    expect_identical(extract_bin_overlap(polys, bins),
        terra::extract(polys, bins))
})

test_that("area gives a bin to the polygon covering it", {
    # polygon "b" comes first, so poly_ID order and row order differ
    polys <- rbind(.sq(3, 6, 0, 3, "b"), .sq(0, 3, 0, 3, "a"))
    # unit bins centred on their points
    bins <- terra::vect(cbind(c(1.5, 3.2, 2.9, 6.4, 6.6, 3.0), 1.5),
        type = "points")
    got <- extract_bin_overlap(polys, bins, "area", bin_width = 1)
    expect_identical(got$id.y, 1:6)
    expect_identical(got$poly_ID, c(
        "a", # inside a
        "b", # centre in b
        "a", # centre in a, square also in b
        "b", # outside both, square reaches 0.1 into b
        NA,  # square ends on b's edge: edge contact is no hit
        "a"  # centre on the shared edge: equal halves, lowest poly_ID
    ))
    off <- extract_bin_overlap(polys,
        terra::vect(cbind(6.1, 1.5), type = "points"),
        "area", bin_width = 1, bin_offset = c(-0.5, 0))
    expect_identical(off$poly_ID, "b")
    expect_error(extract_bin_overlap(polys, bins, "area"), "bin_width")
})

test_that("bin points carry their geometry and calculateOverlap() uses it", {
    gp <- createGiottoPolygon(rbind(.sq(0.6, 4.6, 0.6, 4.6, "a"),
        .sq(4.6, 8.6, 0.6, 4.6, "b")))
    plain <- .grid_bins()
    expect_identical(plain@bin_overlap, "inside")
    expect_true(is.na(plain@bin_width))
    inside <- overlapToMatrix(overlaps(calculateOverlap(gp, plain), "rna"))
    expect_equal(sum(inside[, "a"]), 16)
    expect_error(calculateOverlap(gp, plain, bin_overlap = "area"),
        "bin_width")

    area <- .grid_bins(bin_width = 1, bin_overlap = "area")
    m <- overlapToMatrix(overlaps(calculateOverlap(gp, area), "rna"))
    # the edges cut bins: a takes 5 x 5, and the column the shared edge cuts
    # (0.6 of each bin in a); b the remaining 3 x 5; every bin once
    expect_equal(sum(m[, "a"]), 25)
    expect_equal(sum(m[, "b"]), 20)
    expect_equal(sum(m), 45)
    # a call overrides the bin points' rule
    m2 <- overlapToMatrix(overlaps(
        calculateOverlap(gp, area, bin_overlap = "inside"), "rna"))
    expect_identical(as.matrix(m2), as.matrix(inside))
    # the geometry survives subsetting and rbind
    expect_identical(area[1:5]@bin_width, 1)
    other <- area
    other@bid <- paste0("x", other@bid)
    expect_identical(rbind(area, other)@bin_overlap, "area")
})

test_that("binOverlaps() and bin ids name each bin's polygon", {
    gp <- createGiottoPolygon(.sq(0, 2, 0, 2, "a"))
    bp <- .grid_bins(4)
    ov <- binOverlaps(calculateOverlap(gp, bp), bp)
    expect_identical(names(ov), c("poly_ID", "bin_ID"))
    expect_setequal(ov$bin_ID, c("bin001", "bin002", "bin005", "bin006"))
    expect_true(all(ov$poly_ID == "a"))
    expect_error(binOverlaps(gp, bp), "calculateOverlap")
    dt <- data.table::as.data.table(bp, bin_id = TRUE)
    expect_identical(sort(unique(dt$bin_ID)), sort(bp@bid))
    expect_identical(names(data.table::as.data.table(bp)),
        c("feat_ID", "count"))
})

test_that("objects saved before the geometry slots update to the defaults", {
    g <- giotto()
    g <- setGiotto(g, .grid_bins(3), verbose = FALSE)
    # an old object: no geometry attributes on its bin points
    fi <- attr(g, "feat_info")
    for (s in c("bin_width", "bin_offset", "bin_overlap")) {
        attr(fi[[1L]], s) <- NULL
    }
    attr(g, "feat_info") <- fi
    g@versions$gclass <- "0.7.26"
    bp <- updateGiottoObject(g)@feat_info[[1L]]
    expect_true(is.na(bp@bin_width))
    expect_identical(bp@bin_offset, c(0, 0))
    expect_identical(bp@bin_overlap, "inside")
})
