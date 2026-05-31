# spatRelate: filter form on giottoSpatial.
#
# `spatRelate()` is the filter-form complement to `relate()` -- returns x
# narrowed to features satisfying the spatial predicate against any feature
# of y, rather than the relation matrix.

gpoly_full <- GiottoData::loadSubObjectMini("giottoPolygon")
gpoints_full <- GiottoData::loadSubObjectMini("giottoPoints")
# Trim to a small subset so tests are quick and the y reference is contained.
gpoly_small <- gpoly_full[1:10]
gpoints_small <- gpoints_full[1:200]


test_that("spatRelate(giottoSpatial, giottoSpatial) returns same class as x", {
    res <- spatRelate(gpoints_small, gpoly_small, relation = "intersects")
    expect_s4_class(res, "giottoPoints")
    res2 <- spatRelate(gpoly_small, gpoly_small, relation = "intersects")
    expect_s4_class(res2, "giottoPolygon")
})


test_that("spatRelate() narrows to features satisfying the predicate", {
    res <- spatRelate(gpoints_small, gpoly_small, relation = "intersects")
    expect_lte(nrow(res), nrow(gpoints_small))
    # The narrowed set should match the unique x indices from relate(pairs)
    pairs <- relate(gpoints_small, gpoly_small,
        relation = "intersects",
        pairs = TRUE, output = "data.table", use_names = FALSE
    )
    expect_equal(nrow(res), length(unique(pairs$x)))
})


test_that("spatRelate() default relation is 'intersects'", {
    res_default <- spatRelate(gpoints_small, gpoly_small)
    res_explicit <- spatRelate(gpoints_small, gpoly_small,
        relation = "intersects")
    expect_equal(nrow(res_default), nrow(res_explicit))
})


test_that("spatRelate() predicate choice affects the narrowing", {
    # `within` and `disjoint` should partition the input on the same y
    n_within <- nrow(spatRelate(gpoints_small, gpoly_small, "within"))
    n_disjoint <- nrow(spatRelate(gpoints_small, gpoly_small, "disjoint"))
    # NB: in general within + disjoint can exceed input n due to boundary
    # cases; assert each is bounded by the input count.
    expect_lte(n_within, nrow(gpoints_small))
    expect_lte(n_disjoint, nrow(gpoints_small))
    # They should report different counts for non-trivial inputs.
    expect_false(identical(n_within, n_disjoint))
})


test_that("spatRelate() with no matches returns an empty giottoSpatial", {
    # Build a small polygon far outside the data extent so nothing matches.
    far_poly <- createGiottoPolygon(
        terra::vect("POLYGON ((1e9 1e9, 1e9 1e9.1, 1e9.1 1e9.1, 1e9.1 1e9, 1e9 1e9))"),
        verbose = FALSE
    )
    res <- spatRelate(gpoints_small, far_poly, relation = "intersects")
    expect_s4_class(res, "giottoPoints")
    expect_equal(nrow(res), 0L)
})
