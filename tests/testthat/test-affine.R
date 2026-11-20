xy <- data.table::data.table(
    cell_ID = c("a", "b", "c"),
    sdimx = c(0, 3, -2), sdimy = c(0, 1, 5)
)
sl <- createSpatLocsObj(xy, name = "raw", verbose = FALSE)
crd <- function(x) unname(as.matrix(x[][, c("sdimx", "sdimy")]))
pts <- unname(as.matrix(xy[, c("sdimx", "sdimy")]))

# a non-symmetric linear block, so a transposed reading is visible
m <- matrix(c(
    2, 0.5, 10,
    -0.3, 1.5, -20,
    0, 0, 1
), nrow = 3, byrow = TRUE)
expected <- t(m[1:2, 1:2] %*% t(pts) + m[1:2, 3])

test_that("affine() pre-multiplies column vectors by default", {
    expect_equal(crd(affine(sl, m)), expected)
    expect_equal(crd(affine(affine(sl, m), m, inv = TRUE)), pts)
})

test_that("pre_multiply = FALSE reads the linear block transposed", {
    m_post <- m
    m_post[1:2, 1:2] <- t(m[1:2, 1:2])
    expect_equal(crd(affine(sl, m_post, pre_multiply = FALSE)), expected)
})

test_that("affine2d stores the matrix it applies", {
    a <- affine(m)
    expect_equal(a[], m)
    expect_equal(crd(affine(sl, a)), expected)

    # spin() turns clockwise for positive angles
    s <- spin(affine(), 90, x0 = 0, y0 = 0)
    expect_equal(s[][1:2, 1:2], matrix(c(0, -1, 1, 0), 2L), tolerance = 1e-12)
    expect_equal(crd(affine(sl, s)), crd(spin(sl, 90, x0 = 0, y0 = 0)))
})

test_that("affine2d composes in the order the ops are applied", {
    a <- affine() |>
        spin(30, x0 = 0, y0 = 0) |>
        shear(fx = 0.2, fy = 0.1, x0 = 0, y0 = 0) |>
        rescale(fx = 2, fy = 0.5, x0 = 0, y0 = 0) |>
        flip() |>
        spatShift(dx = 5, dy = -6) |>
        affine(m)
    geo <- sl |>
        spin(30, x0 = 0, y0 = 0) |>
        shear(fx = 0.2, fy = 0.1, x0 = 0, y0 = 0) |>
        rescale(fx = 2, fy = 0.5, x0 = 0, y0 = 0) |>
        flip() |>
        spatShift(dx = 5, dy = -6) |>
        affine(m)
    expect_equal(crd(affine(sl, a)), crd(geo))
})

test_that("a space's composite affine is a pre-multiply matrix", {
    sp <- .space_record(perSampleSpace(name = "s"), "affine", list(y = m))
    expect_equal(.space_composite_affine(sp), m)
})

test_that("objects saved before 0.8.0 keep their placement", {
    r <- terra::rast(matrix(1:12, nrow = 3), extent = terra::ext(0, 4, 0, 3))
    img <- spin(createGiottoLargeImage(r, name = "img", verbose = FALSE), 30)
    old_layout <- function(a) {
        a@affine[1:2, 1:2] <- t(a@affine[1:2, 1:2])
        a
    }

    g <- giotto()
    g@images <- list(img = img)
    g@images$img@affine <- old_layout(img@affine)
    # loadGiotto() initializes images before the object is updated
    g@images$img <- initialize(g@images$img)
    sp <- perSampleSpace(name = "s")
    sp@steps <- list(
        .space_step_transform("affine", list(y = old_layout(affine(m))[])),
        .space_step_transform("affine",
            list(y = m, pre_multiply = TRUE)),
        .space_step_transform("affine", list(y = old_layout(affine(m))))
    )
    g@spaces <- list(s = sp)
    g@versions$gclass <- package_version("0.7.31")

    g <- updateGiottoObject(g)
    expect_equal(g@images$img@affine[], img@affine[])
    expect_equal(as.vector(ext(g@images$img)), as.vector(ext(img)))
    steps <- g@spaces$s@steps
    expect_equal(steps[[1]]$args$y, m)
    expect_equal(steps[[2]]$args$y, m)
    expect_equal(steps[[3]]$args$y[], m)

    # migrated once: the version is current afterwards
    expect_identical(updateGiottoObject(g)@images$img@affine[], img@affine[])
})

test_that("a giottoMulti saved before 0.8.0 updates its children and spaces", {
    mk <- function() {
        m0 <- matrix(0, nrow = 2, ncol = 3,
            dimnames = list(c("f1", "f2"), c("c1", "c2", "c3")))
        createGiottoObject(expression = m0, verbose = FALSE)
    }
    mg <- createGiottoMulti(list(a = mk(), b = mk()))
    m_old <- m
    m_old[1:2, 1:2] <- t(m[1:2, 1:2])
    sp <- perSampleSpace(name = "s")
    sp@steps <- list(.space_step_transform("affine", list(y = m_old)))
    mg@spaces <- list(s = sp)
    mg@objects$a@spaces <- list(s = sp)
    mg@versions$gclass <- package_version("0.7.31")
    mg@objects$a@versions$gclass <- package_version("0.7.31")

    mg <- initialize(mg)
    expect_equal(mg@spaces$s@steps[[1]]$args$y, m)
    expect_equal(mg@objects$a@spaces$s@steps[[1]]$args$y, m)
    expect_identical(mg@objects$a@versions$gclass,
        packageVersion("GiottoClass"))
    # and only once
    expect_equal(initialize(mg)@spaces$s@steps[[1]]$args$y, m)
})
