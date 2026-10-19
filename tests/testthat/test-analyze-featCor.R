test_that("pearson and spearman match cor(), sparse or dense", {
    set.seed(3)
    m <- matrix(rpois(200, 2), nrow = 5,
        dimnames = list(paste0("g", 1:5), NULL))
    expect_equal(analyzeData(m, featCorParam()), cor(t(m)))
    expect_equal(analyzeData(Matrix::Matrix(m, sparse = TRUE), featCorParam()),
        cor(t(m)))
    expect_equal(analyzeData(m, featCorParam("spearman")),
        cor(t(m), method = "spearman"))
})

test_that("a constant feature gets NA, the others are unaffected", {
    set.seed(4)
    m <- rbind(a = rnorm(30), b = rnorm(30), c = rep(2, 30))
    r <- analyzeData(m, featCorParam())
    expect_true(all(is.na(r["c", ])) && all(is.na(r[, "c"])))
    expect_equal(r[c("a", "b"), c("a", "b")], cor(t(m[1:2, ])))
})

test_that("smooth = correlates the smoothed values", {
    set.seed(5)
    m <- Matrix::Matrix(matrix(rpois(120, 2), nrow = 4,
        dimnames = list(paste0("g", 1:4), paste0("c", 1:30))), sparse = TRUE)
    net <- data.frame(from = paste0("c", 1:29), to = paste0("c", 2:30))
    sp <- spatialSmoothParam(smoothing = 0.5)
    expect_equal(
        analyzeData(m, featCorParam(smooth = sp), network = net),
        analyzeData(processData(m, sp, network = net), featCorParam()))
    expect_error(analyzeData(m, featCorParam(smooth = sp)), "network")
})
