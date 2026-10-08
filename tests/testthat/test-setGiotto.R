test_that("setGiotto(list) restores init options when an item fails", {
    op <- options(giotto.init = TRUE, giotto.check_valid = TRUE)
    on.exit(options(op), add = TRUE)

    g <- GiottoData::loadGiottoMini("visium", verbose = FALSE)
    ex <- getExpression(g)
    expect_error(setGiotto(g, list(ex, "not a subobject"), verbose = FALSE))

    expect_true(getOption("giotto.init"))
    expect_true(getOption("giotto.check_valid"))
})
