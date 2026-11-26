test_that("an envname resolves without conda unless it must exist", {
    no_conda <- function(conda = "auto", error = TRUE) {
        if (isTRUE(error)) stop("Unable to find a conda binary.")
        FALSE
    }
    testthat::local_mocked_bindings(.check_conda = no_conda)
    expect_identical(.envname_to_pypath("giotto_env", must_exist = FALSE),
        "giotto_env")
    expect_error(.envname_to_pypath("giotto_env", must_exist = TRUE),
        "conda binary")
})
