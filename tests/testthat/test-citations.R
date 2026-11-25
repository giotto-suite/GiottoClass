setClass("citeTestParam", contains = "analyzeParam")
setMethod("methodCitation", "citeTestParam", function(x, ...) {
    utils::bibentry("Article",
        title = "A toy method", author = "Ann Author",
        journal = "Journal of Toys", year = 2024
    )
})
setClass("noCiteTestParam", contains = "analyzeParam")

cite_fun <- function(gobject, param) {
    update_giotto_params(gobject, description = "_cite_test",
        attachments = list(param = param))
}

reset_shown <- function() {
    rm(list = ls(GiottoClass:::.cite_shown), envir = GiottoClass:::.cite_shown)
}

test_that("methodCitation() is NULL for params without a citation", {
    expect_null(methodCitation(new("noCiteTestParam")))
    expect_s3_class(methodCitation(new("citeTestParam")), "bibentry")
})

test_that("a cited param is recorded with its step and shown once", {
    reset_shown()
    g <- giotto()
    expect_message(g <- cite_fun(g, new("citeTestParam")), "Journal of Toys")
    expect_no_message(g <- cite_fun(g, new("citeTestParam")))

    cites <- objCitations(g)
    expect_length(cites, 1L)
    expect_match(cites, "A toy method")
    expect_output(print(cites), "Please cite")
})

test_that("steps without a citation record none", {
    g <- cite_fun(giotto(), new("noCiteTestParam"))
    expect_null(objHistory(g)[[1]]$cite)
    expect_length(objCitations(g), 0L)
    expect_output(print(objCitations(g)), "No citations")
})

test_that("citations survive the NDJSON history", {
    old <- options(giotto.cite = FALSE)
    on.exit(options(old), add = TRUE)
    g <- cite_fun(giotto(), new("citeTestParam"))
    f <- tempfile(fileext = ".ndjson")
    on.exit(unlink(f), add = TRUE)
    objHistory(g, file = f)
    expect_identical(unclass(objCitations(f)), unclass(objCitations(g)))
})

test_that("giotto.cite = FALSE silences the message", {
    reset_shown()
    old <- options(giotto.cite = FALSE)
    on.exit(options(old), add = TRUE)
    expect_no_message(cite_fun(giotto(), new("citeTestParam")))
})

test_that("citeMethod() takes a bibentry", {
    reset_shown()
    b <- methodCitation(new("citeTestParam"))
    expect_message(citeMethod(b, what = "toy()"), "If you use toy\\(\\)")
    expect_no_message(citeMethod(b, what = "toy()"))
})

test_that("citations are one line of plain text", {
    b <- utils::bibentry("Article",
        author = c(person("Ann", "Author"), person("Bo", "Other"),
            person("others")),
        title = "A toy method", journal = "Journal of Toys", year = 2024,
        volume = 3, pages = "1--9", doi = "10.1000/toy_a_b"
    )
    # quote marks follow the locale
    txt <- gsub("[\u201c\u201d]", "\"", GiottoClass:::.citation_text(b))
    expect_identical(txt, paste0("Author A, Other B, et al. (2024). ",
        "\"A toy method.\" Journal of Toys, 3, 1-9. doi:10.1000/toy_a_b."))
})
