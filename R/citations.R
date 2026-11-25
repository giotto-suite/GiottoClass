#' @title Citation of a method
#' @name methodCitation
#' @description
#' The publication to cite for a method that runs through a param object.
#' A package that wraps or reimplements a published method defines a
#' `methodCitation()` method for the method's param class, returning a
#' [utils::bibentry()].
#'
#' When the param is attached to the object history
#' (`update_giotto_params(attachments = list(param = param))`), the citation is
#' recorded with that step and shown once per session. [objCitations()] lists
#' the citations of every recorded step. Set `options(giotto.cite = FALSE)` to
#' turn the messages off.
#' @param x a param object
#' @param ... additional params (none implemented)
#' @returns a `bibentry`, or `NULL` when the method has no citation
#' @examples
#' setClass("toyParam", contains = "analyzeParam")
#' setMethod("methodCitation", "toyParam", function(x, ...) {
#'     utils::bibentry("Article",
#'         title = "A toy method", author = "Ann Author",
#'         journal = "Journal of Toys", year = 2024
#'     )
#' })
#' methodCitation(new("toyParam"))
#' @export
setMethod("methodCitation", signature("ANY"), function(x, ...) NULL)

#' @title Show the citation of a method
#' @name citeMethod
#' @description
#' Shows the citation of a method once per R session. Functions that record
#' their param with [update_giotto_params()] do not need to call this; it is
#' for functions that do not return a giotto object.
#' @param x a param object with a [methodCitation()], or a `bibentry`
#' @param what character. Name of the method, used in the message
#' @returns the citation text, invisibly
#' @examples
#' cit <- utils::bibentry("Article",
#'     title = "A toy method", author = "Ann Author",
#'     journal = "Journal of Toys", year = 2024
#' )
#' citeMethod(cit, what = "toyMethod")
#' citeMethod(cit, what = "toyMethod") # already shown: silent
#' @export
citeMethod <- function(x, what = NULL) {
    txt <- .citation_text(x)
    .cite_show(txt, what = what)
    invisible(txt)
}

#' @title Citations of the methods used on an object
#' @name objCitations
#' @description
#' The publications to cite for the steps recorded in an object's history
#' ([objHistory()]): one entry per method, in the order first used. Only
#' steps whose method declares a [methodCitation()] are listed.
#' @param x giotto object, a history from `objHistory()`, or the path to a
#' [saveGiotto()] folder or its `history.ndjson`
#' @returns character vector of citations, one per method
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' objCitations(g)
#' @export
objCitations <- function(x) {
    h <- if (inherits(x, "ghistory")) x else objHistory(x)
    cites <- unlist(lapply(unclass(h), function(r) r$cite), use.names = FALSE)
    structure(unique(as.character(cites)), class = "giottoCitations")
}

#' @export
#' @keywords internal
print.giottoCitations <- function(x, ...) {
    if (!length(x)) {
        cat("No citations recorded.\n")
        return(invisible(x))
    }
    cat("Please cite:\n\n")
    cat(paste0(unclass(x), collapse = "\n\n"), "\n")
    invisible(x)
}

# Citation text of a bibentry, a param with a methodCitation(), or text.
.citation_text <- function(x) {
    if (is.null(x)) return(NULL)
    if (!inherits(x, "bibentry") && isS4(x)) x <- methodCitation(x)
    if (is.null(x)) return(NULL)
    if (inherits(x, "bibentry")) {
        x <- vapply(seq_along(x), function(i) {
            .bibentry_plain(format(x[[i]], style = "text"))
        }, character(1L))
    }
    checkmate::assert_character(x, any.missing = FALSE)
    x
}

# One line of plain text from format(<bibentry>, style = "text"): no
# markdown emphasis, no repeated DOI URL, and "et al." for `person("others")`.
.bibentry_plain <- function(txt) {
    txt <- gsub("\\s+", " ", paste(txt, collapse = " "))
    txt <- gsub("\\s*<https?://[^>]*>", "", txt)
    txt <- gsub("(^|\\s)([_*])([^_*]+)\\2(?=[,.;:]|\\s|$)", "\\1\\3", txt,
        perl = TRUE)
    txt <- gsub("(,| and) others \\(", ", et al. (", txt)
    trimws(txt)
}

# Citations of the attachments of a history step.
.attachment_citations <- function(attachments) {
    if (!length(attachments)) return(NULL)
    out <- unlist(lapply(attachments, function(a) {
        if (isS4(a) || inherits(a, "bibentry")) .citation_text(a)
    }), use.names = FALSE)
    if (length(out)) unique(out)
}

.cite_shown <- new.env(parent = emptyenv())

# Each citation is shown once per session, whichever function shows it.
.cite_show <- function(txt, what = NULL) {
    if (!length(txt) || isFALSE(getOption("giotto.cite", TRUE))) {
        return(invisible())
    }
    new <- txt[!vapply(txt, exists, logical(1L),
        envir = .cite_shown, inherits = FALSE)]
    if (!length(new)) return(invisible())
    for (t in new) assign(t, TRUE, envir = .cite_shown)
    head <- if (is.null(what)) "If you use this method" else
        sprintf("If you use %s", what)
    message(head, " in published research, please cite:\n",
        paste0("  ", new, collapse = "\n"),
        "\n(shown once per session; objCitations() lists them all, ",
        "options(giotto.cite = FALSE) turns this off)")
    invisible()
}
