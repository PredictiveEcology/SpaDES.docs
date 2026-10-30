#' Options every manual chapter is built with
#'
#' The shared contents of a manual's `before_chapter_script`. Each manual kept
#' its own near-identical copy of these, which is how they drifted: one set a
#' CRAN repository, overriding the one CI configures, and one removed
#' dot-prefixed objects that a build script had deliberately named with a dot so
#' they would survive.
#'
#' @details
#' `width` and the chunk options are the printing conventions the chapters are
#' written against -- narrow output that fits the page, and figures placed where
#' they appear in the text rather than floated.
#'
#' `knitr.table.format` is a function, not a string, because it is consulted
#' per chunk: the same chapter is rendered to LaTeX for the PDF and to markdown
#' for the website, and a table format fixed at either breaks the other.
#'
#' `knitr.graphics.rel_path = FALSE` keeps figure paths as given rather than
#' rewriting them relative to the chapter, which is what [stageFigure()] relies
#' on to put a module's figure where the book can find it.
#'
#' @param width column width for printed output.
#'
#' @return `NULL`, invisibly; called for its effect on options.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ## the whole of a manual's _common.R:
#' SpaDES.docs::manualChunkOptions()
#' }
manualChunkOptions <- function(width = 60) {
  options(
    width = width,
    knitr.graphics.rel_path = FALSE,
    ## a function, so it is asked again for each output format
    knitr.table.format = function() {
      if (knitr::is_latex_output()) "latex" else "pipe"
    }
  )

  knitr::opts_chunk$set(
    fig.pos = "H",
    out.extra = "",
    size = "tiny",
    tidy = TRUE,
    tidy.opts = list(width.cutoff = width)
  )

  invisible(NULL)
}

#' The manual's version, from its DESCRIPTION
#'
#' Reads `Version` and puts it in an environment variable, which is how a
#' manual's `index.Rmd` gets it onto the title page: the YAML header is parsed
#' before any of the book's code runs, so a subtitle can only reach it through
#' the environment.
#'
#' @details
#' By field name, never by position. A manual that read `read.dcf(...)[4]` got
#' `Version` only because it happened to be the fourth field, so adding any
#' field above it would have printed the wrong string on the title page.
#'
#' A manual whose chapters still read a name of its own passes it as `legacy`,
#' e.g. `manualVersion(legacy = "LANDR_MAN_VERSION")`. Nothing is set by
#' default: setting every manual's name in every manual meant each one exported
#' variables belonging to the others.
#'
#' @param description path to the DESCRIPTION file to read.
#'
#' @param envVar name of the environment variable to set.
#'
#' @param legacy additional variable names to set to the same value, for
#'  documents that still read a manual-specific name such as
#'  `LANDR_MAN_VERSION`. Empty by default.
#'
#' @return the version string, invisibly.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ## in build.R, before render_book()
#' manualVersion()
#' }
manualVersion <- function(
  description = "DESCRIPTION",
  envVar = "SPADES_MANUAL_VERSION",
  legacy = character(0)
) {
  if (!file.exists(description)) {
    stop("manualVersion(): no DESCRIPTION at '", description, "'.", call. = FALSE)
  }
  version <- read.dcf(description, fields = "Version")[1]
  if (is.na(version) || !nzchar(version)) {
    stop(
      "manualVersion(): '",
      description,
      "' has no Version field, so the ",
      "manual would be titled, and its PDF archived, without a version.",
      call. = FALSE
    )
  }

  vars <- unique(c(envVar, legacy))
  values <- as.list(rep(version, length(vars)))
  names(values) <- vars
  do.call(Sys.setenv, values)

  message("manualVersion(): v", version, " (", paste(vars, collapse = ", "), ").")
  invisible(version)
}

#' Render a manual, without letting one format bring down the rest
#'
#' Renders every format the book declares and keeps the site even when another
#' format fails, which a plain [bookdown::render_book()] call does not.
#'
#' @details
#' bookdown knits once and then writes each declared format in turn, so the
#' website is already on disk by the time a later format is attempted. Letting a
#' failure propagate therefore threw away a book that had rendered: the LandR
#' manual's deploy step was skipped and its published site sat unchanged for 38
#' days because the PDF could not compile.
#'
#' A failure is reported instead, as a GitHub Actions warning so it stays visible
#' on the run rather than scrolling past in a log, and the caller carries on to
#' deploy what did render. That is only safe because the site itself is still
#' required: `index` is removed first, so finding it afterwards means this build
#' produced it, and its absence is an error. Without removing it, a stale copy
#' from a previous local build would satisfy the check.
#'
#' Declare the fragile formats last in `_output.yml`. The formats are written in
#' the order declared and a failure stops the ones after it, so with a PDF in the
#' middle a broken PDF also cost the EPUB.
#'
#' @param outputDir directory the book is rendered into, i.e. `output_dir` in
#'  `_bookdown.yml`.
#'
#' @param index file within `outputDir` that the website must produce.
#'
#' @param ... passed to [bookdown::render_book()], e.g. `output_format`.
#'
#' @return `TRUE` if every format rendered, `FALSE` if one failed and was
#'  reported, invisibly. Errors if the site did not render.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ## in build.R, in place of bookdown::render_book(output_format = "all", ...)
#' renderManual(outputDir = "docs")
#' }
renderManual <- function(outputDir = "docs", index = "index.html", ...) {
  if (!requireNamespace("bookdown", quietly = TRUE)) {
    stop("renderManual() needs the 'bookdown' package to be installed.", call. = FALSE)
  }

  indexPath <- file.path(outputDir, index)
  unlink(indexPath)

  complete <- TRUE
  tryCatch(bookdown::render_book(...), error = function(e) {
    complete <<- FALSE
    ## ::warning:: is rendered on the run by GitHub Actions and is an ordinary
    ## line anywhere else, so this is worth emitting unconditionally
    message(
      "::warning title=Manual render incomplete::",
      "not every output format rendered: ",
      conditionMessage(e)
    )
  })

  if (!file.exists(indexPath)) {
    stop(
      "renderManual(): no '",
      indexPath,
      "' was produced, so the site itself ",
      "failed to render and there is nothing to publish.",
      call. = FALSE
    )
  }

  invisible(complete)
}
