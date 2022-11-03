## --------------------------------------------------
## Useful functions to deal with tables in manuals
## --------------------------------------------------

#' Wrap/break strings by size
#'
#' Unlike \code{strwrap} and \code{strsplit}, \code{wrapStrFun}
#'   "breaks" a string based on length creating chunks of a
#'   specified size that are collapsed into a single string but
#'   separated using a special character
#'
#' @param x the string, or a vector or list of strings to break
#'
#' @param size desired size of string chunks. Strings whose
#'   original size is <= than \code{size} will not be cut.
#'
#' @param breakChar break symbol/character used to separate chunks
#'
#' @return the "broken" and collapsed strings, in the same class
#'   as \code{x}
#'
#' @export
#' @importFrom stringi stri_sub
wrapStrFun <- function(x, size, breakChar = "\n") {
  sapply(x, function(xx) {
    if (is.na(xx) | nchar(xx) <= size) {
      xx
    } else {
      paste(stri_sub(xx, seq(1, nchar(xx), by = size), length = size), collapse = breakChar)
    }
  })
}

#' Labels and captions using \code{bookdown} cross-referencing format
#'
#' Makes captions for tables and figures using the \code{bookdown}
#'   cross-referencing format. Meant to be used inside an Rmarkdown chunk.
#'
#' @param caption a string with the desired caption.
#'
#' @param tag the cross-reference tag used by \code{bookdown}. Recognised
#'   tagas are \code{"tab"}, \code{"fig"} and \code{"eq"}. See
#'   https://bookdown.org/yihui/bookdown
#'
#' @return the full caption with its label in a HTML or LaTeX compatible
#'   format
#'
#' @export
#' @importFrom knitr opts_current is_latex_output
addLabel <- function(caption = "", tag = "tab") {
  chunkLabel <- opts_current$get("label")
  pretag <- if (is_latex_output()) {
    paste0("\\label{", tag, ":",chunkLabel , "}")
  } else {
    paste0("(\\#", tag, ":", chunkLabel, ")")
  }
  paste0(pretag, caption)
}


#' Pander and kable wrapper function
#'
#' This function overcomes/works around a recent issue with
#' \code{kable(..., longtable = TRUE)} and \code{kable_styling(..., full_width = TRUE)}
#' which fail when knitting to PDF (see https://stackoverflow.com/questions/71651334/longtable-t-messes-up-with-scaled-down-table-in-r-markdown-pdf/72511243#72511243).
#' It uses \code{pander::pander.table} to automatically deal with long/wide tables when knitting to PDF
#' and \code{knitr::kable} when knitting to HTML.
#' Note that the chunk option \code{results} must be set to `"asis"`.
#'
#' @param tab table object compatible with \code{pander::pander} AND
#'   \code{knitr::kable}
#'
#' @param caption a caption. Make sure special LaTeX characters are escaped.
#'  \code{bookdown} text references can be useful when formatting text and using
#'  special characters (https://bookdown.org/yihui/bookdown/markdown-extensions-by-bookdown.html#text-references).
#'
#' @param landscape if TRUE panderOptions are changed so that the page can be
#'   put in landscape position. Note that this requires adding \code{\\newpage} and
#'   \code{\\begin{landscape}} before the chunk and \code{\\end{landscape}} after the chunk.
#'
#' @param panderArgs named list of additional arguments passed to \code{pander}.
#'   Do NOT pass the caption and input table arguments.
#'
#' @param kableArgs named list of additional arguments passed to \code{kable}.
#'   Do NOT pass the caption and input table arguments.
#'
#' @param kable_stylingArgs named list of additional arguments passed to
#'   \code{kable_styling}. Do NOT pass the input table argument.
#'
#' @param column_specArgs named list of additional arguments passed to
#'   \code{column_spec}. Do NOT pass the input table argument.
#'
#' @return a markdown table.
#'
#' @export
#' @importFrom knitr is_latex_output kable
#' @importFrom kableExtra kable_styling landscape column_spec
#' @importFrom pander pander panderOptions

panble <- function(tab, caption = "",
                   landscape = FALSE,
                   panderArgs = list(), kableArgs = list(),
                   kable_stylingArgs = list(), column_specArgs = list()) {

  ## check that chunk results must be 'asis'
  if (isFALSE(knitr::opts_current$get("results") == "asis")) {
    stop("Please use chunk option results = 'asis'")
  }

  if (is_latex_output()) {
    panderArgs$x <- tab
    panderArgs$caption <- addLabel(caption)
    if (landscape) {
      opts <- panderOptions("knitr.auto.asis", FALSE)
      on.exit(options(opts))
    }
    do.call(pander, panderArgs)
  } else {
    kableArgs$x <- tab
    kableArgs$caption <- caption
    kableArgs$format <- "html"
    outTable <- do.call(kable, kableArgs)

    ## TODO: there must be a smarter way to do this. The point is that
    ## returning the table before its finished doesn't seem to work
    if (length(kable_stylingArgs)) {
      kable_stylingArgs$kable_input <- outTable
      outTable <- do.call(kable_styling, kable_stylingArgs)
    }

    if (length(column_specArgs)) {
      column_specArgs$kable_input <- outTable
      outTable <- do.call(column_spec, column_specArgs)
    }
    ## this won't do anything in HTML but is left here for future reference
    # if (landscape) {
    # outTable <- landscape(outTable)
    # }
    # print(outTable)  ## this was working before?
    return(outTable)
  }
}

