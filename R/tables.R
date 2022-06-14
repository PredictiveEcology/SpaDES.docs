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
