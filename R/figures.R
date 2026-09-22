#' Put a module figure where the rendered document can find it
#'
#' A module chapter is rendered from wherever the *document* lives: the module's
#' own directory when the module renders on its own, and the book root once
#' [prepManualRmds()] has staged it into a manual. A relative path written for
#' one of those is wrong in the other, and it is the chapter that moves, not the
#' figure.
#'
#' @details
#' Writing an absolute path instead — via `normPath()`, say — makes the render
#' succeed and the *publication* fail. The path is correct only on the machine
#' that built the book, so the deployed site serves a dead link from every
#' chapter, and the build stays green while it happens.
#'
#' This copies `path` next to the rendered document, under the same relative
#' name, and hands that name back. The reference is then the same relative path
#' in both cases, and the file travels with the book into the published output.
#' A figure the module downloads at render time is handled the same way: write it
#' into the module's own `figures/` first, as usual, then pass it through here.
#'
#' @param path figure path relative to the module directory, i.e. what the module
#'  uses when it renders on its own — for example `"figures/schematic.png"`.
#'
#' @param outputDir directory the document is being rendered in. The default asks
#'  knitr, which is what a chunk wants; pass it explicitly when testing.
#'
#' @return `path`, unchanged, so the call can be used in place of the path.
#'
#' @export
#' @importFrom Require checkPath
#'
#' @examples
#' \dontrun{
#' ## in a module chapter, instead of normPath():
#' knitr::include_graphics(stageFigure("figures/schematic.png"))
#' }
stageFigure <- function(path, outputDir = knitr::opts_knit$get("output.dir")) {
  ## no knitr, or a knitr that does not say: nothing has been staged, so the
  ## module's own relative path is both the best answer available and the right one
  if (is.null(outputDir) || !nzchar(outputDir)) {
    return(path)
  }

  dest <- file.path(outputDir, path)
  if (identical(normalizePath(path, mustWork = FALSE),
                normalizePath(dest, mustWork = FALSE))) {
    ## rendering in place: the figure is already beside the document
    return(path)
  }

  if (!file.exists(path)) {
    stop("stageFigure(): no figure at '", path, "', relative to '", getwd(),
         "'. A figure fetched at render time must be written before it is staged.",
         call. = FALSE)
  }

  checkPath(dirname(dest), create = TRUE)
  if (!file.copy(path, dest, overwrite = TRUE)) {
    stop("stageFigure(): could not copy '", path, "' to '", dest, "'", call. = FALSE)
  }
  path
}
