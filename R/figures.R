#' Put a module figure where the rendered document can find it
#'
#' A module chapter is rendered from wherever the *document* lives: the module's
#' own directory when the module renders on its own, and the book root once
#' [prepManualRmds()] has staged it into a manual. A relative path written for
#' one of those is wrong in the other, and it is the chapter that moves, not the
#' figure.
#'
#' @details
#' Writing an absolute path instead -- via `normPath()`, say -- makes the render
#' succeed and the *publication* fail. The path is correct only on the machine
#' that built the book, so the deployed site serves a dead link from every
#' chapter, and the build stays green while it happens.
#'
#' When the chapter has been staged, this copies `path` into a directory of the
#' module's own beside the staged chapter, and returns the relative path to that
#' copy, so the file travels with the book into the published output. The
#' directory is per module because modules reuse conventional names: every one
#' writes `figures/moduleVersionBadge.png`, and a shared directory would show
#' each chapter the badge of whichever module was knitted last.
#'
#' When the module renders on its own, `path` is returned unchanged and nothing
#' is copied: its relative paths already resolve from its own directory.
#' [prepManualRmds()] marks a staged chapter by setting the knitr option
#' `SpaDES.docs.stageDir` in its setup chunk; that option's absence is what
#' "rendering on its own" means here.
#'
#' A figure fetched at render time is handled the same way: write it into the
#' module's own `figures/` first, as usual, then pass it through.
#'
#' @param path figure path(s) relative to the module directory, i.e. what the
#'  module uses when it renders on its own -- for example
#'  `"figures/schematic.png"`. A vector is staged element by element.
#'
#' @param stageDir directory the staged chapter's figures go in, relative to the
#'  book root. The default reads what [prepManualRmds()] set for the chapter; pass
#'  it explicitly only when testing.
#'
#' @param outputDir directory the document is being rendered in, i.e. the book
#'  root. The default asks knitr; pass it explicitly only when testing.
#'
#' @return the path(s) to use in the reference: `path` unchanged when the module
#'  renders on its own, otherwise the relative path(s) to the staged copies.
#'
#' @export
#' @importFrom Require checkPath
#'
#' @examples
#' \dontrun{
#' ## in a module chapter, instead of normPath():
#' knitr::include_graphics(stageFigure("figures/schematic.png"))
#' }
stageFigure <- function(path,
                        stageDir = knitr::opts_knit$get("SpaDES.docs.stageDir"),
                        outputDir = knitr::opts_knit$get("output.dir")) {
  if (is.null(stageDir) || !nzchar(stageDir)) {
    return(path)
  }
  if (is.null(outputDir) || !nzchar(outputDir)) {
    stop("stageFigure(): the chapter is staged into '", stageDir, "', but knitr ",
         "does not say where the document is rendered, so there is nowhere to ",
         "copy to.", call. = FALSE)
  }

  missing <- path[!file.exists(path)]
  if (length(missing)) {
    stop("stageFigure(): no figure at ", paste0("'", missing, "'", collapse = ", "),
         ", relative to '", getwd(), "'. A figure fetched at render time must be ",
         "written before it is staged.", call. = FALSE)
  }

  ## the reference is resolved against the rendered document, not the module, so
  ## a leading "./" would only carry through into it as noise
  ref <- file.path(stageDir, sub("^\\./", "", path))
  dest <- file.path(outputDir, ref)
  for (d in unique(dirname(dest))) {
    checkPath(d, create = TRUE)
  }
  copied <- file.copy(path, dest, overwrite = TRUE)
  if (!all(copied)) {
    stop("stageFigure(): could not copy ",
         paste0("'", path[!copied], "'", collapse = ", "),
         " into '", file.path(outputDir, stageDir), "'", call. = FALSE)
  }
  ref
}
