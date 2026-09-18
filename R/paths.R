#' Locate a manual's directories
#'
#' Resolves the paths a manual's build script works in, so each one does not
#' repeat the same root-finding and `_bookdown.yml` parsing.
#'
#' @param prjDir the manual's root. Defaults to the enclosing RStudio project or
#'  git repository, found from `getwd()`.
#'
#' @param bookdownYML the book's `_bookdown.yml`, relative to `prjDir`. Its
#'  `output_dir` gives the rendered book directory.
#'
#' @param create whether to create the `citations` and `figures` directories.
#'  The rendered book directory is left to bookdown.
#'
#' @return a named list of absolute paths: `prj`, `docs`, `citations` and
#'  `figures`.
#'
#' @export
#' @importFrom Require checkPath
manualPaths <- function(prjDir = NULL, bookdownYML = "_bookdown.yml", create = TRUE) {
  if (is.null(prjDir)) {
    if (!requireNamespace("rprojroot", quietly = TRUE)) {
      stop("manualPaths() needs the 'rprojroot' package, or an explicit `prjDir`.",
           call. = FALSE)
    }
    prjDir <- rprojroot::find_root(
      rprojroot::is_rstudio_project | rprojroot::is_git_root | rprojroot::from_wd,
      path = getwd()
    )
  }
  prjDir <- normalizePath(prjDir, winslash = "/", mustWork = TRUE)

  ymlPath <- file.path(prjDir, bookdownYML)
  if (!file.exists(ymlPath)) {
    stop("manualPaths(): no '", bookdownYML, "' in '", prjDir, "'.", call. = FALSE)
  }
  outputDir <- yaml::read_yaml(ymlPath)[["output_dir"]]
  if (is.null(outputDir)) {
    stop("manualPaths(): '", bookdownYML, "' does not set `output_dir`.", call. = FALSE)
  }

  paths <- list(
    prj = prjDir,
    docs = normalizePath(file.path(prjDir, outputDir), winslash = "/", mustWork = FALSE),
    citations = file.path(prjDir, "citations"),
    figures = file.path(prjDir, "figures")
  )
  if (isTRUE(create)) {
    paths$citations <- checkPath(paths$citations, create = TRUE)
    paths$figures <- checkPath(paths$figures, create = TRUE)
  }
  paths
}
