#' Stage the files GitHub Pages needs, inside the published directory
#'
#' A deploy action publishes the *contents* of the rendered book directory, so
#' anything written to the repository root never reaches the site. Two files have
#' to be in the published directory itself:
#'
#' * `.nojekyll`, without which GitHub Pages runs the output through Jekyll and
#'   drops the `_`-prefixed directories bookdown emits;
#' * `CNAME`, without which GitHub resets a custom domain to the default
#'   `*.github.io` address on the next deploy.
#'
#' Writing these at the repository root appears to work for as long as an earlier
#' deploy left copies behind and the action deploys with `clean = false`. A
#' branch published for the first time has neither.
#'
#' @param path the rendered book directory, i.e. `_bookdown.yml`'s `output_dir`.
#'
#' @param cname custom domain for the site, or `NULL` (the default) to write no
#'  `CNAME` file. Pass the bare domain, without a scheme.
#'
#' @return `path`, invisibly.
#'
#' @export
stagePagesFiles <- function(path, cname = NULL) {
  if (!dir.exists(path)) {
    stop("stagePagesFiles(): '", path, "' does not exist. Render the book first.",
         call. = FALSE)
  }
  file.create(file.path(path, ".nojekyll"))
  if (!is.null(cname)) {
    if (grepl("^[[:alpha:]]+://", cname)) {
      stop("stagePagesFiles(): `cname` must be a bare domain, with no scheme: got '",
           cname, "'", call. = FALSE)
    }
    writeLines(cname, file.path(path, "CNAME"))
  }
  invisible(path)
}

#' Archive a rendered manual PDF alongside the book
#'
#' Keeps a versioned copy of each released PDF. Does nothing when no PDF was
#' produced, which is the usual case while a manual renders HTML only -- copying
#' unconditionally silently succeeds with `FALSE` and leaves an empty archive.
#'
#' @param pdf path to the rendered PDF.
#'
#' @param version version string for the archived file name.
#'
#' @param prefix leading part of the archived file name; the result is
#'  `<prefix>-v<version>.pdf`.
#'
#' @param archiveDir directory to copy into, created if needed.
#'
#' @return path of the archived copy, or `NULL` if there was no PDF to archive,
#'  invisibly.
#'
#' @export
#' @importFrom Require checkPath
archiveManualPDF <- function(pdf, version, prefix, archiveDir = file.path("archive", "pdf")) {
  if (!file.exists(pdf)) {
    message("archiveManualPDF(): no PDF at '", pdf, "', nothing archived.")
    return(invisible(NULL))
  }
  if (!nzchar(version)) {
    stop("archiveManualPDF(): `version` is empty, so the archived file would ",
         "not be identifiable.", call. = FALSE)
  }
  archiveDir <- checkPath(archiveDir, create = TRUE)
  dest <- file.path(archiveDir, paste0(prefix, "-v", version, ".pdf"))
  file.copy(from = pdf, to = dest, overwrite = TRUE)
  invisible(dest)
}
