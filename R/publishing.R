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
  ## say so: a deploy that quietly lost its CNAME looks exactly like one that
  ## kept it, until the custom domain stops resolving
  message("stagePagesFiles(): wrote .nojekyll",
          if (!is.null(cname)) paste0(" and CNAME (", cname, ")") else "",
          " to '", path, "'.")
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
  message("archiveManualPDF(): archived '", basename(dest), "' to '", archiveDir, "'.")
  invisible(dest)
}

#' Publish a manual's archived releases alongside the site
#'
#' A manual that is released keeps a PDF of each release. Those PDFs belong in
#' version control rather than only on the published branch: that branch is
#' rebuilt by every deploy, and an old PDF cannot be regenerated from current
#' sources. This copies them into the rendered book directory so they deploy with
#' the site, and writes an index page listing them.
#'
#' @details
#' The index is built from the files actually present, not from a list kept by
#' hand, so it cannot drift. Versions are read from the file names, which are
#' what [archiveManualPDF()] writes: `<prefix>-v<version>.pdf`. Anything not
#' matching that shape is copied but left out of the ordering.
#'
#' @param archiveDir directory holding the archived PDFs, usually tracked in the
#'  repository.
#'
#' @param docsDir the rendered book directory, i.e. `_bookdown.yml`'s
#'  `output_dir`.
#'
#' @param manualName name of the manual, used in the index page's headings and
#'  link text.
#'
#' @param subdir directory within `docsDir` to publish into.
#'
#' @return path to the index page, invisibly, or `NULL` if there was nothing to
#'  publish.
#'
#' @export
#' @importFrom Require checkPath
publishManualArchive <- function(archiveDir = file.path("archive", "pdf"),
                                 docsDir, manualName, subdir = "archive") {
  if (!dir.exists(archiveDir)) {
    message("publishManualArchive(): no archive at '", archiveDir,
            "', nothing published.")
    return(invisible(NULL))
  }
  pdfs <- list.files(archiveDir, pattern = "[.]pdf$", full.names = TRUE)
  if (!length(pdfs)) {
    message("publishManualArchive(): no PDFs in '", archiveDir,
            "', nothing published.")
    return(invisible(NULL))
  }

  outDir <- checkPath(file.path(docsDir, subdir, "pdf"), create = TRUE)
  file.copy(pdfs, outDir, overwrite = TRUE)

  nms <- basename(pdfs)
  vers <- sub("^.*-v(.*)[.]pdf$", "\\1", nms)
  ## newest first; anything that is not a version sorts last rather than erroring
  ord <- order(numeric_version(vers, strict = FALSE), decreasing = TRUE,
               na.last = TRUE)
  nms <- nms[ord]
  vers <- vers[ord]
  sizes <- file.size(file.path(outDir, nms)) / 1048576

  index <- file.path(docsDir, subdir, "index.html")
  writeLines(c(
    "<!DOCTYPE html>",
    '<html lang="en"><head><meta charset="utf-8">',
    paste0("<title>", manualName, " &mdash; archived versions</title>"),
    "<style>body{font-family:system-ui,sans-serif;max-width:40rem;margin:3rem auto;padding:0 1rem}",
    "li{margin:.4rem 0}span{color:#666;font-size:.9em}</style>",
    "</head><body>",
    paste0("  <h1>", manualName, " &mdash; archived versions</h1>"),
    paste0('  <p>Released versions of this manual. The current one is <a href="../">here</a>.</p>'),
    "  <ul>",
    sprintf('    <li><a href="pdf/%s">%s v%s</a> <span>(PDF, %.1f MB)</span></li>',
            nms, manualName, vers, sizes),
    "  </ul>",
    "</body></html>"
  ), index)
  message("publishManualArchive(): published ", length(nms), " archived PDF(s) to '",
          file.path(subdir, "pdf"), "', newest v", vers[1], ".")
  invisible(index)
}
