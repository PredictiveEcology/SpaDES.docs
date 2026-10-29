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
    stop("stagePagesFiles(): '", path, "' does not exist. Render the book first.", call. = FALSE)
  }
  file.create(file.path(path, ".nojekyll"))
  if (!is.null(cname)) {
    if (grepl("^[[:alpha:]]+://", cname)) {
      stop(
        "stagePagesFiles(): `cname` must be a bare domain, with no scheme: got '",
        cname,
        "'",
        call. = FALSE
      )
    }
    writeLines(cname, file.path(path, "CNAME"))
  }
  ## say so: a deploy that quietly lost its CNAME looks exactly like one that
  ## kept it, until the custom domain stops resolving
  message(
    "stagePagesFiles(): wrote .nojekyll",
    if (!is.null(cname)) paste0(" and CNAME (", cname, ")") else "",
    " to '",
    path,
    "'."
  )
  invisible(path)
}

#' Archive a rendered manual PDF alongside the book
#'
#' Keeps a versioned copy of each released PDF. Does nothing when no PDF was
#' produced, which is the usual case while a manual renders HTML only -- copying
#' unconditionally silently succeeds with `FALSE` and leaves an empty archive.
#'
#' @details
#' An archived version is never overwritten. A release PDF is a record of what
#' that version looked like, so the first copy is the one that counts, and a
#' later build at the same version leaves it alone.
#'
#' That is also the only workable test. A PDF is not reproducible byte for byte:
#' xelatex stamps a creation date and a document ID into every run, so the same
#' sources archive differently each time. A caller that commits whatever changed
#' under `archiveDir` therefore saw a modified file after every build, and
#' overwriting made a release PDF depend on which build touched it last. Keyed
#' on the file's existence, archiving happens once per version, which is what a
#' release means.
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
#' @return path of the archived copy, whether this call wrote it or found it
#'  already there, or `NULL` if there was no PDF to archive, invisibly.
#'
#' @export
#' @importFrom Require checkPath
archiveManualPDF <- function(pdf, version, prefix, archiveDir = file.path("archive", "pdf")) {
  if (!file.exists(pdf)) {
    message("archiveManualPDF(): no PDF at '", pdf, "', nothing archived.")
    return(invisible(NULL))
  }
  if (!nzchar(version)) {
    stop(
      "archiveManualPDF(): `version` is empty, so the archived file would ",
      "not be identifiable.",
      call. = FALSE
    )
  }
  ## normalised because checkPath() returns the path as given when it creates the
  ## directory and an absolute one when it was already there, so without this the
  ## first build of a manual and every later one disagree about what was archived
  archiveDir <- normalizePath(
    checkPath(archiveDir, create = TRUE),
    winslash = "/",
    mustWork = FALSE
  )
  dest <- file.path(archiveDir, paste0(prefix, "-v", version, ".pdf"))
  if (file.exists(dest)) {
    message(
      "archiveManualPDF(): v",
      version,
      " is already archived as '",
      basename(dest),
      "'; left as it is."
    )
    return(invisible(dest))
  }
  file.copy(from = pdf, to = dest)
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
#' @seealso [manualArchiveLinks()] to list the same versions inline, in a
#'  chapter.
#'
#' @param subdir directory within `docsDir` to publish into.
#'
#' @return path to the index page, invisibly, or `NULL` if there was nothing to
#'  publish.
#'
#' @export
#' @importFrom Require checkPath
publishManualArchive <- function(
  archiveDir = file.path("archive", "pdf"),
  docsDir,
  manualName,
  subdir = "archive"
) {
  if (!dir.exists(archiveDir)) {
    message("publishManualArchive(): no archive at '", archiveDir, "', nothing published.")
    return(invisible(NULL))
  }
  pdfs <- list.files(archiveDir, pattern = "[.]pdf$", full.names = TRUE)
  if (!length(pdfs)) {
    message("publishManualArchive(): no PDFs in '", archiveDir, "', nothing published.")
    return(invisible(NULL))
  }

  outDir <- checkPath(file.path(docsDir, subdir, "pdf"), create = TRUE)
  file.copy(pdfs, outDir, overwrite = TRUE)

  archived <- archivedPDFs(archiveDir)
  nms <- archived$file
  vers <- archived$version
  sizes <- file.size(file.path(outDir, nms)) / 1048576

  index <- file.path(docsDir, subdir, "index.html")
  writeLines(
    c(
      "<!DOCTYPE html>",
      '<html lang="en"><head><meta charset="utf-8">',
      paste0("<title>", manualName, " &mdash; archived versions</title>"),
      "<style>body{font-family:system-ui,sans-serif;max-width:40rem;margin:3rem auto;padding:0 1rem}",
      "li{margin:.4rem 0}span{color:#666;font-size:.9em}</style>",
      "</head><body>",
      paste0("  <h1>", manualName, " &mdash; archived versions</h1>"),
      paste0(
        '  <p>Released versions of this manual. The current one is <a href="../">here</a>.</p>'
      ),
      "  <ul>",
      sprintf(
        '    <li><a href="pdf/%s">%s v%s</a> <span>(PDF, %.1f MB)</span></li>',
        nms,
        manualName,
        vers,
        sizes
      ),
      "  </ul>",
      "</body></html>"
    ),
    index
  )
  message(
    "publishManualArchive(): published ",
    length(nms),
    " archived PDF(s) to '",
    file.path(subdir, "pdf"),
    "', newest v",
    vers[1],
    "."
  )
  invisible(index)
}

## The archived PDFs, newest first. Shared by publishManualArchive(), which
## writes a page of them, and manualArchiveLinks(), which returns them as
## markdown -- two presentations of one list, which had been implemented twice.
archivedPDFs <- function(archiveDir) {
  if (!dir.exists(archiveDir)) {
    return(data.frame(file = character(0), version = character(0)))
  }
  files <- basename(list.files(archiveDir, pattern = "[.]pdf$"))
  versions <- sub("^.*-v(.*)[.]pdf$", "\\1", files)
  ## newest first. strict = FALSE so a file that is not named for a version
  ## sorts last rather than taking the whole build down
  ord <- order(numeric_version(versions, strict = FALSE), decreasing = TRUE, na.last = TRUE)
  data.frame(file = files[ord], version = versions[ord])
}

#' Markdown links to a manual's archived versions
#'
#' The "previous versions" list for a manual's landing page, built from the
#' files actually present rather than maintained by hand.
#'
#' @details
#' Every hand-maintained version of this list had drifted by the time it was
#' replaced: one manual's `index.Rmd` still named three releases in commented-out
#' markdown, and two listed versions whose files had moved.
#'
#' The links are relative to the published book, where [publishManualArchive()]
#' has copied the PDFs, rather than absolute URLs into the repository. A link to
#' a repository's raw files stops working if it is renamed or made private, and
#' sends a reader off the site to fetch a file the site is already serving.
#'
#' @param archiveDir directory holding the archived PDFs.
#'
#' @param manualName name of the manual, used in the link text.
#'
#' @param exclude version to leave out, normally the one being built: it is the
#'  current version, not a previous one. `NULL` or `""` keeps all of them.
#'
#' @param urlPrefix path the links are relative to, i.e. where
#'  [publishManualArchive()] publishes the PDFs within the book.
#'
#' @return a character vector of markdown list items, one per version, newest
#'  first. Empty if there is nothing archived.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ## in index.Rmd, in a `results = "asis"` chunk:
#' cat(manualArchiveLinks(manualName = "LandR Manual"), sep = "")
#' }
manualArchiveLinks <- function(
  archiveDir = file.path("archive", "pdf"),
  manualName,
  exclude = Sys.getenv("SPADES_MANUAL_VERSION"),
  urlPrefix = file.path("archive", "pdf")
) {
  archived <- archivedPDFs(archiveDir)
  if (!nrow(archived)) {
    return(character(0))
  }
  if (length(exclude) && nzchar(exclude)) {
    archived <- archived[archived$version != exclude, , drop = FALSE]
  }
  if (!nrow(archived)) {
    return(character(0))
  }

  paste0(
    "-   [",
    manualName,
    " v",
    archived$version,
    "](",
    file.path(urlPrefix, archived$file),
    ")\n"
  )
}
