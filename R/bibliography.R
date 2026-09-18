#' Collapse the modules' bibliographies into one
#'
#' A project manual cites references that live in each module's own
#' `citations/references_<module>.bib`. bookdown wants a single bibliography, so
#' these are merged, together with any further files the manual supplies --
#' typically an auto-generated one for the R packages in use, and the manual's
#' own references.
#'
#' Files with no entries are skipped. A module that cites nothing yet reasonably
#' ships a comments-only `.bib`, and `RefManageR::ReadBib()` fails on such a file
#' with `arguments imply differing number of rows: 0, 1`, which would otherwise
#' take a whole manual down over one placeholder.
#'
#' `outFile` may also appear in `extraBibs`: every input is read before anything
#' is written, so a manual can accumulate into its own bibliography as before.
#'
#' @param modulePath directory holding the module directories, searched
#'  recursively for `references_*.bib`.
#'
#' @param extraBibs further `.bib` files to merge in, in the order given. Files
#'  that do not exist are ignored, so a manual can list one that is only
#'  generated on some builds.
#'
#' @param outFile path to write the merged bibliography to. Its directory is
#'  created if needed.
#'
#' @return `outFile`, invisibly.
#'
#' @export
#' @importFrom Require checkPath
collapseModuleBibs <- function(modulePath = "modules",
                               extraBibs = character(0),
                               outFile = file.path("citations", "references.bib")) {
  if (!requireNamespace("RefManageR", quietly = TRUE)) {
    stop("collapseModuleBibs() needs the 'RefManageR' package to be installed.",
         call. = FALSE)
  }

  bibFiles <- c(
    list.files(modulePath, pattern = "references_.*\\.bib$",
               recursive = TRUE, full.names = TRUE),
    extraBibs
  )
  bibFiles <- unique(bibFiles[file.exists(bibFiles)])

  hasEntries <- vapply(bibFiles, bibHasEntries, logical(1), USE.NAMES = FALSE)
  if (any(!hasEntries)) {
    ## not a warning: a module that cites nothing is a normal state, and this
    ## would otherwise fire on every build of a manual that has one
    message("collapseModuleBibs(): skipping ", sum(!hasEntries),
            " .bib file(s) with no entries: ",
            paste(basename(bibFiles[!hasEntries]), collapse = ", "))
  }
  bibFiles <- bibFiles[hasEntries]

  if (!length(bibFiles)) {
    warning("collapseModuleBibs(): no .bib file with any entries was found ",
            "under '", modulePath, "'. Writing an empty bibliography.",
            call. = FALSE)
    checkPath(dirname(outFile), create = TRUE)
    file.create(outFile)
    return(invisible(outFile))
  }

  bibs <- lapply(bibFiles, RefManageR::ReadBib)
  merged <- Reduce(merge, bibs)

  checkPath(dirname(outFile), create = TRUE)
  RefManageR::WriteBib(merged, file = outFile)
  invisible(outFile)
}

## A .bib with only comments, or an empty one, has no `@` line. Cheaper and more
## forgiving than parsing, which is the thing we are trying to avoid doing.
bibHasEntries <- function(f) {
  any(grepl("^[[:space:]]*@", readLines(f, warn = FALSE)))
}

#' Write a bibliography for the R packages in use
#'
#' A manual cites the packages its modules depend on. [knitr::write_bib()] keys
#' these `R-<package>`.
#'
#' @param file path to write to. Its directory is created if needed.
#'
#' @param packages packages to cite. Defaults to `base` plus every package
#'  available in the current library paths, which is what a manual built in a
#'  disposable CI library wants.
#'
#' @return `file`, invisibly.
#'
#' @export
#' @importFrom knitr write_bib
#' @importFrom Require checkPath
writePkgBib <- function(file = file.path("citations", "packages.bib"),
                        packages = NULL) {
  if (is.null(packages)) {
    packages <- c("base", .packages(all.available = TRUE))
  }
  checkPath(dirname(file), create = TRUE)
  ## not every package carries a date or year, and write_bib() warns per package
  suppressWarnings(write_bib(packages, file))
  invisible(file)
}

#' Fetch a citation style
#'
#' Downloads a Citation Style Language file from the Zotero style repository, as
#' referenced by a book's `csl:` field. Existing files are kept, so a build does
#' not depend on the network once the style is present.
#'
#' @param style style name as it appears in the Zotero repository, e.g.
#'  `"ecology-letters"`.
#'
#' @param destDir directory to write into, created if needed.
#'
#' @param overwrite whether to re-download when the file is already there.
#'
#' @return path to the style file, invisibly.
#'
#' @export
#' @importFrom Require checkPath
#' @importFrom utils download.file
downloadCSL <- function(style = "ecology-letters", destDir = "citations",
                        overwrite = FALSE) {
  destDir <- checkPath(destDir, create = TRUE)
  dest <- file.path(destDir, paste0(style, ".csl"))
  if (file.exists(dest) && !isTRUE(overwrite)) {
    return(invisible(dest))
  }
  download.file(paste0("https://www.zotero.org/styles/", style, "?source=1"),
                destfile = dest)
  invisible(dest)
}
