utils::globalVariables(c(
  ":=", "dups", "lineText", "lineID"
))

#' Prepare module .Rmd to render book
#'
#' Creates modified versions of the modules' .Rmd files,
#'   with YAML headers removed and adapted knitr setup
#'   chunks
#'
#' @param modulePath modules' folder directory. It must be
#'  exactly as in the `_bookdown.yml` lines specifying where
#'  the module .Rmds live. For instance, if in `_bookdown.yml`
#'  the list of module .Rmds is provided as `-  modules/XXX.Rmd`,
#'  `-  ~/modules/XXX.Rmd`, or `-  ../X/modules/XXX.Rmd`
#'  `modulePath` must be either `modules`, `~/modules` and
#'  `../X/modules` respectively. A `/` may be appended at the
#'  of `modulePath` (e.g. `~/modules/`).
#'  Note that all modules must be in the same directory.
#'
#' @param rebuildCache should cached chunks be re-executed?
#'
#' @return file paths of the modified module .Rmd files
#'
#' @export
#' @importFrom Require normPath
#' @importFrom data.table data.table rbindlist
#' @importFrom utils capture.output
prepManualRmds <- function(modulePath, rebuildCache = FALSE) {
  moduleRmds <- list.dirs(modulePath, recursive = FALSE)
  moduleRmds <- paste0(file.path(moduleRmds, basename(moduleRmds)), ".Rmd")

  copyModuleRmds <- sapply(moduleRmds, rebuildCache = rebuildCache,
                           FUN = function(x, rebuildCache) {
                             copyModuleRmd <- sub("(.*)(\\.Rmd)", "\\12\\2", x)
                             file.copy(x, copyModuleRmd, overwrite = TRUE)

                             ## strip module.Rmd YAML headers -----
                             linesModuleRmd <- readLines(copyModuleRmd)
                             lines2Rm <- modelr::seq_range(which(linesModuleRmd == "---"), by = 1)
                             linesModuleRmd <- linesModuleRmd[-lines2Rm]

                             ## add chapter title if not present
                             nonEmptyLines <- linesModuleRmd[linesModuleRmd != ""]
                             if (!grepl("^# ", nonEmptyLines[1])) {
                               modName <- sub(".Rmd", "" ,basename(x))
                               chapterTitle <- paste0("# LandR *", modName, "* Module") ## TODO: remove 'LandR' to keep it general

                               linesModuleRmd <- c("", chapterTitle, linesModuleRmd)
                             }

                             ## make sure that setup chunk will be evaluated again
                             ## (a previous setup chunk may have set "eval = FALSE" and "cache = TRUE")
                             setupChunkStart <- which(grepl("```{r setup", linesModuleRmd, fixed = TRUE))
                             setupChunkOptions <- linesModuleRmd[setupChunkStart]
                             if (isFALSE(grepl("eval[[:space:]]*=[[:space:]]*TRUE", setupChunkOptions))) {
                               setupChunkOptions <- if (grepl("eval", setupChunkOptions)) {
                                 sub("(.*)(eval[[:space:]]*=[[:space:]]*FALSE)(.*)\\}", "\\1eval = TRUE\\3\\}", setupChunkOptions)
                               } else {
                                 sub("(.*)\\}", "\\1, eval = TRUE\\}", setupChunkOptions)
                               }
                             }

                             if (isFALSE(grepl("cache[[:space:]]*=[[:space:]]*FALSE", setupChunkOptions))) {
                               setupChunkOptions <- if (grepl("cache", setupChunkOptions)) {
                                 sub("(.*)(cache[[:space:]]*=[[:space:]]*)(TRUE|[[:digit:]])(.*)\\}", "\\1\\2FALSE\\4\\}", setupChunkOptions)
                               } else {
                                 sub("(.*)\\}", "\\1, cache = FALSE\\}", setupChunkOptions)
                               }
                             }

                             linesModuleRmd[setupChunkStart] <- setupChunkOptions

                             ## change root.dir for each .Rmd
                             existsRootDirsSetup <- any(grepl("root\\.dir", linesModuleRmd))   ## only searching for argument instead of <function>(.*<arg>) as the code may be split into different lines
                             if (existsRootDirsSetup) {
                               ## make sure the root.dir is the right one
                               rootDirLine <- which(grepl("root\\.dir", linesModuleRmd))
                               dir2replace <- normPath(dirname(copyModuleRmd))
                               code2replace <-  sub("(.*root\\.dir.*=[[:space:]]*)(.*)(\\))",
                                                    paste0("\\1", "'",  dir2replace, "'", "\\3"),
                                                    linesModuleRmd[rootDirLine])
                               linesModuleRmd[rootDirLine] <- code2replace
                             } else {
                               ## break lines into 2 to add a working dir setup line
                               beforeSetupChunkStart <- linesModuleRmd[1:setupChunkStart]
                               afterSetupChunkStart <- linesModuleRmd[(setupChunkStart + 1):length(linesModuleRmd)]

                               addedCode <- paste0("knitr::opts_knit$set(root.dir = '", normPath(dirname(copyModuleRmd)), "')")

                               linesModuleRmd <- c(beforeSetupChunkStart, addedCode, afterSetupChunkStart)
                             }

                             ## add cache rebuild options for each .Rmd
                             existsCacheRebuildSetup <- any(grepl("cache.rebuild", linesModuleRmd))
                             if (existsCacheRebuildSetup) {
                               ## overwrite option
                               cacheRebuildLine <- which(grepl(",*[[:space:]]*cache.rebuild[[:space:]]*=[[:space:]]*(TRUE|FALSE)[[:space:]]*(,|\\})", linesModuleRmd))
                               code2replace <- sub("(.*cache\\.rebuild.*=[[:space:]]*)(TRUE|FALSE)(.*)",
                                                   paste0("\\1", rebuildCache, "\\3"),
                                                   linesModuleRmd[cacheRebuildLine])
                               linesModuleRmd[cacheRebuildLine] <- code2replace
                             } else {
                               ## break lines into 2 to add a cache rebuild dir setup line (it doesn't matter if there
                               ## is another call to `knitr::opts_chunk$set`)
                               beforeSetupChunkStart <- linesModuleRmd[1:setupChunkStart]
                               afterSetupChunkStart <- linesModuleRmd[(setupChunkStart + 1):length(linesModuleRmd)]

                               addedCode <- paste0("knitr::opts_chunk$set(cache.rebuild = ", rebuildCache, ")")

                               linesModuleRmd <- c(beforeSetupChunkStart, addedCode, afterSetupChunkStart)
                             }

                             ## if missing add chapter bibliography at the end of each module chapter:
                             chapterBibLine <- grep("## References|# References", linesModuleRmd)

                             ## if not in one of the last two lines, "move to the end"
                             if (length(chapterBibLine)) {
                               if (!chapterBibLine %in% c(length(linesModuleRmd), length(linesModuleRmd) - 1)) {
                                 chapterBibLineChar <- linesModuleRmd[chapterBibLine]
                                 linesModuleRmd <- linesModuleRmd[-chapterBibLine]
                                 linesModuleRmd <- capture.output(cat(linesModuleRmd, chapterBibLineChar, append = TRUE, sep = "\n"))
                               }
                             } else {
                               linesModuleRmd <- capture.output(cat(linesModuleRmd, "## References", append = TRUE, sep = "\n"))
                             }

                             ## add the LaTex bib command for chapter references
                             latexChapterBibLine <- any(grepl("printbibliography", linesModuleRmd))
                             if (isFALSE(latexChapterBibLine)) {
                               linesModuleRmd <- capture.output(cat(linesModuleRmd, "\\printbibliography[segment=\\therefsegment,heading=none]", append = TRUE, sep = "\n"))
                             }

                             writeLines(linesModuleRmd, con = copyModuleRmd)

                             return(copyModuleRmd)
                           })

  ## make sure there aren't repeated text references across modules
  ## first get module order
  bkdwnYML <- readLines("_bookdown.yml")

  ## grep module folder name at the start of the string and escape "." and "/"
  grepStr <- paste0("^", modulePath)
  grepStr <- gsub("/", "\\/", grepStr, fixed = TRUE)
  grepStr <- gsub(".", "\\.", grepStr, fixed = TRUE)
  ## make sure there's a / at the end to grep only a folder at the start of a .Rmd path
  if (!grepl("\\/$", grepStr)) {
    grepStr <- paste0(grepStr, "\\/")
  }
  bkdwnYMLsub <- sub("  - ", "", bkdwnYML, fixed = TRUE)
  bkdwnYMLsub <- bkdwnYMLsub[grepl(grepStr, bkdwnYMLsub)]
  bkdwnYMLsub <- normPath(bkdwnYMLsub)

  ## now read all module lines and put the list in the right order
  allModules <- lapply(copyModuleRmds, readLines)
  names(allModules) <- normPath(copyModuleRmds)
  allModules <- allModules[bkdwnYMLsub]

  ## get the text ref lines and their line IDs
  refTextLinesID <- lapply(allModules, function(x) {
    data.table(lineText = grep("^\\(ref\\:.*\\)", x, value = TRUE),
               lineID = grep("^\\(ref\\:.*\\)", x))
  })
  refTextLinesID <- rbindlist(refTextLinesID, idcol = "file", use.names = TRUE)
  refTextLinesID[, dups := duplicated(lineText)]

  lapply(split(refTextLinesID, by = "file"), function(dupsTab, allModules) {
    if (any(dupsTab$dups)) {
      modLines <- allModules[[unique(dupsTab$file)]]
      modLines <- modLines[-dupsTab[which(dups), lineID]]

      ## if now we have two empty lines, remove one
      for (i in dupsTab[which(dups), lineID]) {
        if (all(modLines[c(i, i + 1)] == "")) {
          modLines <- modLines[-i]
        }
        if (all(modLines[c(i - 1, i)] == "")) {
          modLines <- modLines[-i]
        }
      }

      writeLines(modLines, con = unique(dupsTab$file))
    }
  }, allModules = allModules)

  return(copyModuleRmds)
}
