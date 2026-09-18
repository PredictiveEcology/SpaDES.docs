utils::globalVariables(c(
  ":=", "dups", "lineText", "lineID"
))

#' Prepare module .Rmd to render book
#'
#' Creates modified versions of the modules' .Rmd files,
#'   with YAML headers removed and adapted knitr setup
#'   chunks
#'
#' @param modulePath modules' folder directory, specified exactly as in `_bookdown.yml`.
#'  For instance, if in `_bookdown.yml` the list of module `.Rmd` files is provided as
#'  `-  modules/XXX.Rmd`, `-  ~/modules/XXX.Rmd`, or `-  ../X/modules/XXX.Rmd`, then
#'  `modulePath` must be either `modules`, `~/modules` or `../X/modules` respectively.
#'  A `/` may be appended at the of `modulePath` (e.g. `~/modules/`).
#'  Note that all modules must be in the same directory.
#'
#' @param rebuildCache should cached chunks be re-executed?
#'
#' @param ignoreModules character vector of modules to ignore.
#'
#' @return file paths of the modified module `.Rmd` files
#'
#' @export
#' @importFrom Require normPath
#' @importFrom data.table data.table rbindlist
#' @importFrom utils capture.output
prepManualRmds <- function(modulePath, rebuildCache = FALSE, ignoreModules = NULL) {
  moduleDirs <- list.dirs(modulePath, recursive = FALSE)

  ## whole module names, not a regex alternation matched against the whole path.
  ## As a pattern, "Biomass_core" also dropped "Biomass_coreTest", "mod" dropped
  ## everything via the `modules/` path component, and character(0) collapsed to
  ## "" -- which matches everything.
  if (length(ignoreModules)) {
    moduleDirs <- moduleDirs[!basename(moduleDirs) %in% ignoreModules]
  }

  moduleRmds <- file.path(moduleDirs, paste0(basename(moduleDirs), ".Rmd"))

  ## not every subdirectory is a module (hidden caches, retired modules). Without
  ## this the copy silently returned FALSE and readLines() failed on a file that
  ## was never created.
  notModules <- !file.exists(moduleRmds)
  if (any(notModules)) {
    message("prepManualRmds(): skipping ", sum(notModules),
            " director(ies) with no <name>.Rmd: ",
            paste(basename(moduleDirs[notModules]), collapse = ", "))
    moduleRmds <- moduleRmds[!notModules]
  }

  copyModuleRmds <- sapply(moduleRmds, rebuildCache = rebuildCache,
                           FUN = function(x, rebuildCache) {
                             message(paste("Copying module", basename(dirname(x)), "..."))
                             copyModuleRmd <- sub("(.*)(\\.Rmd)", "\\12\\2", x)
                             file.copy(x, copyModuleRmd, overwrite = TRUE)

                             ## strip module.Rmd YAML headers -----
                             ## Only the header: the first two `---` delimiters, and only when
                             ## nothing but whitespace precedes the first. A `---` anywhere else
                             ## is a thematic rule, and taking the range from the first delimiter
                             ## to the last -- which is what modelr::seq_range(ids, by = 1) did --
                             ## deleted the header and every line of prose above that rule, with
                             ## no error. SpaDES.core::moduleRmdToVignette() reads the same file
                             ## this way.
                             linesModuleRmd <- readLines(copyModuleRmd)
                             ids <- which(linesModuleRmd == "---")
                             hasHeader <- length(ids) >= 2L &&
                               !any(nzchar(trimws(linesModuleRmd[seq_len(ids[1] - 1L)])))
                             if (hasHeader) {
                               linesModuleRmd <- linesModuleRmd[-seq(ids[1], ids[2])]
                             }

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
                             if (length(setupChunkStart) != 1L) {
                               ## every chunk-option fixup below writes into this one line, and the
                               ## root.dir and cache settings the chapter needs go with it. Zero used
                               ## to fail as `1:integer(0)`; two silently skipped every fixup.
                               stop("prepManualRmds(): ", basename(x), " has ", length(setupChunkStart),
                                    " setup chunk(s); expected exactly 1, opening with '```{r setup'")
                             }
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
                             ## inside the setup chunk only. Over the whole file a prose mention of
                             ## root.dir was enough to trigger the rewrite, or to abort the build.
                             chunkEnd <- grep("^[[:space:]]*```[[:space:]]*$", linesModuleRmd)
                             chunkEnd <- chunkEnd[chunkEnd > setupChunkStart]
                             chunkEnd <- if (length(chunkEnd)) chunkEnd[1] else length(linesModuleRmd)
                             setupChunkLines <- seq.int(setupChunkStart, chunkEnd)
                             existsRootDirsSetup <- any(grepl("root\\.dir", linesModuleRmd[setupChunkLines]))
                             if (existsRootDirsSetup) {
                               ## make sure the root.dir is the right one
                               rootDirLine <- setupChunkLines[grepl("root\\.dir", linesModuleRmd[setupChunkLines])]
                               dir2replace <- normPath(dirname(copyModuleRmd))
                               code2replace <-  sub("(.*root\\.dir.*=[[:space:]]*)(.*)(\\))",
                                                    paste0("\\1", "'",  dir2replace, "'", "\\3"),
                                                    linesModuleRmd[rootDirLine])
                               ## the substitution closes on the last ")" of the
                               ## matched line, so a call split over several lines
                               ## used to leave an orphaned ")" behind and the
                               ## chunk stopped parsing. Fail loudly instead.
                               if (length(rootDirLine) != 1L ||
                                   !nzchar(code2replace) ||
                                   inherits(try(parse(text = code2replace), silent = TRUE),
                                            "try-error")) {
                                 stop("prepManualRmds(): cannot rewrite root.dir in ",
                                      basename(x), ". It must be set on a single line, ",
                                      "e.g. knitr::opts_knit$set(root.dir = '..')")
                               }
                               linesModuleRmd[rootDirLine] <- code2replace
                             } else {
                               ## break lines into 2 to add a working dir setup line
                               beforeSetupChunkStart <- linesModuleRmd[seq_len(setupChunkStart)]
                               afterSetupChunkStart <- if (setupChunkStart < length(linesModuleRmd)) {
                                 linesModuleRmd[seq.int(setupChunkStart + 1L, length(linesModuleRmd))]
                               } else character(0)

                               addedCode <- paste0("knitr::opts_knit$set(root.dir = '", normPath(dirname(copyModuleRmd)), "')")

                               linesModuleRmd <- c(beforeSetupChunkStart, addedCode, afterSetupChunkStart)
                             }

                             ## add cache rebuild options for each .Rmd
                             ## branch on a settable occurrence, not on the word appearing anywhere.
                             ## A module that only mentions cache.rebuild in a comment took the
                             ## "already set" path, matched nothing, and injected nothing.
                             cacheRebuildLine <- which(grepl(
                               ",*[[:space:]]*cache.rebuild[[:space:]]*=[[:space:]]*(TRUE|FALSE)[[:space:]]*(,|\\})",
                               linesModuleRmd))
                             existsCacheRebuildSetup <- length(cacheRebuildLine) > 0L
                             if (existsCacheRebuildSetup) {
                               ## overwrite option
                               code2replace <- sub("(.*cache\\.rebuild.*=[[:space:]]*)(TRUE|FALSE)(.*)",
                                                   paste0("\\1", rebuildCache, "\\3"),
                                                   linesModuleRmd[cacheRebuildLine])
                               linesModuleRmd[cacheRebuildLine] <- code2replace
                             } else {
                               ## break lines into 2 to add a cache rebuild dir setup line (it doesn't matter if there
                               ## is another call to `knitr::opts_chunk$set`)
                               beforeSetupChunkStart <- linesModuleRmd[seq_len(setupChunkStart)]
                               afterSetupChunkStart <- if (setupChunkStart < length(linesModuleRmd)) {
                                 linesModuleRmd[seq.int(setupChunkStart + 1L, length(linesModuleRmd))]
                               } else character(0)

                               addedCode <- paste0("knitr::opts_chunk$set(cache.rebuild = ", rebuildCache, ")")

                               linesModuleRmd <- c(beforeSetupChunkStart, addedCode, afterSetupChunkStart)
                             }

                             ## if missing add chapter bibliography at the end of each module chapter:
                             ## a heading line only. The old pattern was unanchored, so a prose line
                             ## mentioning "## References" matched, and two matches made the `if`
                             ## below a length-2 condition -- an error on R >= 4.3.
                             chapterBibLine <- grep("^#{1,4}[[:space:]]+References[[:space:]]*(\\{[^}]*\\})?[[:space:]]*$",
                                                    linesModuleRmd)

                             ## if not in one of the last two lines, "move to the end"
                             if (length(chapterBibLine)) {
                               chapterBibLine <- chapterBibLine[length(chapterBibLine)]
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
    if (!length(x)) {
      return(data.table(lineText = character(0), lineID = integer(0)))
    }
    ## A bookdown text reference is its own paragraph. A line matching the same
    ## pattern but following a non-blank line is a *use* inside prose, and
    ## treating it as a definition deleted the sentence it was part of.
    prev <- c("", x[-length(x)])
    ## its own block: preceded by a blank line, an HTML comment or a heading.
    ## LandR modules open the list with `<!-- text references ... -->` and no
    ## blank line, so a blank-only rule missed the first definition.
    ownBlock <- !nzchar(trimws(prev)) |
      grepl("^[[:space:]]*(<!--|#)", prev)
    isDef <- grepl("^\\(ref:[^)]+\\)", x) & ownBlock
    data.table(lineText = x[isDef], lineID = which(isDef))
  })
  refTextLinesID <- rbindlist(refTextLinesID, idcol = "file", use.names = TRUE)
  refTextLinesID[, dups := duplicated(lineText)]

  lapply(split(refTextLinesID, by = "file"), function(dupsTab, allModules) {
    if (any(dupsTab$dups)) {
      modLines <- allModules[[unique(dupsTab$file)]]

      ## Drop in one pass, by mask. The loop this replaces removed the lines
      ## first and then indexed the shortened vector with the original line
      ## numbers, so every index past the first removal was wrong and could run
      ## off the end -- `all(NA == "")` is NA, and `if (NA)` aborts.
      drop <- dupsTab[which(dups), lineID]

      ## a removal that leaves a blank line on each side collapses the pair, at
      ## the removal site only -- a whole-chapter sweep would eat blank lines
      ## inside fenced code blocks elsewhere in the file
      blank <- !nzchar(trimws(modLines))
      pad <- drop[drop > 1L & drop < length(modLines)]
      pad <- pad[blank[pad - 1L] & blank[pad + 1L]]
      modLines <- modLines[-sort(unique(c(drop, pad + 1L)))]

      writeLines(modLines, con = unique(dupsTab$file))
    }
  }, allModules = allModules)

  return(copyModuleRmds)
}
