utils::globalVariables(c(
  ":=", "dups", "lineText", "lineID"
))

## ---- document structure ----------------------------------------------------

#' Label every line of an `.Rmd` by its role in the document
#'
#' The rewrites in [prepManualRmds()] each need to know whether a line sits
#' inside a fenced chunk. Deriving that separately per concern is what produced
#' a family of bugs: a prose mention of `root.dir` treated as a setting, a
#' `## References` inside a sentence treated as a heading, and a `(ref:key)` use
#' in mid-paragraph treated as a definition and deleted.
#'
#' Uses `knitr::all_patterns$md`, so a four-backtick or indented fence is
#' counted the way \pkg{knitr} counts it.
#'
#' @param lines character vector of lines, as from [readLines()].
#'
#' @return character vector the same length as `lines`, each element one of
#'   `"chunkHeader"`, `"chunkBody"`, `"chunkEnd"` or `"prose"`.
#'
#' @keywords internal
#' @rdname classifyRmdLines
classifyRmdLines <- function(lines) {
  cls <- rep("prose", length(lines))
  if (!length(lines)) {
    return(cls)
  }

  ## knitr's own patterns, so a four-backtick or indented fence counts the same
  ## way knitr counts it
  isHeader <- grepl(knitr::all_patterns$md$chunk.begin, lines)
  isEnd <- grepl(knitr::all_patterns$md$chunk.end, lines)

  inChunk <- FALSE
  for (i in seq_along(lines)) {
    if (!inChunk && isHeader[i]) {
      cls[i] <- "chunkHeader"
      inChunk <- TRUE
    } else if (inChunk && isEnd[i]) {
      cls[i] <- "chunkEnd"
      inChunk <- FALSE
    } else if (inChunk) {
      cls[i] <- "chunkBody"
    }
  }
  cls
}

#' Locate a module's setup chunk, creating one if it has none
#'
#' A module needs somewhere to carry `root.dir` and `cache.rebuild`; it does not
#' need to have supplied that place itself. The fireSense modules have no setup
#' chunk at all, and refusing them would stop the whole book from building.
#'
#' @param lines character vector of lines.
#' @param modName module name, used for the synthesized chunk's label.
#'
#' @return a list with `lines` (with a chunk inserted if there was none) and
#'   `at`, the index of the setup chunk header.
#'
#' @keywords internal
#' @rdname ensureSetupChunk
ensureSetupChunk <- function(lines, modName) {
  cls <- classifyRmdLines(lines)
  at <- which(cls == "chunkHeader" & grepl("\\{r[ ,]*setup", lines))

  if (length(at) > 1L) {
    stop("prepManualRmds(): ", modName, " has ", length(at),
         " setup chunks; expected at most one")
  }
  if (length(at)) {
    return(list(lines = lines, at = at))
  }

  ## after the chapter title if there is one, otherwise at the top
  title <- which(cls == "prose" & grepl("^# ", lines))
  after <- if (length(title)) title[1] else 0L
  lines <- append(lines,
                  c("", paste0("```{r setup-", gsub("_", "-", modName),
                               ", include = FALSE}"), "```"),
                  after = after)
  list(lines = lines, at = after + 2L)
}

#' Line indices belonging to the chunk opened at `at`
#'
#' @param lines character vector of lines.
#' @param at index of the chunk header.
#' @param cls line classes, as from [classifyRmdLines()].
#'
#' @return integer vector of indices, from the header to its closing fence, or
#'   to the end of the file if the chunk is never closed.
#'
#' @keywords internal
#' @rdname chunkLines
chunkLines <- function(lines, at, cls = classifyRmdLines(lines)) {
  close <- which(cls == "chunkEnd" & seq_along(lines) > at)
  seq.int(at, if (length(close)) close[1] else length(lines))
}

#' Text-reference definitions, as distinct from uses
#'
#' A \pkg{bookdown} text reference is its own paragraph. The same pattern in the
#' middle of a paragraph is a *use*, and removing it as a cross-chapter
#' duplicate deletes the sentence around it.
#'
#' @param lines character vector of lines.
#'
#' @return integer vector of line indices holding definitions.
#'
#' @keywords internal
#' @rdname textRefDefs
textRefDefs <- function(lines) {
  if (!length(lines)) {
    return(integer(0))
  }
  cls <- classifyRmdLines(lines)
  prev <- c("", lines[-length(lines)])
  prevCls <- c("prose", cls[-length(cls)])

  ## the first line of a prose block: preceded by a blank line, or by the HTML
  ## comment SpaDES.core's module template puts directly above the list (real
  ## LandR modules leave a blank line there, the template does not)
  ownBlock <- (!nzchar(trimws(prev)) | grepl("^[[:space:]]*<!--", prev)) &
    prevCls != "chunkBody"

  which(cls == "prose" & grepl("^\\(ref:[^)]+\\)", lines) & ownBlock)
}

## ---- per-module preparation -------------------------------------------------

#' Turn one module's `.Rmd` into a book chapter
#'
#' Copies `<module>/<module>.Rmd` to `<stagingPath>/<module>2.Rmd` and rewrites
#' the copy. Nothing is written into the module's own directory. The steps, in order,
#' because several depend on the one before:
#'
#' \enumerate{
#'   \item strip the YAML header, so the chapter inherits the book's output format;
#'   \item add a chapter title if the module does not open with one;
#'   \item find the setup chunk, or synthesize one ([ensureSetupChunk()]);
#'   \item force that chunk to `eval = TRUE, cache = FALSE`, so it runs again
#'         in the book even if the module set otherwise for standalone knitting;
#'   \item point `root.dir` at the module directory, injecting it if absent;
#'   \item set `cache.rebuild` from `rebuildCache`, injecting it if absent;
#'   \item move the References heading to the end, adding one if absent, and
#'         append the LaTeX `\\printbibliography` command.
#' }
#'
#' Steps 5 and 6 both inject after the chunk header, so `cache.rebuild` ends up
#' above `root.dir` in the generated chapter.
#'
#' @param x path to the module's own `.Rmd`.
#' @param rebuildCache passed through from [prepManualRmds()]; the value written
#'   into the chunk's `cache.rebuild` option.
#' @param stagingPath directory the chapter is written into. The module's own
#'   directory is never written to; `root.dir` still points at it.
#'
#' @return the path of the `<module>2.Rmd` written.
#'
#' @keywords internal
#' @rdname prepOneModuleRmd
prepOneModuleRmd <- function(x, rebuildCache, stagingPath) {
  modName <- basename(dirname(x))
  message(paste("Copying module", modName, "..."))
  copyModuleRmd <- file.path(stagingPath, paste0(modName, "2.Rmd"))
  if (!file.copy(x, copyModuleRmd, overwrite = TRUE)) {
    stop("prepManualRmds(): could not copy ", x, " to ", stagingPath)
  }

  lines <- readLines(copyModuleRmd, warn = FALSE)

  ## strip the YAML header: the first two delimiters, and only when nothing but
  ## whitespace precedes the first. Taking the range to the *last* delimiter
  ## deleted every line above any `---` thematic rule in the document.
  ids <- which(lines == "---")
  if (length(ids) >= 2L && !any(nzchar(trimws(lines[seq_len(ids[1] - 1L)])))) {
    lines <- lines[-seq(ids[1], ids[2])]
  }

  ## chapter title, if the module does not open with one
  nonEmpty <- lines[nzchar(trimws(lines))]
  if (!length(nonEmpty) || !grepl("^# ", nonEmpty[1])) {
    lines <- c("", paste0("# LandR *", modName, "* Module"), lines)
  }

  setup <- ensureSetupChunk(lines, modName)
  lines <- setup$lines
  at <- setup$at

  ## the setup chunk must run, and must not be served from cache
  opts <- lines[at]
  if (isFALSE(grepl("eval[[:space:]]*=[[:space:]]*TRUE", opts))) {
    opts <- if (grepl("eval", opts)) {
      sub("(.*)(eval[[:space:]]*=[[:space:]]*FALSE)(.*)\\}", "\\1eval = TRUE\\3\\}", opts)
    } else {
      sub("(.*)\\}", "\\1, eval = TRUE\\}", opts)
    }
  }
  if (isFALSE(grepl("cache[[:space:]]*=[[:space:]]*FALSE", opts))) {
    opts <- if (grepl("cache[^.]", opts)) {
      sub("(.*)(cache[[:space:]]*=[[:space:]]*)([[:alnum:]]+)(.*)\\}", "\\1\\2FALSE\\4\\}", opts)
    } else {
      sub("(.*)\\}", "\\1, cache = FALSE\\}", opts)
    }
  }
  lines[at] <- opts

  ## root.dir, within the setup chunk only
  inSetup <- chunkLines(lines, at)
  rootDirLine <- inSetup[grepl("root\\.dir", lines[inSetup])]
  if (length(rootDirLine) > 1L) {
    stop("prepManualRmds(): ", modName, " sets root.dir more than once in its setup chunk")
  }
  if (length(rootDirLine)) {
    code <- sub("(.*root\\.dir.*=[[:space:]]*)(.*)(\\))",
                paste0("\\1'", normPath(dirname(x)), "'\\3"),
                lines[rootDirLine])
    ## the substitution closes on the last ")" of the line it matched, so a call
    ## split over several lines would leave an orphaned ")" behind
    if (inherits(try(parse(text = code), silent = TRUE), "try-error")) {
      stop("prepManualRmds(): cannot rewrite root.dir in ", modName,
           ". It must be set on a single line, ",
           "e.g. knitr::opts_knit$set(root.dir = '..')")
    }
    lines[rootDirLine] <- code
  } else {
    lines <- append(lines,
                    paste0("knitr::opts_knit$set(root.dir = '",
                           normPath(dirname(x)), "')"),
                    after = at)
  }

  ## where stageFigure() puts this chapter's figures. A directory of the module's
  ## own, because modules reuse conventional names -- every one writes
  ## figures/moduleVersionBadge.png -- and one shared directory left each chapter
  ## showing the badge of the module knitted last. Setting it is also how
  ## stageFigure() knows the chapter is staged at all. knit() restores opts_knit
  ## when it returns, so the setting cannot outlive the book.
  lines <- append(lines,
                  paste0("knitr::opts_knit$set(SpaDES.docs.stageDir = '",
                         moduleStageDir(stagingPath, modName), "')"),
                  after = at)

  ## cache.rebuild, likewise within the setup chunk. A settable occurrence, not
  ## the word appearing anywhere: a module mentioning it only in a comment must
  ## still get one injected.
  inSetup <- chunkLines(lines, at)
  cacheLine <- inSetup[grepl(
    "cache\\.rebuild[[:space:]]*=[[:space:]]*(TRUE|FALSE)[[:space:]]*(,|\\}|\\))",
    lines[inSetup])]
  if (length(cacheLine)) {
    lines[cacheLine] <- sub("(.*cache\\.rebuild.*=[[:space:]]*)(TRUE|FALSE)(.*)",
                            paste0("\\1", rebuildCache, "\\3"), lines[cacheLine])
  } else {
    lines <- append(lines,
                    paste0("knitr::opts_chunk$set(cache.rebuild = ", rebuildCache, ")"),
                    after = at)
  }

  ## Literal markdown images in prose are resolved by pandoc against the BOOK
  ## ROOT, where bookdown merges the chapters -- not against the module directory
  ## the chapter came from. The `root.dir` set above cannot help: it is the
  ## directory chunks EVALUATE in, and a path written in prose is never evaluated.
  ##
  ## The image is copied in beside the staged chapter and the reference rewritten
  ## to point there. Staying INSIDE the book root is the part that matters: an
  ## absolute path renders locally and then publishes a dead link, because the
  ## deployed site has no /home/<user>/ to serve. The module's own .Rmd is left
  ## alone, so it still renders standalone from its own directory.
  lines <- stageModuleImages(lines, moduleDir = dirname(x), modName = modName,
                             stagingPath = stagingPath)

  ## the chapter bibliography goes last. A heading line only -- unanchored, this
  ## matched prose, and two matches made a length-2 `if` condition.
  cls <- classifyRmdLines(lines)
  bib <- which(cls == "prose" &
                 grepl("^#+[[:space:]]+References[[:space:]]*(\\{[^}]*\\})?[[:space:]]*$", lines))
  if (length(bib)) {
    bib <- bib[length(bib)]
    if (!bib %in% c(length(lines), length(lines) - 1L)) {
      lines <- c(lines[-bib], lines[bib])
    }
  } else {
    lines <- c(lines, "## References")
  }
  if (!any(grepl("printbibliography", lines, fixed = TRUE))) {
    lines <- c(lines, "\\printbibliography[segment=\\therefsegment,heading=none]")
  }

  writeLines(lines, con = copyModuleRmd)
  copyModuleRmd
}

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
#' @param bookdownYML path to the book's `_bookdown.yml`, which supplies the
#'  chapter order used when de-duplicating text references across chapters.
#'  Read from the working directory by default. A chapter that is prepared but
#'  not listed here is reported: it would otherwise be absent from the book
#'  without the build failing.
#'
#' @param stagingPath directory the generated chapters are written to, relative
#'  to the book root. Nothing is written into the module directories: a failed
#'  build used to leave a `<module>2.Rmd` in each one, dirtying every module
#'  checkout, and each module repository carried a `.gitignore` line to hide it.
#'  List the chapters from here in `_bookdown.yml`, and add this directory to
#'  the book's `.gitignore`.
#'
#' @return file paths of the modified module `.Rmd` files
#'
#' @export
#' @importFrom Require normPath
#' @importFrom data.table data.table rbindlist
#' @importFrom utils capture.output
prepManualRmds <- function(modulePath, rebuildCache = FALSE, ignoreModules = NULL,
                           bookdownYML = "_bookdown.yml",
                           stagingPath = "_manual_rmds") {
  ## checked before anything is written: a missing or unreadable book file used
  ## to surface only after every <module>2.Rmd had been created, leaving them
  ## behind for the caller to clean up
  if (!file.exists(bookdownYML)) {
    stop("prepManualRmds(): cannot find '", bookdownYML, "'. It is read from the ",
         "working directory unless `bookdownYML` says otherwise.")
  }
  rmdFiles <- unlist(yaml::read_yaml(bookdownYML)[["rmd_files"]], use.names = FALSE)

  moduleDirs <- list.dirs(modulePath, recursive = FALSE)

  ## whole module names. As a regex alternation over the whole path,
  ## "Biomass_core" also dropped "Biomass_coreTest", "mod" dropped everything via
  ## the `modules/` path component, and character(0) collapsed to "", which
  ## matches everything.
  if (length(ignoreModules)) {
    moduleDirs <- moduleDirs[!basename(moduleDirs) %in% ignoreModules]
  }

  moduleRmds <- file.path(moduleDirs, paste0(basename(moduleDirs), ".Rmd"))

  ## not every subdirectory is a module (hidden caches, retired modules)
  notModules <- !file.exists(moduleRmds)
  if (any(notModules)) {
    message("prepManualRmds(): skipping ", sum(notModules),
            " director(ies) with no <name>.Rmd: ",
            paste(basename(moduleDirs[notModules]), collapse = ", "))
    moduleRmds <- moduleRmds[!notModules]
  }

  ## nothing to do is not an error, but it must not reach the code below: an
  ## empty set makes sapply() return a list(), and the de-duplication pass then
  ## fails with "object 'lineText' not found" -- after every file has been
  ## written. See PredictiveEcology/SpaDES.docs#1.
  if (!length(moduleRmds)) {
    warning("prepManualRmds(): no modules to prepare in '", modulePath, "'",
            if (length(ignoreModules)) {
              paste0(" (after ignoring ", paste(ignoreModules, collapse = ", "), ")")
            } else "",
            call. = FALSE)
    return(character(0))
  }

  ## chapters from a previous run are cleared, so a module removed from the
  ## project does not linger as an orphan chapter
  dir.create(stagingPath, recursive = TRUE, showWarnings = FALSE)
  stale <- list.files(stagingPath, pattern = "2[.]Rmd$", full.names = TRUE)
  if (length(stale)) {
    file.remove(stale)
  }

  copyModuleRmds <- vapply(moduleRmds, prepOneModuleRmd, rebuildCache = rebuildCache,
                           stagingPath = stagingPath, FUN.VALUE = character(1))

  ## chapter order, for the chapters this call generated -- i.e. those the book
  ## lists from the staging directory. Parsed rather than pattern-matched:
  ## sub("  - ", ...) assumed exactly two spaces of indent, could not see the
  ## flow-style `rmd_files: [a, b]` form, and treated a commented-out line as a
  ## listed chapter.
  staged <- startsWith(rmdFiles, paste0(sub("/+$", "", stagingPath), "/"))
  bkdwnYMLsub <- normPath(rmdFiles[staged])

  allModules <- lapply(copyModuleRmds, readLines, warn = FALSE)
  names(allModules) <- normPath(copyModuleRmds)

  ## de-duplication only covers the chapters that are both listed and prepared.
  ## Say so: silently narrowing it lets a duplicate text reference through to a
  ## bookdown error later with no hint of why.
  listedNotPrepped <- setdiff(bkdwnYMLsub, names(allModules))
  if (length(listedNotPrepped)) {
    warning("prepManualRmds(): _bookdown.yml lists ", length(listedNotPrepped),
            " chapter(s) that were not prepared, so text references are not ",
            "de-duplicated against them: ",
            paste(basename(listedNotPrepped), collapse = ", "), call. = FALSE)
  }
  ## and the other direction. This one is quiet: the chapter is written, the
  ## build goes green, and the module is simply absent from the book. Cheap to
  ## do while both lists are in hand, and the likely mistake once a manual
  ## fetches its modules from a list rather than from git submodules.
  ## When the book lists nothing from the staging directory at all, the warning
  ## below says so more usefully than naming every module, so leave it to that.
  preppedNotListed <- setdiff(names(allModules), bkdwnYMLsub)
  if (length(bkdwnYMLsub) && length(preppedNotListed)) {
    warning("prepManualRmds(): prepared ", length(preppedNotListed),
            " chapter(s) that ", bookdownYML, " does not list, so they will not ",
            "appear in the book: ",
            paste(basename(preppedNotListed), collapse = ", "), call. = FALSE)
  }

  allModules <- allModules[intersect(bkdwnYMLsub, names(allModules))]

  ## nothing to de-duplicate against. The chapters are written and usable, so
  ## this is a warning, not an error -- it used to reach the table below and die
  ## with "object 'lineText' not found". See PredictiveEcology/SpaDES.docs#1.
  if (!length(allModules)) {
    warning("prepManualRmds(): '", bookdownYML, "' lists no chapters under '",
            stagingPath, "', so text references were not de-duplicated. ",
            "The chapters were still written.", call. = FALSE)
    return(copyModuleRmds)
  }

  refTextLinesID <- lapply(allModules, function(x) {
    ids <- textRefDefs(x)
    data.table(lineText = x[ids], lineID = ids)
  })
  refTextLinesID <- rbindlist(refTextLinesID, idcol = "file", use.names = TRUE)
  refTextLinesID[, dups := duplicated(lineText)]

  lapply(split(refTextLinesID, by = "file"), function(dupsTab, allModules) {
    if (any(dupsTab$dups)) {
      modLines <- allModules[[unique(dupsTab$file)]]

      ## drop in one pass, by mask over the original indices. The loop this
      ## replaces shortened the vector and then kept indexing it with the old
      ## line numbers.
      drop <- dupsTab[which(dups), lineID]

      ## where a removal leaves a blank line on each side, collapse the pair --
      ## at the removal site only, not across the whole chapter
      blank <- !nzchar(trimws(modLines))
      pad <- drop[drop > 1L & drop < length(modLines)]
      pad <- pad[blank[pad - 1L] & blank[pad + 1L]]
      modLines <- modLines[-c(drop, pad + 1L)]

      writeLines(modLines, con = unique(dupsTab$file))
    }
  }, allModules = allModules)

  copyModuleRmds
}

## Copy the images a chapter references in beside the staged chapter, and rewrite
## the references to match. Only prose is considered: rewriting inside a chunk
## would change what the module RUNS, and a chunk already evaluates with
## `root.dir` pointing at the module.
stageModuleImages <- function(lines, moduleDir, modName, stagingPath) {
  dest <- moduleStageDir(stagingPath, modName)
  ## a previous run's copies must not linger: an image dropped from the module
  ## would otherwise keep rendering from the stale copy
  unlink(dest, recursive = TRUE)

  cls <- classifyRmdLines(lines)
  idx <- which(cls == "prose" & grepl("![", lines, fixed = TRUE))
  missing <- character(0)
  for (i in idx) {
    at <- gregexpr("!\\[[^]]*\\]\\([^)]+\\)", lines[i])
    found <- regmatches(lines[i], at)[[1]]
    if (!length(found)) {
      next
    }
    done <- lapply(found, stageOneImage, moduleDir = moduleDir, dest = dest)
    missing <- c(missing, unlist(lapply(done, attr, "missing")))
    regmatches(lines[i], at) <- list(vapply(done, as.character, character(1)))
  }

  ## quiet is the wrong default here: the chapter renders, the book goes green,
  ## and the image is simply absent from the published page
  if (length(missing)) {
    message("prepManualRmds(): ", modName, " references ", length(missing),
            " image(s) that are not in the module, so they were left as-is: ",
            paste(unique(missing), collapse = ", "))
  }
  lines
}

## One `![alt](target)`. Returns the replacement, carrying a "missing" attribute
## when the module does not actually hold the file.
stageOneImage <- function(img, moduleDir, dest) {
  head <- sub("^(!\\[[^]]*\\]\\().*\\)$", "\\1", img)
  target <- sub("^!\\[[^]]*\\]\\((.*)\\)$", "\\1", img)
  ## an optional title travels with the target: ![alt](path "title")
  path <- sub("[[:space:]]+[\"'].*$", "", target)
  rest <- substring(target, nchar(path) + 1L)

  ## a URL, an absolute path, a fragment or an angle-bracketed target has no
  ## module-relative meaning to re-base
  if (!nzchar(path) || grepl("^([[:alpha:]][[:alnum:]+.-]*:|//|/|#|<)", path)) {
    return(img)
  }

  src <- file.path(moduleDir, path)
  if (!file.exists(src)) {
    ## e.g. downloaded by a setup chunk at render time; there is nothing to copy,
    ## and guessing a destination would only move the broken link
    return(structure(img, missing = path))
  }

  to <- file.path(dest, path)
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(src, to, overwrite = TRUE)) {
    stop("prepManualRmds(): could not copy ", src, " to ", to)
  }
  paste0(head, to, rest, ")")
}

## The directory a staged chapter's figures live in, relative to the book root.
## Shared by the prose images prepManualRmds() copies and the chunk figures
## stageFigure() copies, so a module's figures all end up in one place.
moduleStageDir <- function(stagingPath, modName) {
  file.path(sub("/+$", "", stagingPath), modName)
}
