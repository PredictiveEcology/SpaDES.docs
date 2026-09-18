test_that("prepManualRmds() strips the YAML header and keeps a thematic rule", {
  localBook("modA")
  writeModule("modA", "modules",
              body = c("Text above the rule.", "", "---", "", "Text below the rule."))

  out <- prepManualRmds("modules")
  chapter <- readLines(out)

  ## the header is gone
  expect_false(any(grepl("^title:", chapter)))
  expect_false(any(grepl("bookdown::html_document2", chapter)))

  ## the prose either side of the rule, and the rule itself, are untouched.
  ## taking the range from the first `---` to the last removed all three.
  expect_length(grep("Text above the rule.", chapter, fixed = TRUE), 1)
  expect_length(grep("Text below the rule.", chapter, fixed = TRUE), 1)
  expect_length(grep("^---$", chapter), 1)

  ## and the chapter title survives, which it cannot if the prose above the
  ## rule was deleted with the header
  expect_length(grep("^# modA Module$", chapter), 1)
})

test_that("prepManualRmds() leaves a module with no YAML header alone", {
  localBook("modB")
  writeModule("modB", "modules", body = c("Only prose.", "", "---", "", "More prose."),
              header = FALSE)

  chapter <- readLines(prepManualRmds("modules"))

  expect_length(grep("Only prose.", chapter, fixed = TRUE), 1)
  expect_length(grep("More prose.", chapter, fixed = TRUE), 1)
  expect_length(grep("^---$", chapter), 1)
})

test_that("prepManualRmds() writes one <module>2.Rmd per module and leaves sources intact", {
  localBook(c("modC", "modD"))
  for (m in c("modC", "modD")) writeModule(m, "modules", body = "Body.")

  out <- prepManualRmds("modules")

  expect_setequal(basename(out), c("modC2.Rmd", "modD2.Rmd"))
  expect_true(all(file.exists(out)))
  ## the module's own .Rmd keeps its header
  src <- readLines(file.path("modules", "modC", "modC.Rmd"))
  expect_length(grep("^---$", src), 2)
})

test_that("prepManualRmds() synthesizes a setup chunk when a module has none", {
  localBook("modE")
  writeModule("modE", "modules", body = "Body.", setup = FALSE)

  ## was: Error in 1:setupChunkStart : argument of length 0. Refusing instead
  ## would stop the whole book -- the five fireSense modules have no setup
  ## chunk, and the chapter still needs somewhere to carry root.dir.
  chapter <- readLines(prepManualRmds("modules"))
  expect_length(grep("^```\\{r setup-modE", chapter), 1)
  expect_length(grep("opts_knit\\$set\\(root\\.dir", chapter), 1)
})

test_that("prepManualRmds() errors clearly when a module has two setup chunks", {
  localBook("modF")
  writeModule("modF", "modules",
              body = c("```{r setup-again, include = FALSE}", "1 + 1", "```"))

  ## was: silently skipped every chunk-option fixup, then warned about a
  ## "numerical expression with 2 elements" and used the first
  expect_error(prepManualRmds("modules"), "setup chunks")
})

test_that("prepManualRmds() copes with more than one References heading", {
  localBook("modG")
  writeModule("modG", "modules",
              body = c("## References", "", "Mid-chapter text.", "", "## References"))

  ## was: Error: the condition has length > 1
  chapter <- readLines(prepManualRmds("modules"))
  expect_length(grep("printbibliography", chapter, fixed = TRUE), 1)
})

test_that("prepManualRmds() ignores a 'References' mention that is not a heading", {
  localBook("modH")
  writeModule("modH", "modules",
              body = c("See the ## References section below.", "", "## References"))

  chapter <- readLines(prepManualRmds("modules"))
  expect_length(grep("See the ## References section below.", chapter, fixed = TRUE), 1)
})

test_that("prepManualRmds() de-duplicates text references across chapters", {
  localBook(c("modI", "modJ"))
  refs <- c("(ref:a) alpha", "", "(ref:b) beta", "", "(ref:c) gamma")
  for (m in c("modI", "modJ")) writeModule(m, "modules", body = c("Body.", "", refs))

  ## was: Error: missing value where TRUE/FALSE needed -- the loop indexed a
  ## vector that the line above had already shortened
  out <- prepManualRmds("modules")
  first <- readLines(grep("modI", out, value = TRUE))
  second <- readLines(grep("modJ", out, value = TRUE))

  expect_length(grep("^\\(ref:[abc]\\)", first), 3)
  expect_length(grep("^\\(ref:[abc]\\)", second), 0)
})

test_that("prepManualRmds() does not delete prose that begins with a text reference", {
  localBook(c("modK", "modL"))
  ## a definition (its own paragraph) and a use (mid-paragraph), sharing a key
  body <- c("(ref:pct) %", "",
            "Paragraph start,", "(ref:pct) cover data in the study area.", "paragraph end.")
  for (m in c("modK", "modL")) writeModule(m, "modules", body = body)

  out <- prepManualRmds("modules")
  second <- readLines(grep("modL", out, value = TRUE))

  ## the duplicate *definition* goes
  expect_length(grep("^\\(ref:pct\\) %$", second), 0)
  ## the prose does not -- it is a use, not a definition
  expect_length(grep("cover data in the study area", second, fixed = TRUE), 1)
  expect_length(grep("Paragraph start,", second, fixed = TRUE), 1)
})

## ---- structural guards -----------------------------------------------------
## The rewrites below are line-wise regex over the setup chunk. Asserting on the
## text only catches the substitution someone thought about; asserting that the
## result still parses catches any that corrupts the chunk, which is the failure
## mode that actually breaks a book build.

chunkBody <- function(lines) {
  fences <- grep("^\\s*```", lines)
  if (length(fences) < 2L) return(character(0))
  lines[seq.int(fences[1] + 1L, fences[2] - 1L)]
}

test_that("the generated setup chunk still parses", {
  localBook("modM")
  writeModule("modM", "modules", body = "Body.")

  chapter <- readLines(prepManualRmds("modules"))
  expect_no_error(parse(text = chunkBody(chapter)))
})

test_that("the setup chunk parses when the module sets root.dir over several lines", {
  localBook("modN")
  writeModule("modN", "modules", body = "Body.")
  ## a multi-line opts_knit$set() -- the substitution matches the argument name
  ## on one line but closes the call on another
  rmd <- file.path("modules", "modN", "modN.Rmd")
  lines <- readLines(rmd)
  at <- grep("opts_chunk", lines)
  writeLines(append(lines, c("knitr::opts_knit$set(", "  root.dir = normalizePath('.')", ")"),
                    after = at), rmd)

  ## the rewrite closes on the last ")" of the line it matched, so it cannot
  ## handle this. It used to emit an orphaned ")" and break the chunk; now it says so.
  expect_error(prepManualRmds("modules"), "root.dir")
})

test_that("a numeric cache option survives as a valid chunk option", {
  localBook("modO")
  writeModule("modO", "modules", body = "Body.")
  rmd <- file.path("modules", "modO", "modO.Rmd")
  lines <- readLines(rmd)
  lines <- sub("include = FALSE}", "include = FALSE, cache = 2}", lines, fixed = TRUE)
  writeLines(lines, rmd)

  chapter <- readLines(prepManualRmds("modules"))
  header <- grep("^\\s*```\\{r setup", chapter, value = TRUE)
  ## cache must end up a bare logical, not something like `FALSE2`
  expect_match(header, "cache[[:space:]]*=[[:space:]]*(TRUE|FALSE)[,}]")
})

test_that("rebuildCache reaches the generated chapter", {
  localBook("modP")
  ## mentions cache.rebuild only in a comment: the old check looked at the whole
  ## file, found the word, and then injected nothing
  writeModule("modP", "modules",
              body = c("Body.", "", "<!-- set cache.rebuild = TRUE when metadata changes -->"))

  chapter <- readLines(prepManualRmds("modules", rebuildCache = TRUE))
  ## assert the *injected* call, not the word -- the fixture's own comment
  ## satisfies a looser pattern, which made an earlier version of this test pass
  ## while the option was still never set
  expect_length(grep("opts_chunk\\$set\\(cache\\.rebuild = TRUE\\)", chapter), 1)
})

## ---- contract and blast radius ---------------------------------------------

test_that("prepManualRmds() writes nothing into the module directory", {
  localBook("modQ")
  writeModule("modQ", "modules", body = "Body.")
  before <- list.files(file.path("modules", "modQ"), all.files = TRUE, no.. = TRUE)

  out <- prepManualRmds("modules")
  after <- list.files(file.path("modules", "modQ"), all.files = TRUE, no.. = TRUE)

  ## the module directories are git submodules in every project that uses this
  ## package; a build that touches them shows up as a dirty submodule
  expect_setequal(after, before)
  expect_setequal(basename(out), "modQ2.Rmd")
  expect_true(all(startsWith(normalizePath(out), normalizePath("_manual_rmds"))))
})

test_that("prepManualRmds() clears chapters left by a previous run", {
  localBook("modQ2")
  writeModule("modQ2", "modules", body = "Body.")
  dir.create("_manual_rmds", showWarnings = FALSE)
  writeLines("stale", file.path("_manual_rmds", "goneModule2.Rmd"))

  prepManualRmds("modules")

  ## a module removed from the project must not linger as an orphan chapter
  expect_false(file.exists(file.path("_manual_rmds", "goneModule2.Rmd")))
})

test_that("ignoreModules matches whole module names, not substrings", {
  localBook(c("modR", "modRExtra"))
  for (m in c("modR", "modRExtra")) writeModule(m, "modules", body = "Body.")

  ## _bookdown.yml still lists modR, so the de-duplication scope narrows and
  ## prepManualRmds() says so
  expect_warning(out <- prepManualRmds("modules", ignoreModules = "modR"),
                 "not de-duplicated")
  expect_setequal(basename(out), "modRExtra2.Rmd")
})

test_that("ignoreModules = character(0) ignores nothing", {
  localBook("modS")
  writeModule("modS", "modules", body = "Body.")

  out <- prepManualRmds("modules", ignoreModules = character(0))
  expect_setequal(basename(out), "modS2.Rmd")
})

## ---- shape lock ------------------------------------------------------------

test_that("a generated chapter has the expected shape", {
  localBook("modT")
  writeModule("modT", "modules",
              body = c("Body text.", "", "(ref:key) a caption", "", "## References"))

  chapter <- readLines(prepManualRmds("modules"))
  ## the module path is absolute and machine-specific; everything else is fixed
  expect_snapshot(
    cat(sub("root\\.dir = '.*'", "root.dir = '<moduledir>'", chapter), sep = "\n")
  )
})

## ---- regressions caught by review of the fixes above ------------------------

test_that("a References heading carrying a pandoc attribute is recognised", {
  localBook("modU")
  writeModule("modU", "modules", body = c("Body.", "", "## References {#modu-refs}"))

  chapter <- readLines(prepManualRmds("modules"))
  ## the real heading is kept and no bare duplicate is appended beside it
  expect_length(grep("^## References", chapter), 1)
  expect_length(grep("printbibliography", chapter, fixed = TRUE), 1)
})

test_that("a text reference directly under an HTML comment is a definition", {
  localBook(c("modV", "modW"))
  ## LandR modules open the list with a comment and no blank line after it
  body <- c("<!-- the following are text references -->", "(ref:pct) %", "", "Body.")
  for (m in c("modV", "modW")) writeModule(m, "modules", body = body)

  out <- prepManualRmds("modules")
  expect_length(grep("^\\(ref:pct\\)", readLines(grep("modV", out, value = TRUE))), 1)
  expect_length(grep("^\\(ref:pct\\)", readLines(grep("modW", out, value = TRUE))), 0)
})

test_that("de-duplication leaves blank lines inside code blocks alone", {
  localBook(c("modX", "modY"))
  body <- c("(ref:dup) shared", "", "```r", "a <- 1", "", "b <- 2", "```")
  for (m in c("modX", "modY")) writeModule(m, "modules", body = body)

  second <- readLines(grep("modY", prepManualRmds("modules"), value = TRUE))
  ## the last two fences are the ```r block; the first two are the setup chunk
  fences <- grep("^```", second)
  expect_length(fences, 4)
  block <- second[seq.int(fences[3] + 1L, fences[4] - 1L)]
  ## the blank line between the two statements survives
  expect_length(which(!nzchar(block)), 1)
})

test_that("a prose mention of root.dir does not derail the rewrite", {
  localBook("modZ")
  writeModule("modZ", "modules",
              body = c("Set `root.dir` if you knit this on its own.", "", "Body."))

  chapter <- readLines(prepManualRmds("modules"))
  expect_length(grep("opts_knit\\$set\\(root\\.dir", chapter), 1)
  expect_length(grep("Set `root.dir` if you knit", chapter, fixed = TRUE), 1)
})

test_that("prepManualRmds() warns and returns nothing when no modules match", {
  localBook("modAA")
  writeModule("modAA", "modules", body = "Body.")

  ## every module ignored: used to return a `named list()` -- the wrong type,
  ## silently. See #1.
  expect_warning(out <- prepManualRmds("modules", ignoreModules = "modAA"),
                 "no modules to prepare")
  expect_identical(out, character(0))
})

test_that("prepManualRmds() warns on a directory with no modules in it", {
  localBook("modAB")
  writeModule("modAB", "modules", body = "Body.")
  dir.create("empty")

  ## used to reach the de-duplication pass and die with
  ## "object 'lineText' not found". This is the failure reported in #1.
  expect_warning(out <- prepManualRmds("empty"), "no modules to prepare")
  expect_identical(out, character(0))
})

## ---- _bookdown.yml handling (#1) -------------------------------------------

test_that("prepManualRmds() warns when _bookdown.yml lists no chapters for this modulePath", {
  localBook("modAC")
  writeModule("modAC", "modules", body = "Body.")
  ## the case reported in #1: the module exists, but every module line in
  ## _bookdown.yml is commented out. Used to die in the de-duplication pass with
  ## "object 'lineText' not found", after the chapter had already been written.
  writeLines(c("book_filename: t", "rmd_files:", "  - index.Rmd",
               "  # - _manual_rmds/modAC2.Rmd"), "_bookdown.yml")

  expect_warning(out <- prepManualRmds("modules"), "lists no chapters")
  expect_setequal(basename(out), "modAC2.Rmd")
  expect_true(file.exists(out))
})

test_that("prepManualRmds() reads _bookdown.yml as YAML, not by indentation", {
  localBook("modAD")
  writeModule("modAD", "modules", body = c("(ref:k) shared", "", "Body."))
  ## four-space indent: the old sub("  - ", ...) matched exactly two
  writeLines(c("book_filename: t", "rmd_files:", "    - index.Rmd",
               "    - _manual_rmds/modAD2.Rmd"), "_bookdown.yml")

  expect_no_warning(out <- prepManualRmds("modules"))
  expect_setequal(basename(out), "modAD2.Rmd")
})

test_that("prepManualRmds() reads the flow-style rmd_files form", {
  localBook("modAE")
  writeModule("modAE", "modules", body = "Body.")
  writeLines(c("book_filename: t",
               'rmd_files: ["index.Rmd", "_manual_rmds/modAE2.Rmd"]'), "_bookdown.yml")

  expect_no_warning(out <- prepManualRmds("modules"))
  expect_setequal(basename(out), "modAE2.Rmd")
})

test_that("prepManualRmds() checks for _bookdown.yml before writing anything", {
  localBook("modAF")
  writeModule("modAF", "modules", body = "Body.")
  file.remove("_bookdown.yml")

  ## must fail before the write loop, or a failed run leaves <module>2.Rmd
  ## behind in the module directory
  expect_error(prepManualRmds("modules"), "_bookdown.yml")
  expect_false(dir.exists("_manual_rmds"))
})
