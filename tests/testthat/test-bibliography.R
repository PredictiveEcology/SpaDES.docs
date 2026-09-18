## A .bib holding only comments, which is what a module that cites nothing ships.
writeCommentBib <- function(path) {
  writeLines(c("% References cited in someModule.Rmd.",
               "% The module's own citation lives in ../citation.bib."), path)
}

writeEntryBib <- function(path, key) {
  writeLines(c(paste0("@Article{", key, ","),
               "  author = {A. Smith},",
               "  title = {A real entry},",
               "  journal = {Journal of Fire},",
               "  year = {2017},",
               "}"), path)
}

localModules <- function(envir = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = envir)
  withr::local_dir(d, .local_envir = envir)
  dir.create(file.path("modules", "modA", "citations"), recursive = TRUE)
  dir.create(file.path("modules", "modB", "citations"), recursive = TRUE)
  invisible(d)
}

test_that("collapseModuleBibs() skips .bib files with no entries", {
  skip_if_not_installed("RefManageR")
  localModules()
  writeEntryBib(file.path("modules", "modA", "citations", "references_modA.bib"), "Real:2017")
  ## modB cites nothing yet: ReadBib() errors on this file, which used to take
  ## the whole manual down. See PredictiveEcology/fireSenseManual#7.
  writeCommentBib(file.path("modules", "modB", "citations", "references_modB.bib"))

  expect_message(out <- collapseModuleBibs(outFile = "citations/references.bib"),
                 "no entries")
  expect_true(file.exists(out))
  expect_length(RefManageR::ReadBib(out), 1L)
  expect_setequal(names(RefManageR::ReadBib(out)), "Real:2017")
})

test_that("collapseModuleBibs() merges every module's entries", {
  skip_if_not_installed("RefManageR")
  localModules()
  writeEntryBib(file.path("modules", "modA", "citations", "references_modA.bib"), "One:2017")
  writeEntryBib(file.path("modules", "modB", "citations", "references_modB.bib"), "Two:2018")

  ## WriteBib() reports its own progress, so the assertion is that nothing was
  ## skipped, not that the call is silent
  msgs <- capture_messages(out <- collapseModuleBibs(outFile = "citations/references.bib"))
  expect_false(any(grepl("no entries", msgs)))
  expect_setequal(names(RefManageR::ReadBib(out)), c("One:2017", "Two:2018"))
})

test_that("collapseModuleBibs() takes extraBibs, and ignores ones that do not exist", {
  skip_if_not_installed("RefManageR")
  localModules()
  writeEntryBib(file.path("modules", "modA", "citations", "references_modA.bib"), "One:2017")
  dir.create("citations")
  writeEntryBib(file.path("citations", "packages.bib"), "Pkg:2020")

  out <- collapseModuleBibs(
    extraBibs = c("citations/packages.bib", "citations/never-generated.bib"),
    outFile = "citations/references.bib"
  )
  expect_setequal(names(RefManageR::ReadBib(out)), c("One:2017", "Pkg:2020"))
})

test_that("collapseModuleBibs() warns and writes an empty bibliography when nothing has entries", {
  skip_if_not_installed("RefManageR")
  localModules()
  writeCommentBib(file.path("modules", "modA", "citations", "references_modA.bib"))

  expect_warning(out <- collapseModuleBibs(outFile = "citations/references.bib"),
                 "no .bib file with any entries")
  expect_true(file.exists(out))
  expect_equal(file.size(out), 0)
})

test_that("collapseModuleBibs() returns the output path invisibly", {
  skip_if_not_installed("RefManageR")
  localModules()
  writeEntryBib(file.path("modules", "modA", "citations", "references_modA.bib"), "One:2017")

  expect_invisible(collapseModuleBibs(outFile = "citations/references.bib"))
})
