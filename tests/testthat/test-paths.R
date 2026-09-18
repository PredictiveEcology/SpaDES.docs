localBookRoot <- function(outputDir = "docs", envir = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = envir)
  withr::local_dir(d, .local_envir = envir)
  writeLines(c("book_filename: test", paste0('output_dir: "', outputDir, '"'),
               "rmd_files:", "  - index.Rmd"), "_bookdown.yml")
  invisible(d)
}

test_that("manualPaths() reads output_dir from _bookdown.yml", {
  localBookRoot("docs")
  p <- manualPaths(prjDir = ".")
  expect_named(p, c("prj", "docs", "citations", "figures"))
  expect_identical(basename(p$docs), "docs")
  expect_true(dir.exists(p$citations))
  expect_true(dir.exists(p$figures))
})

test_that("manualPaths(create = FALSE) creates nothing", {
  localBookRoot()
  p <- manualPaths(prjDir = ".", create = FALSE)
  expect_false(dir.exists(p$citations))
  expect_false(dir.exists(p$figures))
})

test_that("manualPaths() errors when there is no _bookdown.yml", {
  d <- withr::local_tempdir(); withr::local_dir(d)
  expect_error(manualPaths(prjDir = "."), "no '_bookdown.yml'")
})

test_that("manualPaths() errors when _bookdown.yml sets no output_dir", {
  d <- withr::local_tempdir(); withr::local_dir(d)
  writeLines(c("book_filename: test", "rmd_files:", "  - index.Rmd"), "_bookdown.yml")
  ## silently defaulting here would render the book somewhere the deploy does not look
  expect_error(manualPaths(prjDir = "."), "does not set `output_dir`")
})

test_that("manualPaths() finds the project root when prjDir is not given", {
  skip_if_not_installed("rprojroot")
  localBookRoot()
  ## a git root is one of the markers rprojroot is asked for
  dir.create(".git")
  p <- manualPaths()
  expect_identical(normalizePath(p$prj, winslash = "/"),
                   normalizePath(getwd(), winslash = "/"))
})

test_that("manualPaths() errors when prjDir does not exist", {
  expect_error(manualPaths(prjDir = file.path(tempdir(), "no-such-dir")))
})

test_that("manualPaths() resolves a nested output_dir", {
  localBookRoot("./docs/site")
  p <- manualPaths(prjDir = ".", create = FALSE)
  expect_match(p$docs, "docs/site$")
})
