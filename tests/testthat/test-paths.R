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
