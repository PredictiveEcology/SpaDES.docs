test_that("manualChunkOptions() sets the shared printing conventions", {
  withr::local_options(width = 80, knitr.graphics.rel_path = TRUE)
  old <- knitr::opts_chunk$get()
  withr::defer(knitr::opts_chunk$set(old))

  manualChunkOptions(width = 60)

  expect_identical(getOption("width"), 60L)
  expect_identical(getOption("knitr.graphics.rel_path"), FALSE)
  expect_identical(knitr::opts_chunk$get("fig.pos"), "H")
  expect_identical(knitr::opts_chunk$get("tidy.opts"), list(width.cutoff = 60))
})

test_that("manualChunkOptions() picks the table format per output format", {
  withr::local_options(width = 80)
  old <- knitr::opts_chunk$get()
  withr::defer(knitr::opts_chunk$set(old))

  manualChunkOptions()

  ## a function, not a string: the same chapter is rendered to both
  expect_type(getOption("knitr.table.format"), "closure")
  expect_identical(getOption("knitr.table.format")(), "pipe")
})

test_that("manualVersion() reads Version by field, not by position", {
  d <- withr::local_tempdir(); withr::local_dir(d)
  ## Version deliberately last, which is what broke a positional read
  writeLines(c("Package: aManual", "Type: project", "Title: A Manual",
               "Version: 2.3.4"), "DESCRIPTION")
  withr::local_envvar(SPADES_MANUAL_VERSION = NA, LANDWEB_VERSION = NA)

  expect_message(v <- manualVersion(legacy = "LANDWEB_VERSION"), "v2.3.4")

  expect_identical(v, "2.3.4")
  expect_identical(Sys.getenv("SPADES_MANUAL_VERSION"), "2.3.4")
  expect_identical(Sys.getenv("LANDWEB_VERSION"), "2.3.4")
})

test_that("manualVersion() refuses a DESCRIPTION with no version", {
  d <- withr::local_tempdir(); withr::local_dir(d)
  writeLines(c("Package: aManual", "Title: A Manual"), "DESCRIPTION")

  expect_snapshot(error = TRUE, manualVersion())
})

test_that("manualVersion() says where it looked when there is no DESCRIPTION", {
  d <- withr::local_tempdir(); withr::local_dir(d)

  expect_snapshot(error = TRUE, manualVersion())
})

test_that("renderManual() errors when the site itself did not render", {
  d <- withr::local_tempdir(); withr::local_dir(d)
  dir.create("docs")
  ## stands in for a render that produced nothing
  local_mocked_bindings(render_book = function(...) invisible(NULL),
                        .package = "bookdown")

  expect_snapshot(error = TRUE, renderManual(outputDir = "docs"))
})

test_that("renderManual() keeps a site that rendered when a later format fails", {
  d <- withr::local_tempdir(); withr::local_dir(d)
  dir.create("docs")
  ## bookdown knits once and writes each format in turn, so the site is already
  ## on disk when a later one fails -- the PDF here
  local_mocked_bindings(
    render_book = function(...) {
      writeLines("<html></html>", "docs/index.html")
      stop("LaTeX failed to compile Manual.tex")
    },
    .package = "bookdown"
  )

  expect_message(complete <- renderManual(outputDir = "docs"),
                 "Manual render incomplete")

  expect_false(complete)
  expect_identical(readLines("docs/index.html"), "<html></html>")
})

test_that("renderManual() does not accept a stale site from an earlier build", {
  d <- withr::local_tempdir(); withr::local_dir(d)
  dir.create("docs")
  writeLines("from a previous build", "docs/index.html")
  local_mocked_bindings(render_book = function(...) invisible(NULL),
                        .package = "bookdown")

  expect_snapshot(error = TRUE, renderManual(outputDir = "docs"))
})

test_that("renderManual() reports success when every format rendered", {
  d <- withr::local_tempdir(); withr::local_dir(d)
  dir.create("docs")
  local_mocked_bindings(
    render_book = function(...) writeLines("<html></html>", "docs/index.html"),
    .package = "bookdown"
  )

  expect_true(renderManual(outputDir = "docs"))
})
