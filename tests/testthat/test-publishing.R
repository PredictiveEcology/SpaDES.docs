test_that("stagePagesFiles() writes .nojekyll into the published directory", {
  d <- withr::local_tempdir()
  docs <- file.path(d, "docs"); dir.create(docs)

  expect_invisible(stagePagesFiles(docs))
  ## at the repository root it would never reach the deployed branch
  expect_true(file.exists(file.path(docs, ".nojekyll")))
  expect_false(file.exists(file.path(d, ".nojekyll")))
})

test_that("stagePagesFiles() writes CNAME only when a domain is given", {
  d <- withr::local_tempdir()
  docs <- file.path(d, "docs"); dir.create(docs)

  stagePagesFiles(docs)
  expect_false(file.exists(file.path(docs, "CNAME")))

  stagePagesFiles(docs, cname = "landr-manual.predictiveecology.org")
  expect_identical(readLines(file.path(docs, "CNAME")), "landr-manual.predictiveecology.org")
})

test_that("stagePagesFiles() rejects a cname with a scheme", {
  d <- withr::local_tempdir()
  docs <- file.path(d, "docs"); dir.create(docs)
  ## GitHub wants a bare domain; a URL here silently breaks the custom domain
  expect_error(stagePagesFiles(docs, cname = "https://example.org"), "bare domain")
})

test_that("stagePagesFiles() errors when the book was not rendered", {
  d <- withr::local_tempdir()
  expect_error(stagePagesFiles(file.path(d, "docs")), "does not exist")
})

test_that("archiveManualPDF() does nothing when no PDF was produced", {
  d <- withr::local_tempdir(); withr::local_dir(d)
  ## the HTML-only case: copying unconditionally returns FALSE and leaves nothing
  expect_message(out <- archiveManualPDF("docs/Manual.pdf", "1.0.4", "LandR-manual"),
                 "nothing archived")
  expect_null(out)
})

test_that("archiveManualPDF() copies to a versioned name", {
  d <- withr::local_tempdir(); withr::local_dir(d)
  dir.create("docs"); writeLines("x", "docs/Manual.pdf")

  out <- archiveManualPDF("docs/Manual.pdf", "1.0.4", "LandR-manual")
  expect_true(file.exists(out))
  expect_identical(basename(out), "LandR-manual-v1.0.4.pdf")
})

test_that("archiveManualPDF() refuses an empty version", {
  d <- withr::local_tempdir(); withr::local_dir(d)
  dir.create("docs"); writeLines("x", "docs/Manual.pdf")
  ## Sys.getenv() of an unset variable returns "", which would archive
  ## "LandR-manual-v.pdf" over the previous build
  expect_error(archiveManualPDF("docs/Manual.pdf", "", "LandR-manual"), "`version` is empty")
})

makeArchive <- function(dir, versions, prefix = "Test-manual") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  for (v in versions) writeLines("x", file.path(dir, sprintf("%s-v%s.pdf", prefix, v)))
}

test_that("publishManualArchive() copies the PDFs and indexes them newest first", {
  d <- withr::local_tempdir(); withr::local_dir(d)
  makeArchive("archive/pdf", c("1.0.0", "1.0.10", "1.0.2"))
  dir.create("docs")

  idx <- publishManualArchive("archive/pdf", "docs", "Test Manual")
  expect_true(file.exists(idx))
  expect_setequal(list.files("docs/archive/pdf"),
                  c("Test-manual-v1.0.0.pdf", "Test-manual-v1.0.10.pdf", "Test-manual-v1.0.2.pdf"))

  html <- paste(readLines(idx), collapse = "\n")
  ## numeric_version ordering, not string ordering: 1.0.10 is newer than 1.0.2
  order_seen <- regmatches(html, gregexpr("v[0-9.]+</a>", html))[[1]]
  expect_identical(order_seen, c("v1.0.10</a>", "v1.0.2</a>", "v1.0.0</a>"))
})

test_that("publishManualArchive() does nothing when there is no archive", {
  d <- withr::local_tempdir(); withr::local_dir(d)
  dir.create("docs")
  expect_message(out <- publishManualArchive("archive/pdf", "docs", "Test Manual"),
                 "nothing published")
  expect_null(out)
})

test_that("publishManualArchive() does nothing when the archive is empty", {
  d <- withr::local_tempdir(); withr::local_dir(d)
  dir.create("archive/pdf", recursive = TRUE); dir.create("docs")
  expect_message(out <- publishManualArchive("archive/pdf", "docs", "Test Manual"),
                 "no PDFs")
  expect_null(out)
})

test_that("publishManualArchive() names the manual in the page", {
  d <- withr::local_tempdir(); withr::local_dir(d)
  makeArchive("archive/pdf", "2.1.0"); dir.create("docs")
  idx <- publishManualArchive("archive/pdf", "docs", "LandR Manual")
  html <- paste(readLines(idx), collapse = "\n")
  expect_match(html, "LandR Manual &mdash; archived versions")
  expect_match(html, 'href="pdf/Test-manual-v2.1.0.pdf"')
})
