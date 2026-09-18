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
