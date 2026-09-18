test_that("installModulePkgs() warns rather than installing nothing silently", {
  skip_if_not_installed("SpaDES.core")
  d <- withr::local_tempdir(); withr::local_dir(d)
  dir.create("modules")
  expect_warning(out <- installModulePkgs("modules"), "no modules found")
  expect_identical(out, character(0))
})
