test_that("stageFigure() copies the figure beside the rendered document", {
  d <- withr::local_tempdir()
  withr::local_dir(d)
  dir.create("modules/modA/figures", recursive = TRUE)
  dir.create("manual")
  file.create("modules/modA/figures/schematic.png")

  ## a chapter staged into a manual renders from the book root, while the chunk
  ## evaluates in the module directory (knitr root.dir)
  withr::with_dir("modules/modA", {
    expect_identical(stageFigure("figures/schematic.png",
                                 outputDir = file.path(d, "manual")),
                     "figures/schematic.png")
  })

  ## the returned path resolves FROM THE BOOK ROOT, which is what the reference
  ## in the rendered document is resolved against
  withr::with_dir("manual", {
    expect_true(file.exists("figures/schematic.png"))
  })
})

test_that("stageFigure() is a no-op when the module renders on its own", {
  d <- withr::local_tempdir()
  withr::local_dir(d)
  dir.create("figures")
  file.create("figures/schematic.png")

  ## rendering standalone, the document IS in the module directory, so there is
  ## nowhere to copy to and the same relative path already works
  expect_identical(stageFigure("figures/schematic.png", outputDir = d),
                   "figures/schematic.png")
  expect_length(list.files(".", recursive = TRUE), 1L)
})

test_that("stageFigure() says so when the figure is not there", {
  d <- withr::local_tempdir()
  withr::local_dir(d)
  dir.create("manual")

  ## silence here is what ships a chapter with a missing image and a green build
  expect_error(stageFigure("figures/never-downloaded.png",
                           outputDir = file.path(d, "manual")),
               "no figure at")
})

test_that("stageFigure() leaves the path alone when knitr says nothing", {
  d <- withr::local_tempdir()
  withr::local_dir(d)
  expect_identical(stageFigure("figures/schematic.png", outputDir = NULL),
                   "figures/schematic.png")
})
