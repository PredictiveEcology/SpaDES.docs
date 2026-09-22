## A staged chapter is knitted with the module as the working directory (root.dir)
## and the book root as knitr's output.dir; prepManualRmds() also sets
## SpaDES.docs.stageDir. These tests reproduce that arrangement directly.
localStagedBook <- function(modules, .local_envir = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = .local_envir)
  withr::local_dir(d, .local_envir = .local_envir)
  dir.create("manual")
  for (m in modules) {
    dir.create(file.path("modules", m, "figures"), recursive = TRUE)
    ## each module writes DIFFERENT content under the SAME conventional name, as
    ## the real modules all do with figures/moduleVersionBadge.png
    writeLines(m, file.path("modules", m, "figures", "moduleVersionBadge.png"))
  }
  invisible(d)
}

test_that("stageFigure() keeps each module's figures apart", {
  d <- localStagedBook(c("modA", "modB"))

  refs <- c(modA = NA_character_, modB = NA_character_)
  for (m in names(refs)) {
    localOptsKnit(output.dir = file.path(d, "manual"),
                  SpaDES.docs.stageDir = file.path("_manual_rmds", m))
    refs[[m]] <- withr::with_dir(file.path("modules", m),
                                 stageFigure("figures/moduleVersionBadge.png"))
  }

  ## a single shared destination left every chapter showing the badge of whichever
  ## module was knitted last
  expect_false(identical(refs[["modA"]], refs[["modB"]]))
  withr::with_dir("manual", {
    expect_identical(readLines(refs[["modA"]]), "modA")
    expect_identical(readLines(refs[["modB"]]), "modB")
  })
})

test_that("stageFigure() returns a relative path that resolves from the book root", {
  d <- localStagedBook("modA")
  localOptsKnit(output.dir = file.path(d, "manual"),
                SpaDES.docs.stageDir = "_manual_rmds/modA")

  ref <- withr::with_dir("modules/modA", stageFigure("figures/moduleVersionBadge.png"))

  expect_false(startsWith(ref, "/"))
  expect_true(file.exists(file.path("manual", ref)))
})

test_that("stageFigure() stages every path it is given", {
  d <- localStagedBook("modA")
  writeLines("second", "modules/modA/figures/other.png")
  localOptsKnit(output.dir = file.path(d, "manual"),
                SpaDES.docs.stageDir = "_manual_rmds/modA")

  ## include_graphics() takes a vector, and module chapters pass one
  refs <- withr::with_dir("modules/modA",
                          stageFigure(c("figures/moduleVersionBadge.png", "figures/other.png")))

  expect_length(refs, 2L)
  expect_true(all(file.exists(file.path("manual", refs))))
})

test_that("stageFigure() drops a leading ./ from the reference it returns", {
  d <- localStagedBook("modA")
  localOptsKnit(output.dir = file.path(d, "manual"),
                SpaDES.docs.stageDir = "_manual_rmds/modA")

  ref <- withr::with_dir("modules/modA", stageFigure("./figures/moduleVersionBadge.png"))

  expect_identical(ref, "_manual_rmds/modA/figures/moduleVersionBadge.png")
})

test_that("stageFigure() copies nothing when the chapter has not been staged", {
  d <- localStagedBook("modA")
  ## knitr always reports SOME output.dir; only prepManualRmds() sets the stage
  ## directory, so its absence -- not a comparison of paths -- is what says the
  ## module is rendering on its own
  localOptsKnit(output.dir = file.path(d, "manual"), SpaDES.docs.stageDir = NULL)

  ref <- withr::with_dir("modules/modA", stageFigure("figures/moduleVersionBadge.png"))

  expect_identical(ref, "figures/moduleVersionBadge.png")
  expect_length(list.files("manual", recursive = TRUE), 0L)
})

test_that("stageFigure() says so when a staged figure is not there", {
  d <- localStagedBook("modA")
  localOptsKnit(output.dir = file.path(d, "manual"),
                SpaDES.docs.stageDir = "_manual_rmds/modA")

  ## silence here is what ships a chapter with a missing image and a green build
  expect_error(withr::with_dir("modules/modA", stageFigure("figures/never-downloaded.png")),
               "no figure at")
})
