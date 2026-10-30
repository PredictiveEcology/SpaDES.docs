test_that("wrapStrFun() breaks a long string into chunks of the given size", {
  expect_identical(
    wrapStrFun("abcdefgh", size = 3, breakChar = "|"),
    c(abcdefgh = "abc|def|gh")
  )
})

test_that("wrapStrFun() leaves a string no longer than size alone", {
  expect_identical(wrapStrFun("abc", size = 3), c(abc = "abc"))
  expect_identical(wrapStrFun("ab", size = 3), c(ab = "ab"))
})

test_that("wrapStrFun() passes NA through rather than breaking it", {
  ## a parameter table column with no description must not become "NA" text
  out <- wrapStrFun(c("abcdef", NA), size = 2, breakChar = "|")

  expect_identical(unname(out), c("ab|cd|ef", NA_character_))
})

test_that("wrapStrFun() handles a vector element by element", {
  out <- wrapStrFun(c("abcd", "ef"), size = 2, breakChar = "|")

  expect_identical(unname(out), c("ab|cd", "ef"))
  expect_named(out, c("abcd", "ef"))
})

test_that("addLabel() writes a bookdown label for HTML output", {
  localOptsKnit(out.format = "markdown")
  old <- knitr::opts_current$get()
  knitr::opts_current$set(label = "inputsTable")
  withr::defer(knitr::opts_current$restore(old))
  local_mocked_bindings(is_latex_output = function(...) FALSE)

  expect_identical(addLabel("A caption.", tag = "tab"),
                   "(\\#tab:inputsTable)A caption.")
})

test_that("addLabel() writes a LaTeX label when knitting to PDF", {
  old <- knitr::opts_current$get()
  knitr::opts_current$set(label = "inputsTable")
  withr::defer(knitr::opts_current$restore(old))
  ## the same chapter is rendered to both formats, and the label syntax differs
  local_mocked_bindings(is_latex_output = function(...) TRUE)

  expect_identical(addLabel("A caption.", tag = "fig"),
                   "\\label{fig:inputsTable}A caption.")
})

test_that("addLabel() takes the label from the chunk it is called in", {
  old <- knitr::opts_current$get()
  knitr::opts_current$set(label = "suffLight")
  withr::defer(knitr::opts_current$restore(old))
  local_mocked_bindings(is_latex_output = function(...) FALSE)

  expect_match(addLabel(), "suffLight", fixed = TRUE)
})

test_that("panble() refuses a chunk that is not results = 'asis'", {
  old <- knitr::opts_current$get()
  knitr::opts_current$set(results = "markup")
  withr::defer(knitr::opts_current$restore(old))
  ## without it the markdown is printed as a quoted string and the table is lost
  expect_snapshot(error = TRUE, panble(data.frame(x = 1)))
})

test_that("panble() returns a kable for HTML output", {
  old <- knitr::opts_current$get()
  knitr::opts_current$set(results = "asis", label = "tbl")
  withr::defer(knitr::opts_current$restore(old))
  local_mocked_bindings(is_latex_output = function(...) FALSE)

  out <- panble(data.frame(species = "Pinus", biomass = 1L), caption = "A caption.")

  expect_s3_class(out, "knitr_kable")
  expect_match(paste(out, collapse = "\n"), "Pinus", fixed = TRUE)
  expect_match(paste(out, collapse = "\n"), "<table", fixed = TRUE)
})

test_that("panble() applies kable_styling and column_spec arguments", {
  old <- knitr::opts_current$get()
  knitr::opts_current$set(results = "asis", label = "tbl")
  withr::defer(knitr::opts_current$restore(old))
  local_mocked_bindings(is_latex_output = function(...) FALSE)

  styled <- panble(
    data.frame(species = "Picea", biomass = 2L),
    kable_stylingArgs = list(full_width = FALSE),
    column_specArgs = list(column = 1, width = "3cm")
  )

  expect_s3_class(styled, "kableExtra")
  expect_match(paste(styled, collapse = "\n"), "3cm", fixed = TRUE)
})

test_that("panble() writes a pander table when knitting to PDF", {
  old <- knitr::opts_current$get()
  knitr::opts_current$set(results = "asis", label = "tbl")
  withr::defer(knitr::opts_current$restore(old))
  local_mocked_bindings(is_latex_output = function(...) TRUE)

  ## pander prints rather than returning, and the caption carries the label
  out <- capture.output(panble(data.frame(species = "Pinus"), caption = "A caption."))

  expect_match(paste(out, collapse = "\n"), "Pinus", fixed = TRUE)
  expect_match(paste(out, collapse = "\n"), "tab:tbl", fixed = TRUE)
})
