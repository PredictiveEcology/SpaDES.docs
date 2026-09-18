test_that("prepManualRmds() strips the YAML header and keeps a thematic rule", {
  localBook("modA")
  writeModule("modA", "modules",
              body = c("Text above the rule.", "", "---", "", "Text below the rule."))

  out <- prepManualRmds("modules")
  chapter <- readLines(out)

  ## the header is gone
  expect_false(any(grepl("^title:", chapter)))
  expect_false(any(grepl("bookdown::html_document2", chapter)))

  ## the prose either side of the rule, and the rule itself, are untouched.
  ## taking the range from the first `---` to the last removed all three.
  expect_length(grep("Text above the rule.", chapter, fixed = TRUE), 1)
  expect_length(grep("Text below the rule.", chapter, fixed = TRUE), 1)
  expect_length(grep("^---$", chapter), 1)

  ## and the chapter title survives, which it cannot if the prose above the
  ## rule was deleted with the header
  expect_length(grep("^# modA Module$", chapter), 1)
})

test_that("prepManualRmds() leaves a module with no YAML header alone", {
  localBook("modB")
  writeModule("modB", "modules", body = c("Only prose.", "", "---", "", "More prose."),
              header = FALSE)

  chapter <- readLines(prepManualRmds("modules"))

  expect_length(grep("Only prose.", chapter, fixed = TRUE), 1)
  expect_length(grep("More prose.", chapter, fixed = TRUE), 1)
  expect_length(grep("^---$", chapter), 1)
})

test_that("prepManualRmds() writes one <module>2.Rmd per module and leaves sources intact", {
  localBook(c("modC", "modD"))
  for (m in c("modC", "modD")) writeModule(m, "modules", body = "Body.")

  out <- prepManualRmds("modules")

  expect_setequal(basename(out), c("modC2.Rmd", "modD2.Rmd"))
  expect_true(all(file.exists(out)))
  ## the module's own .Rmd keeps its header
  src <- readLines(file.path("modules", "modC", "modC.Rmd"))
  expect_length(grep("^---$", src), 2)
})
