## A minimal module directory of the shape prepManualRmds() expects: one folder
## per module, each holding <module>.Rmd. `body` is spliced in after the setup
## chunk, which every module .Rmd written from the SpaDES.core template has and
## which prepManualRmds() requires.
writeModule <- function(name, modulePath, body = character(0), header = TRUE,
                        setup = TRUE) {
  dir.create(file.path(modulePath, name), recursive = TRUE, showWarnings = FALSE)
  yaml <- if (header) {
    c("---", paste0("title: \"", name, " Manual\""), "output: bookdown::html_document2", "---", "")
  } else {
    character(0)
  }
  writeLines(
    c(yaml,
      paste0("# ", name, " Module"),
      "",
      if (setup) c(paste0("```{r setup-", name, ", include = FALSE}"),
                   "knitr::opts_chunk$set(echo = TRUE)",
                   "```", ""),
      body),
    file.path(modulePath, name, paste0(name, ".Rmd"))
  )
}

## prepManualRmds() reads _bookdown.yml from the working directory and orders
## chapters by it, so a fixture needs both the book file and that directory.
localBook <- function(modules, modulePath = "modules", envir = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = envir)
  withr::local_dir(d, .local_envir = envir)
  dir.create(modulePath, showWarnings = FALSE)
  writeLines(
    c("book_filename: test", "rmd_files:", "  - index.Rmd",
      paste0("  - ", file.path(modulePath, modules, paste0(modules, "2.Rmd")))),
    "_bookdown.yml"
  )
  invisible(d)
}
