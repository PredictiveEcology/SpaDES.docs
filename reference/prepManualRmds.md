# Prepare module .Rmd to render book

Creates modified versions of the modules' .Rmd files, with YAML headers
removed and adapted knitr setup chunks

## Usage

``` r
prepManualRmds(
  modulePath,
  rebuildCache = FALSE,
  ignoreModules = NULL,
  bookdownYML = "_bookdown.yml",
  stagingPath = "_manual_rmds"
)
```

## Arguments

- modulePath:

  modules' folder directory, specified exactly as in `_bookdown.yml`.
  For instance, if in `_bookdown.yml` the list of module `.Rmd` files is
  provided as `- modules/XXX.Rmd`, `- ~/modules/XXX.Rmd`, or
  `- ../X/modules/XXX.Rmd`, then `modulePath` must be either `modules`,
  `~/modules` or `../X/modules` respectively. A `/` may be appended at
  the of `modulePath` (e.g. `~/modules/`). Note that all modules must be
  in the same directory.

- rebuildCache:

  should cached chunks be re-executed?

- ignoreModules:

  character vector of modules to ignore.

- bookdownYML:

  path to the book's `_bookdown.yml`, which supplies the chapter order
  used when de-duplicating text references across chapters. Read from
  the working directory by default. A chapter that is prepared but not
  listed here is reported: it would otherwise be absent from the book
  without the build failing.

- stagingPath:

  directory the generated chapters are written to, relative to the book
  root. Nothing is written into the module directories: a failed build
  used to leave a `<module>2.Rmd` in each one, dirtying every module
  checkout, and each module repository carried a `.gitignore` line to
  hide it. List the chapters from here in `_bookdown.yml`, and add this
  directory to the book's `.gitignore`.

## Value

file paths of the modified module `.Rmd` files
