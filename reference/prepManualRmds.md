# Prepare module .Rmd to render book

Creates modified versions of the modules' .Rmd files, with YAML headers
removed and adapted knitr setup chunks

## Usage

``` r
prepManualRmds(modulePath, rebuildCache = FALSE, ignoreModules = NULL)
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

## Value

file paths of the modified module `.Rmd` files
