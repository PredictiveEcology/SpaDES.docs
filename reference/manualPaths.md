# Locate a manual's directories

Resolves the paths a manual's build script works in, so each one does
not repeat the same root-finding and `_bookdown.yml` parsing.

## Usage

``` r
manualPaths(prjDir = NULL, bookdownYML = "_bookdown.yml", create = TRUE)
```

## Arguments

- prjDir:

  the manual's root. Defaults to the enclosing RStudio project or git
  repository, found from [`getwd()`](https://rdrr.io/r/base/getwd.html).

- bookdownYML:

  the book's `_bookdown.yml`, relative to `prjDir`. Its `output_dir`
  gives the rendered book directory.

- create:

  whether to create the `citations` and `figures` directories. The
  rendered book directory is left to bookdown.

## Value

a named list of absolute paths: `prj`, `docs`, `citations` and
`figures`.
