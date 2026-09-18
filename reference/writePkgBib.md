# Write a bibliography for the R packages in use

A manual cites the packages its modules depend on.
[`knitr::write_bib()`](https://rdrr.io/pkg/knitr/man/write_bib.html)
keys these `R-<package>`.

## Usage

``` r
writePkgBib(file = file.path("citations", "packages.bib"), packages = NULL)
```

## Arguments

- file:

  path to write to. Its directory is created if needed.

- packages:

  packages to cite. Defaults to `base` plus every package available in
  the current library paths, which is what a manual built in a
  disposable CI library wants.

## Value

`file`, invisibly.
