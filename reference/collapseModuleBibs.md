# Collapse the modules' bibliographies into one

A project manual cites references that live in each module's own
`citations/references_<module>.bib`. bookdown wants a single
bibliography, so these are merged, together with any further files the
manual supplies – typically an auto-generated one for the R packages in
use, and the manual's own references.

## Usage

``` r
collapseModuleBibs(
  modulePath = "modules",
  extraBibs = character(0),
  outFile = file.path("citations", "references.bib")
)
```

## Arguments

- modulePath:

  directory holding the module directories, searched recursively for
  `references_*.bib`.

- extraBibs:

  further `.bib` files to merge in, in the order given. Files that do
  not exist are ignored, so a manual can list one that is only generated
  on some builds.

- outFile:

  path to write the merged bibliography to. Its directory is created if
  needed.

## Value

`outFile`, invisibly.

## Details

Files with no entries are skipped. A module that cites nothing yet
reasonably ships a comments-only `.bib`, and
[`RefManageR::ReadBib()`](https://docs.ropensci.org/RefManageR/reference/ReadBib.html)
fails on such a file with
`arguments imply differing number of rows: 0, 1`, which would otherwise
take a whole manual down over one placeholder.

`outFile` may also appear in `extraBibs`: every input is read before
anything is written, so a manual can accumulate into its own
bibliography as before.
