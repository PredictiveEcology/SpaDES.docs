# Markdown links to a manual's archived versions

The "previous versions" list for a manual's landing page, built from the
files actually present rather than maintained by hand.

## Usage

``` r
manualArchiveLinks(
  archiveDir = file.path("archive", "pdf"),
  manualName,
  exclude = Sys.getenv("SPADES_MANUAL_VERSION"),
  urlPrefix = file.path("archive", "pdf")
)
```

## Arguments

- archiveDir:

  directory holding the archived PDFs.

- manualName:

  name of the manual, used in the link text.

- exclude:

  version to leave out, normally the one being built: it is the current
  version, not a previous one. `NULL` or `""` keeps all of them.

- urlPrefix:

  path the links are relative to, i.e. where
  [`publishManualArchive()`](https://predictiveecology.github.io/SpaDES.docs/reference/publishManualArchive.md)
  publishes the PDFs within the book.

## Value

a character vector of markdown list items, one per version, newest
first. Empty if there is nothing archived.

## Details

Every hand-maintained version of this list had drifted by the time it
was replaced: one manual's `index.Rmd` still named three releases in
commented-out markdown, and two listed versions whose files had moved.

The links are relative to the published book, where
[`publishManualArchive()`](https://predictiveecology.github.io/SpaDES.docs/reference/publishManualArchive.md)
has copied the PDFs, rather than absolute URLs into the repository. A
link to a repository's raw files stops working if it is renamed or made
private, and sends a reader off the site to fetch a file the site is
already serving.

## Examples

``` r
if (FALSE) { # \dontrun{
## in index.Rmd, in a `results = "asis"` chunk:
cat(manualArchiveLinks(manualName = "LandR Manual"), sep = "")
} # }
```
