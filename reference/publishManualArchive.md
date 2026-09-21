# Publish a manual's archived releases alongside the site

A manual that is released keeps a PDF of each release. Those PDFs belong
in version control rather than only on the published branch: that branch
is rebuilt by every deploy, and an old PDF cannot be regenerated from
current sources. This copies them into the rendered book directory so
they deploy with the site, and writes an index page listing them.

## Usage

``` r
publishManualArchive(
  archiveDir = file.path("archive", "pdf"),
  docsDir,
  manualName,
  subdir = "archive"
)
```

## Arguments

- archiveDir:

  directory holding the archived PDFs, usually tracked in the
  repository.

- docsDir:

  the rendered book directory, i.e. `_bookdown.yml`'s `output_dir`.

- manualName:

  name of the manual, used in the index page's headings and link text.

- subdir:

  directory within `docsDir` to publish into.

## Value

path to the index page, invisibly, or `NULL` if there was nothing to
publish.

## Details

The index is built from the files actually present, not from a list kept
by hand, so it cannot drift. Versions are read from the file names,
which are what
[`archiveManualPDF()`](https://predictiveecology.github.io/SpaDES.docs/reference/archiveManualPDF.md)
writes: `<prefix>-v<version>.pdf`. Anything not matching that shape is
copied but left out of the ordering.
