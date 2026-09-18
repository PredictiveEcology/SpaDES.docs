# Archive a rendered manual PDF alongside the book

Keeps a versioned copy of each released PDF. Does nothing when no PDF
was produced, which is the usual case while a manual renders HTML only –
copying unconditionally silently succeeds with `FALSE` and leaves an
empty archive.

## Usage

``` r
archiveManualPDF(
  pdf,
  version,
  prefix,
  archiveDir = file.path("archive", "pdf")
)
```

## Arguments

- pdf:

  path to the rendered PDF.

- version:

  version string for the archived file name.

- prefix:

  leading part of the archived file name; the result is
  `<prefix>-v<version>.pdf`.

- archiveDir:

  directory to copy into, created if needed.

## Value

path of the archived copy, or `NULL` if there was no PDF to archive,
invisibly.
