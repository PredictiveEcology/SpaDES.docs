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

path of the archived copy, whether this call wrote it or found it
already there, or `NULL` if there was no PDF to archive, invisibly.

## Details

An archived version is never overwritten. A release PDF is a record of
what that version looked like, so the first copy is the one that counts,
and a later build at the same version leaves it alone.

That is also the only workable test. A PDF is not reproducible byte for
byte: xelatex stamps a creation date and a document ID into every run,
so the same sources archive differently each time. A caller that commits
whatever changed under `archiveDir` therefore saw a modified file after
every build, and overwriting made a release PDF depend on which build
touched it last. Keyed on the file's existence, archiving happens once
per version, which is what a release means.
