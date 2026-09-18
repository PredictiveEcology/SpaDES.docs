# Fetch a citation style

Downloads a Citation Style Language file from the Zotero style
repository, as referenced by a book's `csl:` field. Existing files are
kept, so a build does not depend on the network once the style is
present.

## Usage

``` r
downloadCSL(
  style = "ecology-letters",
  destDir = "citations",
  overwrite = FALSE
)
```

## Arguments

- style:

  style name as it appears in the Zotero repository, e.g.
  `"ecology-letters"`.

- destDir:

  directory to write into, created if needed.

- overwrite:

  whether to re-download when the file is already there.

## Value

path to the style file, invisibly.
