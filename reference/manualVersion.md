# The manual's version, from its DESCRIPTION

Reads `Version` and puts it in an environment variable, which is how a
manual's `index.Rmd` gets it onto the title page: the YAML header is
parsed before any of the book's code runs, so a subtitle can only reach
it through the environment.

## Usage

``` r
manualVersion(
  description = "DESCRIPTION",
  envVar = "SPADES_MANUAL_VERSION",
  legacy = c("LANDR_MAN_VERSION", "FIRESENSE_VERSION", "LANDWEB_VERSION")
)
```

## Arguments

- description:

  path to the DESCRIPTION file to read.

- envVar:

  name of the environment variable to set.

- legacy:

  additional variable names to set to the same value, for documents that
  still read a manual-specific name. `NULL` for none.

## Value

the version string, invisibly.

## Details

By field name, never by position. A manual that read `read.dcf(...)[4]`
got `Version` only because it happened to be the fourth field, so adding
any field above it would have printed the wrong string on the title
page.

`envVar` is also set under the name each manual used before this was
shared – `LANDR_MAN_VERSION`, `FIRESENSE_VERSION`, `LANDWEB_VERSION` –
so a chapter or module `.Rmd` still reading one of those keeps working.

## Examples

``` r
if (FALSE) { # \dontrun{
## in build.R, before render_book()
manualVersion()
} # }
```
