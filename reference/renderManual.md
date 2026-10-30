# Render a manual, without letting one format bring down the rest

Renders every format the book declares and keeps the site even when
another format fails, which a plain
[`bookdown::render_book()`](https://pkgs.rstudio.com/bookdown/reference/render_book.html)
call does not.

## Usage

``` r
renderManual(outputDir = "docs", index = "index.html", ...)
```

## Arguments

- outputDir:

  directory the book is rendered into, i.e. `output_dir` in
  `_bookdown.yml`.

- index:

  file within `outputDir` that the website must produce.

- ...:

  passed to
  [`bookdown::render_book()`](https://pkgs.rstudio.com/bookdown/reference/render_book.html),
  e.g. `output_format`.

## Value

`TRUE` if every format rendered, `FALSE` if one failed and was reported,
invisibly. Errors if the site did not render.

## Details

bookdown knits once and then writes each declared format in turn, so the
website is already on disk by the time a later format is attempted.
Letting a failure propagate therefore threw away a book that had
rendered: the LandR manual's deploy step was skipped and its published
site sat unchanged for 38 days because the PDF could not compile.

A failure is reported instead, as a GitHub Actions warning so it stays
visible on the run rather than scrolling past in a log, and the caller
carries on to deploy what did render. That is only safe because the site
itself is still required: `index` is removed first, so finding it
afterwards means this build produced it, and its absence is an error.
Without removing it, a stale copy from a previous local build would
satisfy the check.

Declare the fragile formats last in `_output.yml`. The formats are
written in the order declared and a failure stops the ones after it, so
with a PDF in the middle a broken PDF also cost the EPUB.

## Examples

``` r
if (FALSE) { # \dontrun{
## in build.R, in place of bookdown::render_book(output_format = "all", ...)
renderManual(outputDir = "docs")
} # }
```
