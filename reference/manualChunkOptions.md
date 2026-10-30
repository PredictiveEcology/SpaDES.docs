# Options every manual chapter is built with

The shared contents of a manual's `before_chapter_script`. Each manual
kept its own near-identical copy of these, which is how they drifted:
one set a CRAN repository, overriding the one CI configures, and one
removed dot-prefixed objects that a build script had deliberately named
with a dot so they would survive.

## Usage

``` r
manualChunkOptions(width = 60)
```

## Arguments

- width:

  column width for printed output.

## Value

`NULL`, invisibly; called for its effect on options.

## Details

`width` and the chunk options are the printing conventions the chapters
are written against – narrow output that fits the page, and figures
placed where they appear in the text rather than floated.

`knitr.table.format` is a function, not a string, because it is
consulted per chunk: the same chapter is rendered to LaTeX for the PDF
and to markdown for the website, and a table format fixed at either
breaks the other.

`knitr.graphics.rel_path = FALSE` keeps figure paths as given rather
than rewriting them relative to the chapter, which is what
[`stageFigure()`](https://predictiveecology.github.io/SpaDES.docs/reference/stageFigure.md)
relies on to put a module's figure where the book can find it.

## Examples

``` r
if (FALSE) { # \dontrun{
## the whole of a manual's _common.R:
SpaDES.docs::manualChunkOptions()
} # }
```
