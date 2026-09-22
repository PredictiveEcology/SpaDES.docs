# Include a module figure from a chunk

The chunk form of
[`stageFigure()`](https://predictiveecology.github.io/SpaDES.docs/reference/stageFigure.md):
stages `path` and hands the result to
[`knitr::include_graphics()`](https://rdrr.io/pkg/knitr/man/include_graphics.html),
so a module chapter includes the same figure whether it renders on its
own or staged into a manual.

## Usage

``` r
includeFigure(path, ...)
```

## Arguments

- path:

  figure path(s) relative to the module directory, i.e. what the module
  uses when it renders on its own – for example
  `"figures/schematic.png"`. A vector is staged element by element.

- ...:

  further arguments to
  [`knitr::include_graphics()`](https://rdrr.io/pkg/knitr/man/include_graphics.html),
  e.g. `dpi`.

## Value

what
[`knitr::include_graphics()`](https://rdrr.io/pkg/knitr/man/include_graphics.html)
returns.

## Details

`knitr::include_graphics(stageFigure(path))` does not work from a staged
chapter. `include_graphics()` checks that the file exists relative to
the working directory, which for a staged chapter is the module's
(knitr's `root.dir`), while the path
[`stageFigure()`](https://predictiveecology.github.io/SpaDES.docs/reference/stageFigure.md)
returns is relative to the book root, where the rendered document
resolves it. So this turns that check off –
[`stageFigure()`](https://predictiveecology.github.io/SpaDES.docs/reference/stageFigure.md)
has already made the same check, against the right directory – and
passes everything else through.

Use
[`stageFigure()`](https://predictiveecology.github.io/SpaDES.docs/reference/stageFigure.md)
directly only to write the markdown yourself, e.g. for a linked badge,
`cat(paste0("[![alt](", stageFigure(p), ")](", url, ")"))`.

## Examples

``` r
if (FALSE) { # \dontrun{
## in a module chapter's figure chunk, instead of
## knitr::include_graphics(normPath("figures/schematic.png")):
includeFigure("figures/schematic.png")
} # }
```
