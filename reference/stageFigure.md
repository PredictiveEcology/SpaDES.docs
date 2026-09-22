# Put a module figure where the rendered document can find it

A module chapter is rendered from wherever the *document* lives: the
module's own directory when the module renders on its own, and the book
root once
[`prepManualRmds()`](https://predictiveecology.github.io/SpaDES.docs/reference/prepManualRmds.md)
has staged it into a manual. A relative path written for one of those is
wrong in the other, and it is the chapter that moves, not the figure.

## Usage

``` r
stageFigure(
  path,
  stageDir = knitr::opts_knit$get("SpaDES.docs.stageDir"),
  outputDir = knitr::opts_knit$get("output.dir")
)
```

## Arguments

- path:

  figure path(s) relative to the module directory, i.e. what the module
  uses when it renders on its own – for example
  `"figures/schematic.png"`. A vector is staged element by element.

- stageDir:

  directory the staged chapter's figures go in, relative to the book
  root. The default reads what
  [`prepManualRmds()`](https://predictiveecology.github.io/SpaDES.docs/reference/prepManualRmds.md)
  set for the chapter; pass it explicitly only when testing.

- outputDir:

  directory the document is being rendered in, i.e. the book root. The
  default asks knitr; pass it explicitly only when testing.

## Value

the path(s) to use in the reference: `path` unchanged when the module
renders on its own, otherwise the relative path(s) to the staged copies.

## Details

Writing an absolute path instead – via `normPath()`, say – makes the
render succeed and the *publication* fail. The path is correct only on
the machine that built the book, so the deployed site serves a dead link
from every chapter, and the build stays green while it happens.

When the chapter has been staged, this copies `path` into a directory of
the module's own beside the staged chapter, and returns the relative
path to that copy, so the file travels with the book into the published
output. The directory is per module because modules reuse conventional
names: every one writes `figures/moduleVersionBadge.png`, and a shared
directory would show each chapter the badge of whichever module was
knitted last.

When the module renders on its own, `path` is returned unchanged and
nothing is copied: its relative paths already resolve from its own
directory.
[`prepManualRmds()`](https://predictiveecology.github.io/SpaDES.docs/reference/prepManualRmds.md)
marks a staged chapter by setting the knitr option
`SpaDES.docs.stageDir` in its setup chunk; that option's absence is what
"rendering on its own" means here.

A figure fetched at render time is handled the same way: write it into
the module's own `figures/` first, as usual, then pass it through.

## See also

[`includeFigure()`](https://predictiveecology.github.io/SpaDES.docs/reference/includeFigure.md)
for a figure chunk: `include_graphics(stageFigure())` does not work from
a staged chapter.

## Examples

``` r
if (FALSE) { # \dontrun{
## a linked badge, in an `results = "asis"` chunk, instead of normPath():
cat(paste0("[![module-version](", stageFigure("figures/moduleVersionBadge.png"),
           ")](https://github.com/PredictiveEcology/Biomass_core)"))
} # }
```
