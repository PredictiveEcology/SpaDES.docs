# Building a project manual

A SpaDES module documents itself in `<module>/<module>.Rmd`. That file
is written to be knitted on its own, from beside the module. A project
manual is a [bookdown](https://bookdown.org) book that gathers several
of those files into one document, with continuous chapter numbering, one
bibliography and a PDF.

This vignette is about a manual that documents the modules **as a
particular project used them**, so it pins them. A manual that instead
tracks the modules as they are now, rebuilding itself as they change, is
a different arrangement – see
[`vignette("building-a-continuously-updated-manual")`](https://predictiveecology.github.io/SpaDES.docs/articles/building-a-continuously-updated-manual.md).
The difference is not the book; it is whether the modules are pinned or
tracked, and almost everything else follows from that.

The two are not the same document, and the differences are not cosmetic.
A chapter has no YAML header of its own, its cross-reference labels
share a namespace with every other chapter, and its working directory is
the book’s rather than the module’s.
[`prepManualRmds()`](https://predictiveecology.github.io/SpaDES.docs/reference/prepManualRmds.md)
is what reconciles them. This vignette covers what it does, how to wire
it into a book, and the constraints to work within.

## The shape of a project manual

The layout every PredictiveEcology manual uses, and the one this package
assumes:

    <project>/
      modules/            # the modules, pinned as git submodules
        Biomass_core/
          Biomass_core.Rmd
      manual/             # the book
        index.Rmd
        _bookdown.yml
        _output.yml
        build.R
        _manual_rmds/     # generated chapters; gitignored
      docs/               # rendered output, served by GitHub Pages

[LandWeb](https://github.com/PredictiveEcology/LandWeb) is the reference
implementation; `manual/build.R` there is the script this vignette
generalises. The submodules are the point: they record the module
commits the project ran, so the manual and the results it describes
agree.

## A minimal worked example

Enough of a project to run the real thing. One module, with the pieces
[`prepManualRmds()`](https://predictiveecology.github.io/SpaDES.docs/reference/prepManualRmds.md)
cares about: a setup chunk, a text reference, and a References heading.

``` r

prj <- file.path(tempdir(), "exampleProject")
dir.create(file.path(prj, "modules", "myModule"), recursive = TRUE)
dir.create(file.path(prj, "manual"), recursive = TRUE)

writeLines(c(
  "---", "title: \"myModule Manual\"", "output: bookdown::html_document2", "---", "",
  "# myModule Module", "",
  "```{r setup-myModule, include = FALSE}",
  "knitr::opts_chunk$set(echo = TRUE)",
  "```", "",
  "(ref:mymod) *myModule*", "",
  "Some documentation for (ref:mymod).", "",
  "## References"
), file.path(prj, "modules", "myModule", "myModule.Rmd"))
```

The book lists its chapters, including the generated ones, in
`_bookdown.yml`:

``` r

writeLines(c(
  "book_filename: exampleManual",
  "output_dir: \"../docs\"",
  "rmd_files:",
  "  - index.Rmd",
  "  - _manual_rmds/myModule2.Rmd"
), file.path(prj, "manual", "_bookdown.yml"))
```

[`prepManualRmds()`](https://predictiveecology.github.io/SpaDES.docs/reference/prepManualRmds.md)
runs from the book root, because that is where `_bookdown.yml` lives and
where bookdown knits from:

``` r

## knitr restores the working directory after each chunk, so do the whole thing
## in one and keep absolute paths
owd <- setwd(file.path(prj, "manual"))
chapters <- normalizePath(SpaDES.docs::prepManualRmds(modulePath = "../modules"))
#> Copying module myModule ...
setwd(owd)

basename(chapters)
#> [1] "myModule2.Rmd"
```

The generated chapter, with the header stripped and the setup chunk
rewritten:

``` r

writeLines(head(readLines(chapters[1]), 8))
```

    #> 
    #> # myModule Module
    #> 
    #> ```{r setup-myModule, include = FALSE, eval = TRUE, cache = FALSE}
    #> knitr::opts_chunk$set(cache.rebuild = FALSE)
    #> knitr::opts_knit$set(SpaDES.docs.stageDir = '_manual_rmds/myModule')
    #> knitr::opts_knit$set(root.dir = '/tmp/RtmpLDraTQ/exampleProject/modules/myModule')
    #> knitr::opts_chunk$set(echo = TRUE)

## What it does to each module `.Rmd`

In order, because several steps depend on the one before:

| step | why |
|----|----|
| strip the YAML header | the chapter inherits the book’s output format from `_output.yml` |
| add a chapter title if absent | so the chapter appears in the book’s table of contents |
| find or synthesize a setup chunk | somewhere to put the two settings below |
| force `eval = TRUE, cache = FALSE` | the module may have set otherwise for standalone knitting |
| point `root.dir` at the module | so `moduleInputs("X", "..")` and friends resolve |
| set `cache.rebuild` | from the `rebuildCache` argument |
| move References to the end, add `\printbibliography` | chapter-level bibliographies |

Across chapters it also de-duplicates bookdown *text references*, the
`(ref:key) text` definitions, because several chapters defining the same
key makes bookdown warn `Possibly duplicated text reference labels` and
leaves it to chance which definition wins. Definitions are recognised by
being their own paragraph; a `(ref:key)` in the middle of a sentence is
a use, and is left alone.

bookdown’s own rule is broader: it treats *any* line beginning
`(ref:key)` as a definition, whether or not it is its own paragraph.
Prose that wraps so a *use* lands first on a line therefore becomes a
second definition, which
[`prepManualRmds()`](https://predictiveecology.github.io/SpaDES.docs/reference/prepManualRmds.md)
leaves alone because it is not its own block:

    FAIR datasets [*sensu* @StallEtAl2019] on stand biomass and species
    (ref:percent) cover changes across Canada.

That reads as a use of `(ref:percent)`, but bookdown takes the rest of
the line as its definition. When a duplicate-label warning persists
after de-duplication, look for a label at the start of a line in a
module’s prose and re-wrap it so the label sits mid-line.

## Wiring it into a build script

The pattern, reduced from LandWeb’s `manual/build.R`:

``` r

## prjDir explicitly: manualPaths() resolves the project root and expects
## _bookdown.yml beside it, and in this layout the book is in manual/ while the
## root is the repository above it
prjDir <- SpaDES.config::findProjectPath(from_wd = FALSE)
manDir <- file.path(prjDir, "manual")
paths <- SpaDES.docs::manualPaths(prjDir = manDir)

SpaDES.docs::writePkgBib(file.path(paths$citations, "packages.bib"))
SpaDES.docs::collapseModuleBibs(
  modulePath = file.path(prjDir, "modules"),
  extraBibs = file.path(paths$citations, c("packages.bib", "references_manual.bib")),
  outFile = file.path(paths$citations, "references.bib")
)

withr::with_dir(manDir, {
  SpaDES.docs::manualChunkOptions()          # or call it from _common.R
  version <- SpaDES.docs::manualVersion("../DESCRIPTION")

  chapters <- SpaDES.docs::prepManualRmds("../modules", rebuildCache = FALSE)

  SpaDES.docs::renderManual(outputDir = paths$docs, output_format = "all")

  SpaDES.docs::archiveManualPDF(
    file.path(paths$docs, "LandWebManual.pdf"),
    version = version, prefix = "LandWeb-manual"
  )

  ## the generated chapters are disposable. Keyed on where they were actually
  ## staged, rather than a hardcoded "_manual_rmds"
  unlink(unique(dirname(chapters)), recursive = TRUE)
})
```

`references.bib` is **output only**. Listing it in its own `extraBibs`
looks like “the manual accumulates its references”, and what it actually
does is merge the file into itself on every build: one manual’s copy had
grown to 265 entries, 216 of them RefManageR’s `:1`-style duplicate
markers. Keep curated entries in a separate tracked file –
`references_manual.bib` above – and let the output be disposable. It has
to be, because `citations/` is generated and starts empty on CI.

[`manualVersion()`](https://predictiveecology.github.io/SpaDES.docs/reference/manualVersion.md)
reads `Version` from DESCRIPTION by field name. Setting the version in
the build script instead gives two places to edit, and reading it by
position breaks as soon as a field is added above it.

[`collapseModuleBibs()`](https://predictiveecology.github.io/SpaDES.docs/reference/collapseModuleBibs.md)
skips `.bib` files with no entries. A module that cites nothing yet
reasonably ships a comments-only placeholder, and
[`RefManageR::ReadBib()`](https://docs.ropensci.org/RefManageR/reference/ReadBib.html)
fails on a file with no entries – which is enough to take down a whole
manual.

## Constraints

**Add the staging directory to the book’s `.gitignore`.** The generated
chapters are build output. Before they were staged there they went into
each module’s own directory, and in these projects those are git
submodules. Every module repository therefore needed its own
`.gitignore` line, and a failed build left a file in each one.

**Chapter order comes from `_bookdown.yml`, not from the filesystem.**
[`prepManualRmds()`](https://predictiveecology.github.io/SpaDES.docs/reference/prepManualRmds.md)
warns in both directions: a chapter listed but not prepared is not
de-duplicated against the others, and a chapter prepared but not listed
is written, builds cleanly, and is simply absent from the book.

**Run it from the book root.** `_bookdown.yml` is read from the working
directory by default; pass `bookdownYML` if it is elsewhere.

**Cross-reference labels are global.** Two chapters using
`\@ref(tab:inputs)` collide. The `newModule()` template suffixes labels
with the module name for this reason; keep that if you edit them.

**A module’s figures do not move with its chapter.** The chapter is read
from the book root once it is staged, so a path that worked from the
module’s own directory no longer resolves. Use
[`includeFigure()`](https://predictiveecology.github.io/SpaDES.docs/reference/includeFigure.md)
in a figure chunk, and
[`stageFigure()`](https://predictiveecology.github.io/SpaDES.docs/reference/stageFigure.md)
where a chunk writes the markdown itself, such as a linked badge:

``` r

## in a module chapter
SpaDES.docs::includeFigure("figures/schematic.png")

cat(paste0("[![version](", SpaDES.docs::stageFigure("figures/badge.png"), ")](", url, ")"))
```

Do not write an absolute path instead, via `normPath()` or similar: it
is correct only on the machine that built the book, so the chapter
renders locally and publishes a dead link. The build stays green while
every published chapter serves `src="/home/runner/work/..."`.

**`bs4_book` has no download menu.** `download: ["pdf", "epub"]` is a
`gitbook` option. Setting it under
[`bookdown::bs4_book`](https://pkgs.rstudio.com/bookdown/reference/bs4_book.html)
is accepted and ignored, so the files are published with nothing linking
to them. Link them from the landing page instead, alongside
[`manualArchiveLinks()`](https://predictiveecology.github.io/SpaDES.docs/reference/manualArchiveLinks.md)
for the released ones.
