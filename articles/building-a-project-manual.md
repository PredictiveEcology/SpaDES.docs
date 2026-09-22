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
it into a book, and the things that bite.

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
    #> knitr::opts_knit$set(root.dir = '/tmp/RtmpVGU3hT/exampleProject/modules/myModule')
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
`(ref:key) text` definitions, because two chapters defining the same key
is an error in the assembled book. Definitions are recognised by being
their own paragraph. A `(ref:key)` in the middle of a sentence is a use,
and is left alone.

## Wiring it into a build script

The pattern, reduced from LandWeb’s `manual/build.R`:

``` r

paths <- SpaDES.docs::manualPaths()

SpaDES.docs::writePkgBib(file.path(paths$citations, "packages.bib"))
SpaDES.docs::collapseModuleBibs(
  modulePath = file.path(paths$prj, "..", "modules"),
  extraBibs = file.path(paths$citations, "packages.bib"),
  outFile = file.path(paths$citations, "references.bib")
)

chapters <- SpaDES.docs::prepManualRmds("../modules", rebuildCache = FALSE)

bookdown::render_book(output_format = "all", envir = new.env())

SpaDES.docs::archiveManualPDF(
  file.path(paths$docs, "LandWebManual.pdf"),
  version = "1.0.0", prefix = "LandWeb-manual"
)

unlink("_manual_rmds", recursive = TRUE)   # the generated chapters are disposable
```

[`collapseModuleBibs()`](https://predictiveecology.github.io/SpaDES.docs/reference/collapseModuleBibs.md)
skips `.bib` files with no entries. A module that cites nothing yet
reasonably ships a comments-only placeholder, and
[`RefManageR::ReadBib()`](https://docs.ropensci.org/RefManageR/reference/ReadBib.html)
fails on a file with no entries – which is enough to take down a whole
manual.

## Things that bite

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
