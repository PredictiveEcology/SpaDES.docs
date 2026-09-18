# Building a project manual

A SpaDES module documents itself in `<module>/<module>.Rmd`. That file
is written to be knitted on its own, from beside the module. A project
manual is a [bookdown](https://bookdown.org) book that gathers several
of those files into one document, with continuous chapter numbering, one
bibliography and a PDF.

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
      modules/            # the modules, usually git submodules
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
generalises.
[fireSenseManual](https://github.com/PredictiveEcology/fireSenseManual)
is the variant where the manual is its own repository rather than a
directory inside a project.

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
    #> knitr::opts_knit$set(root.dir = '/tmp/RtmpZd2oqr/exampleProject/modules/myModule')
    #> knitr::opts_chunk$set(echo = TRUE)
    #> ```

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

prjDir <- SpaDES.project::findProjectPath()
manDir <- file.path(prjDir, "manual")
setwd(manDir)

## collect each module's references into one book-level bibliography
bibFiles <- list.files(file.path(prjDir, "modules"), "references_.*[.]bib$",
                       recursive = TRUE, full.names = TRUE)
bib <- Reduce(merge, lapply(bibFiles, RefManageR::ReadBib))
RefManageR::WriteBib(bib, file = "citations/references.bib")

chapters <- SpaDES.docs::prepManualRmds("../modules", rebuildCache = FALSE)

bookdown::render_book(output_format = "all", envir = new.env())

unlink("_manual_rmds", recursive = TRUE)   # the generated chapters are disposable
```

## Things that bite

**Add the staging directory to the book’s `.gitignore`.** The generated
chapters are build output. Before they were staged there they went into
each module’s own directory, and in these projects those are git
submodules. Every module repository therefore needed its own
`.gitignore` line, and a failed build left a file in each one.

**Chapter order comes from `_bookdown.yml`, not from the filesystem.** A
module that is generated but not listed is not de-duplicated against the
others, and
[`prepManualRmds()`](https://predictiveecology.github.io/SpaDES.docs/reference/prepManualRmds.md)
warns when that happens.

**Run it from the book root.** `_bookdown.yml` is read from the working
directory by default; pass `bookdownYML` if it is elsewhere.

**Cross-reference labels are global.** Two chapters using
`\@ref(tab:inputs)` collide. The `newModule()` template suffixes labels
with the module name for this reason; keep that if you edit them.
