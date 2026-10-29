<img align="right" width="80" pad="20" src="https://github.com/PredictiveEcology/SpaDES/raw/main/man/figures/SpaDES.png">

# SpaDES.docs

Helper utilities for creating SpaDES module documentation and manuals.

<!-- badges: start --> [![R-CMD-check](https://github.com/PredictiveEcology/SpaDES.docs/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PredictiveEcology/SpaDES.docs/actions/workflows/R-CMD-check.yaml) [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) <!-- badges: end -->

A SpaDES module documents itself in `<module>/<module>.Rmd`, written to be knitted on its own.
A *project manual* is a [bookdown](https://bookdown.org) book that gathers several of those files into one document, with continuous chapter numbering, one bibliography, and a PDF.

They are not the same document.
A chapter has no YAML header of its own, its cross-reference labels share a namespace with every other chapter, and its working directory is the book's rather than the module's.
`prepManualRmds()` reconciles the two, and the rest of the package is the table and caption helpers those manuals use.

## Usage

```r
library(SpaDES.docs)

## from the book root, where _bookdown.yml lives
chapters <- prepManualRmds(modulePath = "../modules")
```

See [**Building a project manual**](https://predictiveecology.github.io/SpaDES.docs/articles/building-a-project-manual.html) for the whole workflow: the directory layout, a runnable example, what `prepManualRmds()` does to each module `.Rmd` and why, the build-script pattern, and the constraints to work within.

## Manuals built with it

Which one to copy follows from what the manual is for.
A **project manual** pins its modules, so it and the results it describes agree; a **continuously updated** manual tracks them, so a documentation fix reaches the site without anyone republishing.
Almost everything else follows from that choice.

| | | formats |
|---|---|---|
| [LandWeb](https://github.com/PredictiveEcology/LandWeb) | a project manual: `manual/` inside an active project, modules pinned as git submodules. See `vignette("building-a-project-manual")` | HTML, PDF, EPUB |
| [LandR-Manual](https://github.com/PredictiveEcology/LandR-Manual) | continuously updated, and the fullest example: its own repository, modules checked out by the workflow at one `MODULE_REF`, a PDF archived per release and published with the site, custom domain | HTML, PDF, EPUB |
| [fireSenseManual](https://github.com/PredictiveEcology/fireSenseManual) | the same arrangement as LandR-Manual, without the release machinery. See `vignette("building-a-continuously-updated-manual")` for both | HTML |

## Installation

`SpaDES.docs` is not on CRAN.
Install it with [`Require`](https://github.com/PredictiveEcology/Require), which is what the SpaDES workflows use and which resolves this package's own GitHub dependencies:

```r
# install.packages("Require")
Require::Require("PredictiveEcology/SpaDES.docs", require = FALSE)

## development version
Require::Require("PredictiveEcology/SpaDES.docs@development", require = FALSE)
```

`require = FALSE` installs without attaching; drop it to load the package in the same call.
