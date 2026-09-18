# SpaDES.docs

![](https://github.com/PredictiveEcology/SpaDES/raw/main/man/figures/SpaDES.png)

Helper utilities for creating SpaDES module documentation and manuals.

A SpaDES module documents itself in `<module>/<module>.Rmd`, written to
be knitted on its own. A *project manual* is a
[bookdown](https://bookdown.org) book that gathers several of those
files into one document, with continuous chapter numbering, one
bibliography, and a PDF.

They are not the same document. A chapter has no YAML header of its own,
its cross-reference labels share a namespace with every other chapter,
and its working directory is the book’s rather than the module’s.
[`prepManualRmds()`](https://predictiveecology.github.io/SpaDES.docs/reference/prepManualRmds.md)
reconciles the two, and the rest of the package is the table and caption
helpers those manuals use.

## Usage

``` r

library(SpaDES.docs)

## from the book root, where _bookdown.yml lives
chapters <- prepManualRmds(modulePath = "../modules")
```

See [**Building a project
manual**](https://predictiveecology.github.io/SpaDES.docs/articles/building-a-project-manual.html)
for the whole workflow: the directory layout, a runnable example, what
[`prepManualRmds()`](https://predictiveecology.github.io/SpaDES.docs/reference/prepManualRmds.md)
does to each module `.Rmd` and why, the build-script pattern, and the
things that bite.

## Manuals built with it

|  |  |
|----|----|
| [LandWeb](https://github.com/PredictiveEcology/LandWeb) | the reference implementation. `manual/` inside an active project, rendered to `docs/`. Start here |
| [fireSenseManual](https://github.com/PredictiveEcology/fireSenseManual) | the variant where the manual is its own repository rather than a directory inside a project |
| [LandR-Manual](https://github.com/PredictiveEcology/LandR-Manual) | the oldest of the three, and not currently maintained; useful as a reference, not as a template |

## Installation

`SpaDES.docs` is not on CRAN. Install it with
[`Require`](https://github.com/PredictiveEcology/Require), which is what
the SpaDES workflows use and which resolves this package’s own GitHub
dependencies:

``` r

# install.packages("Require")
Require::Require("PredictiveEcology/SpaDES.docs", require = FALSE)

## development version
Require::Require("PredictiveEcology/SpaDES.docs@development", require = FALSE)
```

`require = FALSE` installs without attaching; drop it to load the
package in the same call.
