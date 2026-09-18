<img align="right" width="80" pad="20" src="https://github.com/PredictiveEcology/SpaDES/raw/main/man/figures/SpaDES.png">

# SpaDES.docs

Helper utilities for creating SpaDES module documentation and manuals.

<!-- badges: start -->
[![R-CMD-check](https://github.com/PredictiveEcology/SpaDES.docs/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PredictiveEcology/SpaDES.docs/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Installation

`SpaDES.docs` is not on CRAN. Install it with
[`Require`](https://github.com/PredictiveEcology/Require), which is what the
SpaDES workflows use and which resolves this package's own GitHub dependencies:

```r
# install.packages("Require")
Require::Require("PredictiveEcology/SpaDES.docs", require = FALSE)

## development version
Require::Require("PredictiveEcology/SpaDES.docs@development", require = FALSE)
```

`require = FALSE` installs without attaching; drop it to load the package in the
same call.
