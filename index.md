# SpaDES.docs

![](https://github.com/PredictiveEcology/SpaDES/raw/main/man/figures/SpaDES.png)

Helper utilities for creating SpaDES module documentation and manuals.

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
