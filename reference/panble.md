# Pander and kable wrapper function

This function overcomes/works around a recent issue with
`kable(..., longtable = TRUE)` and
`kable_styling(..., full_width = TRUE)` which fail when knitting to PDF
(see
https://stackoverflow.com/questions/71651334/longtable-t-messes-up-with-scaled-down-table-in-r-markdown-pdf/72511243#72511243).
It uses
[`pander::pander.table`](https://rdrr.io/pkg/pander/man/pander.table.html)
to automatically deal with long/wide tables when knitting to PDF and
[`knitr::kable`](https://rdrr.io/pkg/knitr/man/kable.html) when knitting
to HTML. Note that the chunk option `results` must be set to `"asis"`.

## Usage

``` r
panble(
  tab,
  caption = "",
  landscape = FALSE,
  panderArgs = list(),
  kableArgs = list(),
  kable_stylingArgs = list(),
  column_specArgs = list()
)
```

## Arguments

- tab:

  table object compatible with
  [`pander::pander`](https://rdrr.io/pkg/pander/man/pander.html) AND
  [`knitr::kable`](https://rdrr.io/pkg/knitr/man/kable.html)

- caption:

  a caption. Make sure special LaTeX characters are escaped. `bookdown`
  text references can be useful when formatting text and using special
  characters
  (https://bookdown.org/yihui/bookdown/markdown-extensions-by-bookdown.html#text-references).

- landscape:

  if TRUE panderOptions are changed so that the page can be put in
  landscape position. Note that this requires adding `\\newpage` and
  `\begin{landscape}` before the chunk and `\end{landscape}` after the
  chunk.

- panderArgs:

  named list of additional arguments passed to `pander`. Do NOT pass the
  caption and input table arguments.

- kableArgs:

  named list of additional arguments passed to `kable`. Do NOT pass the
  caption and input table arguments.

- kable_stylingArgs:

  named list of additional arguments passed to `kable_styling`. Do NOT
  pass the input table argument.

- column_specArgs:

  named list of additional arguments passed to `column_spec`. Do NOT
  pass the input table argument.

## Value

a markdown table.
