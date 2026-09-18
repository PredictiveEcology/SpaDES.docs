# Labels and captions using `bookdown` cross-referencing format

Makes captions for tables and figures using the `bookdown`
cross-referencing format. Meant to be used inside an Rmarkdown chunk.

## Usage

``` r
addLabel(caption = "", tag = "tab")
```

## Arguments

- caption:

  a string with the desired caption.

- tag:

  the cross-reference tag used by `bookdown`. Recognised tagas are
  `"tab"`, `"fig"` and `"eq"`. See https://bookdown.org/yihui/bookdown

## Value

the full caption with its label in a HTML or LaTeX compatible format
