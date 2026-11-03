# The label a chunk header carries

knitr reads the first chunk option as the label when it carries no `=`,
so `{r setup, include = FALSE}` is labelled and `{r, include = FALSE}`
is not.

Rebuilds the header and splices it in, rather than substituting inside
it. Two reasons, and the second is the dangerous one: a label may
contain spaces and hyphens that would have to be escaped to match
(several modules have labels like
`module usage example run -Biomass-core`); and
[`sub()`](https://rdrr.io/r/base/grep.html) processes its *replacement*
for backreferences, which collapses a `\\` in an option value to a
single `\`. That turned `fig.cap = "see Table \\\\@ref(tab:x)"` into
`"\\@"`, which is not a valid R escape, so knitr rejected the whole
header with "Invalid syntax for chunk options" and the book failed to
render.

## Usage

``` r
chunkLabel(header)

relabelChunk(header, label)
```

## Arguments

- header:

  one chunk header line.

- label:

  the label it should carry.

## Value

character vector the same length as `header`, empty where the chunk has
no label.

`header`, labelled `label`, with its other options unchanged character
for character.
