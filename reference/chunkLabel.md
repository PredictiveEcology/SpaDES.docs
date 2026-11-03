# The label a chunk header carries

knitr reads the first chunk option as the label when it carries no `=`,
so `{r setup, include = FALSE}` is labelled and `{r, include = FALSE}`
is not.

Rebuilds the header rather than substituting inside it: a label may
contain spaces and hyphens, which would have to be escaped to match, and
several modules have labels like
`module usage example run -Biomass-core`.

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

`header`, labelled `label`, with its other options unchanged.
