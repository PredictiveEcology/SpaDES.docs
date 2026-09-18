# Text-reference definitions, as distinct from uses

A bookdown text reference is its own paragraph. The same pattern in the
middle of a paragraph is a *use*, and removing it as a cross-chapter
duplicate deletes the sentence around it.

## Usage

``` r
textRefDefs(lines)
```

## Arguments

- lines:

  character vector of lines.

## Value

integer vector of line indices holding definitions.
