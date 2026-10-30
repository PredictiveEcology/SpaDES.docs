# Text-reference definitions, as distinct from uses

A bookdown text reference is its own paragraph. The same pattern in the
middle of a paragraph is a *use*, and removing it as a cross-chapter
duplicate deletes the sentence around it.

bookdown treats any line beginning `(ref:key) ` as a definition.
`textRefDefs()` requires the line to be its own paragraph, because
removing a mid-paragraph match as a cross-chapter duplicate would delete
the sentence it sits in.

## Usage

``` r
textRefDefs(lines)

textRefMisreads(lines)
```

## Arguments

- lines:

  character vector of lines.

## Value

integer vector of line indices holding definitions.

integer vector of line indices.

## Details

Prose that wraps so a *use* lands first on a line falls in that gap: a
definition to bookdown, a use here, and so never de-duplicated. The
symptom is `Possibly duplicated text reference labels` from a build that
de-duplicated everything it could see. The fix is to re-wrap the
module's prose, so this is reported rather than acted on.
