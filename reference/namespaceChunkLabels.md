# Make a chapter's chunk labels unique across the whole book

bookdown merges every chapter into one file before knitting, so chunk
labels have to be unique across the book and not merely within a module.
The modules are written and released separately, and several still carry
the labels the SpaDES.core template supplies, so two of them in one book
collide and the render stops at knitr's `Duplicate chunk label`. That is
what `fireSense_burn` and `fireSense_ELFs` did, both holding
`module usage example`.

## Usage

``` r
namespaceChunkLabels(lines, modName)
```

## Arguments

- lines:

  character vector of lines.

- modName:

  module name, used as the namespace.

## Value

`lines`, with chunk labels and the references to them rewritten.

## Details

Each label gains the module's name, which cannot collide because module
names are directory names. A label already ending in it is left exactly
as it is, down to its spacing: most module labels are namespaced by hand
already, and their knitr cache entries and `\\@ref()` targets are built
on the names they chose. Labels duplicated *within* one module are then
numbered apart, since one module's suffix cannot separate them from each
other.

`\\@ref(fig:label)` and `\\@ref(tab:label)` move with the labels.
bookdown builds a figure's anchor from its chunk label, and
[`addLabel()`](https://predictiveecology.github.io/SpaDES.docs/reference/addLabel.md)
writes a table's anchor from `opts_current$get("label")`, so a renamed
label whose references stayed behind renders as `??` rather than failing
– `Biomass_core` alone has 19 such references.

An unlabelled chunk is left to knitr, which numbers those across the
merged book and so cannot produce a collision.
