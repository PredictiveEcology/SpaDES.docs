# Locate a module's setup chunk, creating one if it has none

A module needs somewhere to carry `root.dir` and `cache.rebuild`; it
does not need to have supplied that place itself. The fireSense modules
have no setup chunk at all, and refusing them would stop the whole book
from building.

## Usage

``` r
ensureSetupChunk(lines, modName)
```

## Arguments

- lines:

  character vector of lines.

- modName:

  module name, used for the synthesized chunk's label.

## Value

a list with `lines` (with a chunk inserted if there was none) and `at`,
the index of the setup chunk header.
