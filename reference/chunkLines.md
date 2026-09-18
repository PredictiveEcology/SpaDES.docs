# Line indices belonging to the chunk opened at `at`

Line indices belonging to the chunk opened at `at`

## Usage

``` r
chunkLines(lines, at, cls = classifyRmdLines(lines))
```

## Arguments

- lines:

  character vector of lines.

- at:

  index of the chunk header.

- cls:

  line classes, as from
  [`classifyRmdLines()`](https://predictiveecology.github.io/SpaDES.docs/reference/classifyRmdLines.md).

## Value

integer vector of indices, from the header to its closing fence, or to
the end of the file if the chunk is never closed.
