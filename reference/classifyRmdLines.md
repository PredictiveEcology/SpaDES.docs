# Label every line of an `.Rmd` by its role in the document

The rewrites in
[`prepManualRmds()`](https://predictiveecology.github.io/SpaDES.docs/reference/prepManualRmds.md)
each need to know whether a line sits inside a fenced chunk. Deriving
that separately per concern is what produced a family of bugs: a prose
mention of `root.dir` treated as a setting, a `## References` inside a
sentence treated as a heading, and a `(ref:key)` use in mid-paragraph
treated as a definition and deleted.

## Usage

``` r
classifyRmdLines(lines)
```

## Arguments

- lines:

  character vector of lines, as from
  [`readLines()`](https://rdrr.io/r/base/readLines.html).

## Value

character vector the same length as `lines`, each element one of
`"chunkHeader"`, `"chunkBody"`, `"chunkEnd"` or `"prose"`.

## Details

Uses `knitr::all_patterns$md`, so a four-backtick or indented fence is
counted the way knitr counts it.
