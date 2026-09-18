# Turn one module's `.Rmd` into a book chapter

Copies `<module>/<module>.Rmd` to `<stagingPath>/<module>2.Rmd` and
rewrites the copy. Nothing is written into the module's own directory.
The steps, in order, because several depend on the one before:

## Usage

``` r
prepOneModuleRmd(x, rebuildCache, stagingPath)
```

## Arguments

- x:

  path to the module's own `.Rmd`.

- rebuildCache:

  passed through from
  [`prepManualRmds()`](https://predictiveecology.github.io/SpaDES.docs/reference/prepManualRmds.md);
  the value written into the chunk's `cache.rebuild` option.

- stagingPath:

  directory the chapter is written into. The module's own directory is
  never written to; `root.dir` still points at it.

## Value

the path of the `<module>2.Rmd` written.

## Details

1.  strip the YAML header, so the chapter inherits the book's output
    format;

2.  add a chapter title if the module does not open with one;

3.  find the setup chunk, or synthesize one
    ([`ensureSetupChunk()`](https://predictiveecology.github.io/SpaDES.docs/reference/ensureSetupChunk.md));

4.  force that chunk to `eval = TRUE, cache = FALSE`, so it runs again
    in the book even if the module set otherwise for standalone
    knitting;

5.  point `root.dir` at the module directory, injecting it if absent;

6.  set `cache.rebuild` from `rebuildCache`, injecting it if absent;

7.  move the References heading to the end, adding one if absent, and
    append the LaTeX `\\printbibliography` command.

Steps 5 and 6 both inject after the chunk header, so `cache.rebuild`
ends up above `root.dir` in the generated chapter.
