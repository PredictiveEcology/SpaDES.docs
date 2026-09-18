# Changelog

## SpaDES.docs 0.1.0

- drop support for R 4.1 and 4.2;
- [`prepManualRmds()`](https://predictiveecology.github.io/SpaDES.docs/reference/prepManualRmds.md)
  gains argument `ignoreModules`. It matches whole module names; as a
  regex alternation it also dropped modules whose names merely contained
  one, and `character(0)` dropped everything;
- [`prepManualRmds()`](https://predictiveecology.github.io/SpaDES.docs/reference/prepManualRmds.md)
  classifies each line of a module `.Rmd` once – YAML, chunk header,
  chunk body, prose – and each rewrite now works from that instead of
  re-deriving document structure with its own regex. Fixes a family of
  failures: a prose mention of `root.dir` treated as a setting,
  `## References` inside a sentence treated as a heading, and a
  `(ref:key)` use in mid-paragraph treated as a duplicate definition and
  deleted;
- [`prepManualRmds()`](https://predictiveecology.github.io/SpaDES.docs/reference/prepManualRmds.md)
  no longer aborts on a module with no setup chunk; it synthesizes one.
  Five of the fireSense modules have none, and each would have stopped
  the whole book;
- [`prepManualRmds()`](https://predictiveecology.github.io/SpaDES.docs/reference/prepManualRmds.md)
  no longer crashes de-duplicating text references. It removed lines and
  then kept indexing the shortened vector with the original line
  numbers;
- [`prepManualRmds()`](https://predictiveecology.github.io/SpaDES.docs/reference/prepManualRmds.md)
  warns instead of continuing silently when `_bookdown.yml` lists
  chapters that were not prepared, or when there are no modules to
  prepare at all
  ([\#1](https://github.com/PredictiveEcology/SpaDES.docs/issues/1));
- `rebuildCache` reaches the generated chapter when a module mentions
  `cache.rebuild` only in a comment;
- [`prepManualRmds()`](https://predictiveecology.github.io/SpaDES.docs/reference/prepManualRmds.md)
  no longer deletes a module’s prose along with its YAML header. It
  removed every line from the first `---` to the last, so a `---`
  thematic rule anywhere in a module’s documentation took the header and
  all prose above the rule with it, silently. Only the header is removed
  now, and only when nothing but whitespace precedes it – the same way
  `SpaDES.core::moduleRmdToVignette()` reads the file;
- `modelr` is no longer a dependency; `modelr::seq_range()` was the call
  that caused the above;
- the package has a `testthat` suite;

## SpaDES.docs 0.0.1

- Initial version;
