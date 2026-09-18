# Changelog

## SpaDES.docs 0.3.0

- [`prepManualRmds()`](https://predictiveecology.github.io/SpaDES.docs/reference/prepManualRmds.md)
  warns when it prepares a chapter that `_bookdown.yml` does not list.
  The opposite case already warned; this direction is the quiet one,
  because the chapter is written, the build succeeds, and the module is
  simply absent from the book. It matters most for a manual that takes
  its module list from somewhere other than git submodules, where adding
  a module and forgetting the chapter entry is easy to do
  ([\#14](https://github.com/PredictiveEcology/SpaDES.docs/issues/14)).
- [`collapseModuleBibs()`](https://predictiveecology.github.io/SpaDES.docs/reference/collapseModuleBibs.md)
  merges the modules’ `references_*.bib` files, and any the manual
  supplies, into the single bibliography bookdown wants. Files with no
  entries are skipped: a module that cites nothing yet ships a
  comments-only `.bib`, which
  [`RefManageR::ReadBib()`](https://docs.ropensci.org/RefManageR/reference/ReadBib.html)
  fails on, taking a whole manual down over one placeholder
  ([\#15](https://github.com/PredictiveEcology/SpaDES.docs/issues/15)).
- [`downloadCSL()`](https://predictiveecology.github.io/SpaDES.docs/reference/downloadCSL.md)
  fetches a Citation Style Language file from the Zotero repository,
  keeping an existing copy so a build does not need the network
  ([\#15](https://github.com/PredictiveEcology/SpaDES.docs/issues/15)).
- [`installModulePkgs()`](https://predictiveecology.github.io/SpaDES.docs/reference/installModulePkgs.md)
  installs the packages a manual’s modules declare, and with
  `install = FALSE` resolves the list without installing. It assigns the
  package list before installing rather than piping it, because
  [`Require::Install()`](https://Require.predictiveecology.org/reference/Require.html)
  calls [`substitute()`](https://rdrr.io/r/base/substitute.html) on its
  first parameter and a piped expression resolves to the literal string
  `"packages"`
  ([\#15](https://github.com/PredictiveEcology/SpaDES.docs/issues/15)).
- [`manualPaths()`](https://predictiveecology.github.io/SpaDES.docs/reference/manualPaths.md)
  resolves a manual’s root, rendered book, `citations` and `figures`
  directories, reading `output_dir` from `_bookdown.yml`
  ([\#15](https://github.com/PredictiveEcology/SpaDES.docs/issues/15)).
- [`stagePagesFiles()`](https://predictiveecology.github.io/SpaDES.docs/reference/stagePagesFiles.md)
  writes `.nojekyll`, and optionally `CNAME`, into the rendered book
  directory. A deploy publishes the contents of that directory, so these
  files never reach the site if written to the repository root
  ([\#15](https://github.com/PredictiveEcology/SpaDES.docs/issues/15)).
- [`writePkgBib()`](https://predictiveecology.github.io/SpaDES.docs/reference/writePkgBib.md)
  writes a bibliography for the R packages in use
  ([\#15](https://github.com/PredictiveEcology/SpaDES.docs/issues/15)).
- new vignette, *Building a continuously updated manual*, for a manual
  that tracks its modules’ branches and rebuilds itself – the CI
  arrangement fireSenseManual and LandR-Manual use, as distinct from a
  project manual that pins its modules. *Building a project manual* now
  says which of the two it covers, and both worked examples use the
  functions above
  ([\#15](https://github.com/PredictiveEcology/SpaDES.docs/issues/15)).

## SpaDES.docs 0.2.0

- new vignette, *Building a project manual*: the layout a manual uses, a
  runnable minimal example, what
  [`prepManualRmds()`](https://predictiveecology.github.io/SpaDES.docs/reference/prepManualRmds.md)
  does to each module `.Rmd` and why, the build-script pattern, and the
  things that bite. Resolves the `VignetteBuilder` field that had been
  declared against no vignette
  ([\#2](https://github.com/PredictiveEcology/SpaDES.docs/issues/2));
- the README says what the package is for, points at the vignette, and
  lists the manuals built with it. pkgdown builds the site home page
  from it, so it is also the front page of
  <https://predictiveecology.github.io/SpaDES.docs/>
  ([\#3](https://github.com/PredictiveEcology/SpaDES.docs/issues/3));
- **Breaking:**
  [`prepManualRmds()`](https://predictiveecology.github.io/SpaDES.docs/reference/prepManualRmds.md)
  writes the generated chapters to a staging directory under the book
  root (`stagingPath`, default `_manual_rmds`) instead of into each
  module’s own directory. Books must list the chapters from there in
  `_bookdown.yml`, and should add the directory to `.gitignore`. The
  module directories are git submodules in every project that uses this
  package: a failed build used to leave a `<module>2.Rmd` in each one,
  and each module repository carried a `.gitignore` line to hide it.
  Verified equivalent by rendering the same chapters from both locations
  – the output is byte-identical, including relative images,
  cross-references and citations;
- chapters left by a previous run are cleared, so a module removed from
  a project no longer lingers as an orphan chapter;
- [`prepManualRmds()`](https://predictiveecology.github.io/SpaDES.docs/reference/prepManualRmds.md)
  no longer fails when `_bookdown.yml` lists none of the modules under
  `modulePath` – the case reported in
  [\#1](https://github.com/PredictiveEcology/SpaDES.docs/issues/1),
  where the modules exist but every module line is commented out. It
  warns, writes the chapters, and skips the cross-chapter de-duplication
  it cannot do
  ([\#1](https://github.com/PredictiveEcology/SpaDES.docs/issues/1));
- [`prepManualRmds()`](https://predictiveecology.github.io/SpaDES.docs/reference/prepManualRmds.md)
  parses `_bookdown.yml` as YAML rather than by indentation. The old
  `sub(" - ", ...)` assumed exactly two spaces, could not read the
  flow-style `rmd_files: [a, b]` form, and counted a commented-out line
  as a listed chapter;
- [`prepManualRmds()`](https://predictiveecology.github.io/SpaDES.docs/reference/prepManualRmds.md)
  gains argument `bookdownYML`, and checks the file exists before
  writing anything. A missing book file used to surface only after every
  `<module>2.Rmd` had been created, leaving them behind;

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
  chapters that were not prepared, and when there are no modules to
  prepare at all. Note this does not yet cover
  [\#1](https://github.com/PredictiveEcology/SpaDES.docs/issues/1),
  where the modules exist but every module line in `_bookdown.yml` is
  commented out;
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
