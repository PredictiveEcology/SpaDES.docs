# SpaDES.docs (development version)

* drop support for R 4.1 and 4.2;
* `prepManualRmds()` gains argument `ignoreModules`;
* `prepManualRmds()` no longer deletes a module's prose along with its YAML header. It removed
  every line from the first `---` to the last, so a `---` thematic rule anywhere in a module's
  documentation took the header and all prose above the rule with it, silently. Only the header
  is removed now, and only when nothing but whitespace precedes it -- the same way
  `SpaDES.core::moduleRmdToVignette()` reads the file;
* `modelr` is no longer a dependency; `modelr::seq_range()` was the call that caused the above;
* the package has a `testthat` suite;

# SpaDES.docs 0.0.1

* Initial version;
