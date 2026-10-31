# Install the packages a manual's modules need

The modules a manual documents declare their own `reqdPkgs`, which the
manual must install before it can knit their chapters. Call this after
the manual's own dependencies are in place – typically right after
`remotes::install_deps()`, which is what installs this package.

## Usage

``` r
installModulePkgs(
  modulePath = "modules",
  modules = NULL,
  dependencies = TRUE,
  install = TRUE
)
```

## Arguments

- modulePath:

  directory holding the module directories.

- modules:

  names of the modules to resolve. Defaults to every directory under
  `modulePath`.

- dependencies:

  whether to expand each module package's own dependencies with
  [`Require::pkgDep2()`](https://Require.predictiveecology.org/reference/pkgDep.html)
  before installing. `FALSE` installs only the packages the modules
  name.

- install:

  whether to install. `FALSE` resolves the package list and returns it
  without installing anything, which is useful for seeing what a
  manual's modules ask for.

## Value

the packages, invisibly.

## Details

The package list is deliberately assigned before being installed rather
than piped.
[`Require::Install()`](https://Require.predictiveecology.org/reference/Require.html)
names its first parameter `packages` and calls
[`substitute()`](https://rdrr.io/r/base/substitute.html) on it, so a
piped expression resolves to the literal string `"packages"` and the
installer is asked for a package by that name.
[`Require::pkgDep2()`](https://Require.predictiveecology.org/reference/pkgDep.html)
also returns a named list, which `Install()` wants flattened.

## Unmet requirements

After installing, the versions on disk are checked against the versions
the modules asked for, and an unmet requirement is an error.
[`Require::Install()`](https://Require.predictiveecology.org/reference/Require.html)
reports one as a warning and returns, which leaves the manual knitting
against whatever is installed: the render then fails inside a module's
`defineModule()`, reporting an object the stale package does not export
yet rather than the stale package. On CI the usual cause is a restored
library cache older than the modules, which a failing build never
refreshes.
