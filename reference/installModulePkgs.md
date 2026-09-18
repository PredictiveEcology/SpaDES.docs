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
