#' Install the packages a manual's modules need
#'
#' The modules a manual documents declare their own `reqdPkgs`, which the manual
#' must install before it can knit their chapters. Call this after the manual's
#' own dependencies are in place -- typically right after
#' `remotes::install_deps()`, which is what installs this package.
#'
#' @details
#' The package list is deliberately assigned before being installed rather than
#' piped. [Require::Install()] names its first parameter `packages` and calls
#' [substitute()] on it, so a piped expression resolves to the literal string
#' `"packages"` and the installer is asked for a package by that name.
#' [Require::pkgDep2()] also returns a named list, which `Install()` wants
#' flattened.
#'
#' @param modulePath directory holding the module directories.
#'
#' @param modules names of the modules to resolve. Defaults to every directory
#'  under `modulePath`.
#'
#' @param dependencies whether to expand each module package's own dependencies
#'  with [Require::pkgDep2()] before installing. `FALSE` installs only the
#'  packages the modules name.
#'
#' @param install whether to install. `FALSE` resolves the package list and
#'  returns it without installing anything, which is useful for seeing what a
#'  manual's modules ask for.
#'
#' @return the packages, invisibly.
#'
#' @export
installModulePkgs <- function(modulePath = "modules", modules = NULL,
                              dependencies = TRUE, install = TRUE) {
  for (pkg in c("SpaDES.core", "Require")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("installModulePkgs() needs the '", pkg, "' package to be installed.",
           call. = FALSE)
    }
  }
  if (is.null(modules)) {
    modules <- list.files(modulePath)
  }
  if (!length(modules)) {
    warning("installModulePkgs(): no modules found in '", modulePath, "'.",
            call. = FALSE)
    return(invisible(character(0)))
  }

  ## assigned, never piped -- see @details
  modPkgs <- SpaDES.core::packages(modules = modules, paths = modulePath)
  modPkgs <- unique(unlist(modPkgs, use.names = FALSE))

  pkgs <- if (isTRUE(dependencies)) {
    unique(unlist(Require::pkgDep2(modPkgs), use.names = FALSE))
  } else {
    modPkgs
  }

  if (isTRUE(install)) {
    Require::Install(pkgs)
  }
  invisible(pkgs)
}
