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
#' @section Unmet requirements:
#' After installing, the versions on disk are checked against the versions the
#' modules asked for, and an unmet requirement is an error. [Require::Install()]
#' reports one as a warning and returns, which leaves the manual knitting
#' against whatever is installed: the render then fails inside a module's
#' `defineModule()`, reporting an object the stale package does not export yet
#' rather than the stale package. On CI the usual cause is a restored library
#' cache older than the modules, which a failing build never refreshes.
#'
#' @return the packages, invisibly.
#'
#' @export
installModulePkgs <- function(
  modulePath = "modules",
  modules = NULL,
  dependencies = TRUE,
  install = TRUE
) {
  for (pkg in c("SpaDES.core", "Require")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("installModulePkgs() needs the '", pkg, "' package to be installed.", call. = FALSE)
    }
  }
  if (is.null(modules)) {
    modules <- list.files(modulePath)
  }
  if (!length(modules)) {
    warning("installModulePkgs(): no modules found in '", modulePath, "'.", call. = FALSE)
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

    ## checked against what the modules declared, not the expansion above:
    ## a transitive constraint that Require reads more strictly than the
    ## dependency itself does is not the manual's problem to stop for
    unmet <- unsatisfiedPkgs(modPkgs)
    if (length(unmet)) {
      stop(
        "installModulePkgs(): the modules require package versions that are not installed:\n",
        paste0("  ", names(unmet), ": ", unmet, "\n", collapse = ""),
        "Require::Install() reports this as a warning and returns, so the manual ",
        "would otherwise knit against what is on disk and fail inside a module's ",
        "metadata, where the message names a missing object rather than a stale ",
        "package. On CI the usual cause is a restored library cache older than ",
        "the modules.",
        call. = FALSE
      )
    }
  }
  invisible(pkgs)
}

## Which of `specs` name a version the installed library does not satisfy,
## as installed-version-by-requirement. Specs with no version are skipped:
## there is nothing to check against.
unsatisfiedPkgs <- function(specs) {
  wanted <- Require::extractVersionNumber(specs)
  keep <- !is.na(wanted)
  if (!any(keep)) {
    return(character(0))
  }
  specs <- specs[keep]
  wanted <- wanted[keep]
  name <- Require::extractPkgName(specs)
  ineq <- Require::extractInequality(specs)

  have <- vapply(
    name,
    function(p) {
      tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_)
    },
    character(1),
    USE.NAMES = FALSE
  )

  satisfied <- vapply(
    seq_along(specs),
    function(i) {
      if (is.na(have[i])) {
        return(FALSE)
      }
      cmp <- utils::compareVersion(have[i], wanted[i])
      switch(
        ineq[i],
        ">=" = cmp >= 0L,
        ">" = cmp > 0L,
        "==" = cmp == 0L,
        "<=" = cmp <= 0L,
        "<" = cmp < 0L,
        ## an inequality this does not know is not evidence of a problem
        TRUE
      )
    },
    logical(1)
  )

  unmet <- !satisfied
  if (!any(unmet)) {
    return(character(0))
  }
  out <- ifelse(is.na(have[unmet]), "not installed", have[unmet])
  names(out) <- paste0(name[unmet], " (", ineq[unmet], " ", wanted[unmet], ")")
  out
}
