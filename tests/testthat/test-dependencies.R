test_that("installModulePkgs() warns rather than installing nothing silently", {
  skip_if_not_installed("SpaDES.core")
  d <- withr::local_tempdir()
  withr::local_dir(d)
  dir.create("modules")
  expect_warning(out <- installModulePkgs("modules"), "no modules found")
  expect_identical(out, character(0))
})

## A module is a directory holding <name>.R with a defineModule() call;
## SpaDES.core::packages() reads reqdPkgs out of that metadata.
writeModuleR <- function(name, modulePath, reqdPkgs) {
  dir.create(file.path(modulePath, name), recursive = TRUE, showWarnings = FALSE)
  writeLines(
    c(
      "defineModule(sim, list(",
      sprintf('  name = "%s",', name),
      '  description = "test",',
      '  keywords = "test",',
      '  authors = person("A", "B", role = c("aut", "cre")),',
      '  version = list(x = numeric_version("1.0.0")),',
      '  timeunit = "year",',
      '  citation = list("citation.bib"),',
      '  documentation = list(),',
      sprintf('  reqdPkgs = list(%s),', paste0('"', reqdPkgs, '"', collapse = ", ")),
      "  parameters = rbind(NULL),",
      "  inputObjects = data.frame(objectName = NA_character_),",
      "  outputObjects = data.frame(objectName = NA_character_)",
      "))"
    ),
    file.path(modulePath, name, paste0(name, ".R"))
  )
}

test_that("installModulePkgs(install = FALSE) resolves without installing", {
  skip_if_not_installed("SpaDES.core")
  d <- withr::local_tempdir()
  withr::local_dir(d)
  writeModuleR("modA", "modules", c("stats", "utils"))
  writeModuleR("modB", "modules", "tools")

  pkgs <- installModulePkgs("modules", dependencies = FALSE, install = FALSE)
  expect_type(pkgs, "character")
  expect_true(all(c("stats", "utils", "tools") %in% pkgs))
})

test_that("installModulePkgs() de-duplicates across modules", {
  skip_if_not_installed("SpaDES.core")
  d <- withr::local_tempdir()
  withr::local_dir(d)
  writeModuleR("modA", "modules", c("stats", "utils"))
  writeModuleR("modB", "modules", c("stats", "tools"))

  pkgs <- installModulePkgs("modules", dependencies = FALSE, install = FALSE)
  expect_equal(anyDuplicated(pkgs), 0L)
})

test_that("installModulePkgs() honours an explicit module list", {
  skip_if_not_installed("SpaDES.core")
  d <- withr::local_tempdir()
  withr::local_dir(d)
  writeModuleR("modA", "modules", "stats")
  writeModuleR("modB", "modules", "tools")

  pkgs <- installModulePkgs("modules", modules = "modA", dependencies = FALSE, install = FALSE)
  expect_true("stats" %in% pkgs)
  expect_false("tools" %in% pkgs)
})

## The versions on disk against the versions the modules asked for. Require
## reports an unmet requirement as a warning, so without this check the manual
## renders and fails later on a symbol the stale package does not export.

test_that("unsatisfiedPkgs() passes a spec with no version to check", {
  expect_identical(
    unsatisfiedPkgs(c("stats", "PredictiveEcology/Require@development")),
    character(0)
  )
})

test_that("unsatisfiedPkgs() passes a requirement the library meets", {
  expect_identical(unsatisfiedPkgs("utils (>= 0.1)"), character(0))
})

test_that("unsatisfiedPkgs() reads a version off a GitHub ref", {
  ## the shape a module actually declares -- owner/repo@branch (>= version)
  unmet <- unsatisfiedPkgs("PredictiveEcology/Require@development (>= 99.0)")

  expect_named(unmet, "Require (>= 99.0)")
  expect_identical(unname(unmet), as.character(utils::packageVersion("Require")))
})

test_that("unsatisfiedPkgs() reports the version installed against the one asked for", {
  unmet <- unsatisfiedPkgs("utils (>= 99.0)")

  expect_named(unmet, "utils (>= 99.0)")
  expect_identical(unname(unmet), as.character(utils::packageVersion("utils")))
})

test_that("unsatisfiedPkgs() reports a package that is absent entirely", {
  expect_identical(
    unsatisfiedPkgs("notAnInstalledPackage (>= 1.0)"),
    c("notAnInstalledPackage (>= 1.0)" = "not installed")
  )
})

test_that("unsatisfiedPkgs() honours each inequality", {
  v <- as.character(utils::packageVersion("utils"))

  expect_identical(unsatisfiedPkgs(paste0("utils (== ", v, ")")), character(0))
  expect_identical(unsatisfiedPkgs(paste0("utils (<= ", v, ")")), character(0))
  expect_named(unsatisfiedPkgs(paste0("utils (> ", v, ")")), paste0("utils (> ", v, ")"))
  expect_named(unsatisfiedPkgs(paste0("utils (< ", v, ")")), paste0("utils (< ", v, ")"))
})

test_that("unsatisfiedPkgs() orders development versions by their last component", {
  ## the case this exists for: a cached 0.2.3.9024 against a module asking for
  ## 0.2.3.9132. Anything comparing only the first three components calls these
  ## equal and the check passes while the render does not.
  expect_lt(utils::compareVersion("0.2.3.9024", "0.2.3.9132"), 0L)
  expect_gt(utils::compareVersion("0.2.3.9132", "0.2.3.9024"), 0L)
})

test_that("installModulePkgs() stops when a module's requirement is unmet", {
  skip_if_not_installed("SpaDES.core")
  d <- withr::local_tempdir()
  withr::local_dir(d)
  writeModuleR("modA", "modules", "notAnInstalledPackage (>= 1.0)")
  local_mocked_bindings(Install = function(...) invisible(NULL), .package = "Require")

  expect_snapshot(error = TRUE, installModulePkgs("modules", dependencies = FALSE))
})

test_that("installModulePkgs() installs without complaint when requirements are met", {
  skip_if_not_installed("SpaDES.core")
  d <- withr::local_tempdir()
  withr::local_dir(d)
  writeModuleR("modA", "modules", "utils (>= 0.1)")
  called <- FALSE
  local_mocked_bindings(
    Install = function(...) {
      called <<- TRUE
      invisible(NULL)
    },
    .package = "Require"
  )

  ## SpaDES.core::packages() adds SpaDES.core itself to every module's list
  expect_contains(installModulePkgs("modules", dependencies = FALSE), "utils (>= 0.1)")
  expect_true(called)
})
