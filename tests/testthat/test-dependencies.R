test_that("installModulePkgs() warns rather than installing nothing silently", {
  skip_if_not_installed("SpaDES.core")
  d <- withr::local_tempdir(); withr::local_dir(d)
  dir.create("modules")
  expect_warning(out <- installModulePkgs("modules"), "no modules found")
  expect_identical(out, character(0))
})

## A module is a directory holding <name>.R with a defineModule() call;
## SpaDES.core::packages() reads reqdPkgs out of that metadata.
writeModuleR <- function(name, modulePath, reqdPkgs) {
  dir.create(file.path(modulePath, name), recursive = TRUE, showWarnings = FALSE)
  writeLines(c(
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
  ), file.path(modulePath, name, paste0(name, ".R")))
}

test_that("installModulePkgs(install = FALSE) resolves without installing", {
  skip_if_not_installed("SpaDES.core")
  d <- withr::local_tempdir(); withr::local_dir(d)
  writeModuleR("modA", "modules", c("stats", "utils"))
  writeModuleR("modB", "modules", "tools")

  pkgs <- installModulePkgs("modules", dependencies = FALSE, install = FALSE)
  expect_type(pkgs, "character")
  expect_true(all(c("stats", "utils", "tools") %in% pkgs))
})

test_that("installModulePkgs() de-duplicates across modules", {
  skip_if_not_installed("SpaDES.core")
  d <- withr::local_tempdir(); withr::local_dir(d)
  writeModuleR("modA", "modules", c("stats", "utils"))
  writeModuleR("modB", "modules", c("stats", "tools"))

  pkgs <- installModulePkgs("modules", dependencies = FALSE, install = FALSE)
  expect_equal(anyDuplicated(pkgs), 0L)
})

test_that("installModulePkgs() honours an explicit module list", {
  skip_if_not_installed("SpaDES.core")
  d <- withr::local_tempdir(); withr::local_dir(d)
  writeModuleR("modA", "modules", "stats")
  writeModuleR("modB", "modules", "tools")

  pkgs <- installModulePkgs("modules", modules = "modA",
                            dependencies = FALSE, install = FALSE)
  expect_true("stats" %in% pkgs)
  expect_false("tools" %in% pkgs)
})
