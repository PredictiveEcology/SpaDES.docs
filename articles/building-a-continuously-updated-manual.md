# Building a continuously updated manual

There are two kinds of manual in this ecosystem, and they want opposite
things from their modules.

A **project manual** documents the modules *as a particular project used
them*. It should pin them, so the manual and the results it describes
agree. That is the subject of
[`vignette("building-a-project-manual")`](https://predictiveecology.github.io/SpaDES.docs/articles/building-a-project-manual.md),
and [LandWeb](https://github.com/PredictiveEcology/LandWeb) is the
example.

A **continuously updated manual** documents the modules *as they are
now*. It should track them, so a documentation fix reaches the published
site without anyone republishing the manual.
[fireSenseManual](https://github.com/PredictiveEcology/fireSenseManual)
and [LandR-Manual](https://github.com/PredictiveEcology/LandR-Manual)
are those, and this vignette is about them.

The distinction decides almost everything else. A git submodule records
a *commit*: a state. A manual that tracks development wants to record an
*intent* – “whatever `development` says today”. Every gap between the
two has to be closed by a person bumping a pointer, which is the chore
this arrangement removes.

## The shape

The manual is its own repository, and the module sources are build
output:

    <manual>/
      index.Rmd
      _bookdown.yml
      _output.yml
      build.R
      install.R
      DESCRIPTION            # so install.R can resolve the manual's own deps
      modules/               # fetched by CI; gitignored
      _manual_rmds/          # generated chapters; gitignored
      docs/                  # rendered output, deployed to a gh-pages branch
      .github/workflows/

## Fetching the modules

Check each module out by branch, with the standard action. The workflow
is then the manifest: every entry names a repository and a ref, and
there is no custom code to maintain.

``` yaml
env:
  ## one edit moves every module; workflow_dispatch can override it for a trial
  MODULE_REF: development

jobs:
  build-manual-site:
    steps:
      - uses: actions/checkout@v7

      - uses: actions/checkout@v7
        with:
          repository: PredictiveEcology/Biomass_core
          ref: ${{ github.event.inputs.module_ref || env.MODULE_REF }}
          path: modules/Biomass_core
```

Use `github.event.inputs`, not the `inputs` context, so the expression
is `null` rather than an error on `push` and `schedule` events.

A module whose branches differ from the rest overrides the default in
its own step. This is not hypothetical: `Biomass_validationKNN`’s `main`
is ahead of its `development`, the opposite of the other LandR modules.

Because nothing in the repository records which commits a build used,
have the workflow write them out:

``` yaml
      - name: Record module versions
        run: |
          for d in modules/*/; do
            echo "$(basename "$d") $(git -C "$d" rev-parse --short HEAD)"
          done >> "$GITHUB_STEP_SUMMARY"
```

## Installing what the chapters need

The manual’s own dependencies come from its `DESCRIPTION`. The modules
declare their own, which cannot be resolved until the modules are on
disk:

``` r

remotes::install_deps()
SpaDES.docs::installModulePkgs("modules")
```

## The build script

``` r

paths <- SpaDES.docs::manualPaths()

SpaDES.docs::writePkgBib(file.path(paths$citations, "packages.bib"))
SpaDES.docs::downloadCSL("ecology-letters", paths$citations)
SpaDES.docs::collapseModuleBibs(
  modulePath = file.path(paths$prj, "modules"),
  extraBibs = file.path(paths$citations, c("packages.bib", "references.bib")),
  outFile = file.path(paths$citations, "references.bib")
)

chapters <- SpaDES.docs::prepManualRmds("./modules", rebuildCache = FALSE)

bookdown::render_book(output_format = "all", envir = new.env())

## these must be inside the published directory, not at the repository root
SpaDES.docs::stagePagesFiles(paths$docs, cname = "example.predictiveecology.org")

unlink("_manual_rmds", recursive = TRUE)
```

## Deploying

The deploy publishes the *contents* of the rendered directory to a
branch, so anything written at the repository root never reaches the
site. `.nojekyll` and `CNAME` both have to be inside it, which is what
[`stagePagesFiles()`](https://predictiveecology.github.io/SpaDES.docs/reference/stagePagesFiles.md)
is for.

Writing them at the root appears to work for as long as an earlier
deploy left copies behind and the action deploys with `clean: false`. A
branch published for the first time has neither, and the failure is
quiet: a green build, and a site that either loses its custom domain or
drops the `_`-prefixed directories bookdown emits.

## Caching

Cache the directories bookdown actually writes. With `output_dir` set it
writes knitr’s cache to `<book_filename>_cache/` and the figures to
`<book_filename>_files/`, both at the repository root. There is no
`_bookdown_files` directory, and caching a path of that name fails
silently – the action warns that the path does not exist and saves
nothing, on every build.

``` yaml
      - uses: actions/cache@v6
        with:
          path: |
            *_cache
            *_files
          key: bookdown-${{ hashFiles('**/*Rmd') }}
          restore-keys: bookdown-
```

Cache the R library as well. Installing the SpaDES stack from source
dominates the job, and a daily schedule would otherwise pay it every
day.

## Rebuild cadence

`schedule`, plus `workflow_dispatch`, plus push to the manual’s own
default branch. A daily rebuild keeps the site within a day of the
modules without anyone touching this repository.

Per-push rebuilding, where each module repository notifies the manual,
needs a token with write access distributed to every module repository,
and it rebuilds a whole book because one typo was fixed. The cost is not
worth the freshness.

## Releases, and keeping the old PDFs

A manual that tracks `development` is never “a version” of anything. A
release is the other build: pinned refs, a version number, and a PDF
that stays available after the site has moved on.

Keep the released PDFs in version control, under `archive/pdf/`, and
publish them with the site rather than leaving them on the published
branch. That branch is rebuilt by every deploy, and an old PDF cannot be
regenerated from current sources – so it is the one artifact that must
not live only there.

``` r

paths <- SpaDES.docs::manualPaths()

## after render_book(), and only when a PDF was produced
SpaDES.docs::archiveManualPDF(
  file.path(paths$docs, "MyManual.pdf"),
  version = read.dcf("DESCRIPTION", fields = "Version")[1],
  prefix = "my-manual"
)

## publish every archived release with the site, and index them
SpaDES.docs::publishManualArchive(
  archiveDir = file.path(paths$prj, "archive", "pdf"),
  docsDir = paths$docs,
  manualName = "My Manual"
)
```

Take the version from `DESCRIPTION` rather than setting it in the build
script. Two places to edit is one place to forget, and
[`archiveManualPDF()`](https://predictiveecology.github.io/SpaDES.docs/reference/archiveManualPDF.md)
refuses an empty version rather than writing `my-manual-v.pdf` over the
previous release.

Link the archive from the book’s preface with a **static** link to
`archive/`. The page behind it is regenerated every build from the files
actually present, so nothing has to be edited when a release is cut.

### Committing the new PDF

Have the workflow do it, so a release does not depend on someone
remembering:

``` yaml
      - name: Archive the release PDF
        if: github.event_name != 'pull_request'
        env:
          GH_TOKEN: ${{ secrets.GITHUB_TOKEN }}
        run: |
          set -euo pipefail
          if [ -z "$(git status --porcelain archive/pdf)" ]; then
            echo "No newly archived PDF; nothing to commit."
            exit 0
          fi
          git config --local user.name "github-actions[bot]"
          git config --local user.email "41898282+github-actions[bot]@users.noreply.github.com"
          git add archive/pdf
          git commit -m "Archive the manual PDF"
          git push "https://x-access-token:${GH_TOKEN}@github.com/${GITHUB_REPOSITORY}.git" \
            "HEAD:${GITHUB_REF_NAME}"
```

This is self-gating:
[`archiveManualPDF()`](https://predictiveecology.github.io/SpaDES.docs/reference/archiveManualPDF.md)
writes a file only when a PDF was rendered and that version is not
already archived, so the step is a no-op on ordinary builds and needs no
separate release trigger.

### Do not use `clean-exclude` for this

Keeping the archive only on the published branch, preserved with the
deploy action’s `clean-exclude`, looks simpler and is worse. It
reintroduces files that linger on that branch without any build
producing them – which is how a stale PDF from a previous era ends up
published beside a current one. Deploying with `clean: true`, from a
directory the build fully populates, keeps the branch an exact image of
what was built.

## Adding a module

Three things change together, and two of them fail quietly:

1.  a checkout step in the workflow;
2.  a chapter entry in `_bookdown.yml`;
3.  the module list in the manual’s landing page, if it has one.

Miss the second and the chapter is fetched, staged and silently absent
from the book.
[`prepManualRmds()`](https://predictiveecology.github.io/SpaDES.docs/reference/prepManualRmds.md)
warns in both directions for this reason. Miss the third and it is in
the book but not on the landing page, which nothing checks.
