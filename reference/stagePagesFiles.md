# Stage the files GitHub Pages needs, inside the published directory

A deploy action publishes the *contents* of the rendered book directory,
so anything written to the repository root never reaches the site. Two
files have to be in the published directory itself:

## Usage

``` r
stagePagesFiles(path, cname = NULL)
```

## Arguments

- path:

  the rendered book directory, i.e. `_bookdown.yml`'s `output_dir`.

- cname:

  custom domain for the site, or `NULL` (the default) to write no
  `CNAME` file. Pass the bare domain, without a scheme.

## Value

`path`, invisibly.

## Details

- `.nojekyll`, without which GitHub Pages runs the output through Jekyll
  and drops the `_`-prefixed directories bookdown emits;

- `CNAME`, without which GitHub resets a custom domain to the default
  `*.github.io` address on the next deploy.

Writing these at the repository root appears to work for as long as an
earlier deploy left copies behind and the action deploys with
`clean = false`. A branch published for the first time has neither.
