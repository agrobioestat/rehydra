## Test environments

- Local Windows 11, R 4.5.3

## R CMD check results

Checked from the built tarball with the CRAN incoming checks and their
remote URL/DOI validation enabled:

```
_R_CHECK_CRAN_INCOMING_=TRUE _R_CHECK_CRAN_INCOMING_REMOTE_=TRUE \
  R CMD check --as-cran rehydra_0.2.0.tar.gz
```

- 0 ERROR
- 0 WARNING
- 1 NOTE: `New submission`

No other NOTE. In particular the DOI, URL, line-ending and example-timing
checks all pass.

## Comments

This is version 0.2.0. It renames the package to lower case (`rehydra`) and
addresses the points raised in the previous review.

### Changes made in response to the feedback

- **Package name.** Renamed from `Rehydra` to `rehydra` throughout DESCRIPTION,
  NAMESPACE, `inst/CITATION`, the R sources, the tests, the vignettes and the
  bundled Shiny application.

- **Wording in DESCRIPTION.** "inspired by" was replaced with "based on" and
  "following", so every reference now states a concrete methodological
  relationship rather than a vague one.

- **DOIs.** Every DOI shipped in DESCRIPTION, `inst/CITATION`, the `@references`
  blocks, the README and the vignettes was confirmed twice: against the CrossRef
  metadata API and by resolving `https://doi.org/<doi>` (HTTP 200). The four
  cited works are Lloret et al. (2011), Xu et al. (2010), Ingrisch and Bahn
  (2018) and Ribeiro et al. (2021). `rehydra_references()` exposes the
  per-reference `doi_verified` flag, and `rehydra_check_dois()` re-runs the
  resolution check interactively.

- **Uncited components.** The recovery-period, total-reduction, mean-reduction
  and mean-recovery-rate components carry no citation. The works originally
  suggested for them could not be verified, and rather than attach an
  unconfirmed attribution their definitions are stated in full as mathematics in
  the help pages. The verification checklist lives at the top of
  `R/references.R`.

- **Line endings.** `.gitattributes` (`* text=auto eol=lf`) enforces LF for every
  contributor, and `tools/lf-endings.R` (build-ignored) converts what is already
  on disk. All sources are LF.

- **LazyData.** `LazyData: true` is paired with `LazyDataCompression: xz` for the
  `data/` directory, which contains the documented example dataset
  `rehydra_data`.

### Offline behaviour

Examples, tests and vignettes run without an internet connection and never
launch the Shiny application. `rehydra_check_dois()` is the only function that
would contact the network; it is documented as interactive-only and its example
is wrapped in `\dontrun{}`.

### URLs

`URL:` and `BugReports:` point to https://github.com/agrobioestat/rehydra,
which is public and returns 200. `R CMD check --as-cran` fetches every URL it
finds and reports a 404 as a NOTE, and GitHub answers 404 for private
repositories, so the repository has to stay public. Verify before each
submission:

```
curl -s -o /dev/null -w "%{http_code}\n" https://github.com/agrobioestat/rehydra
```

### Example timings

`summarize_rehydra()` runs the whole pipeline, so its example uses a single
variable to stay well under 5 seconds; the multi-variable form is shown in a
`\donttest{}` block.

### Notes

- The only NOTE is that this is a new submission under a new package name.
- Some environments additionally report a future timestamp from clock
  validation (`unable to verify current time`); that comes from the check
  environment, not from the package.
