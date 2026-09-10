# rehydra 0.2.0

## Package name and CRAN metadata

- The package is now named `rehydra` (lower case) throughout DESCRIPTION,
  NAMESPACE, `inst/CITATION`, the vignettes, the tests and the bundled Shiny
  app. Installed and loaded as `library(rehydra)`.
- `DESCRIPTION` was rewritten: "inspired by" was replaced with "based on" and
  "following", the new index families were added to the description, and
  `LazyData: true` is paired with `LazyDataCompression: xz` for the `data/`
  directory.
- The unresolvable DOI `10.1016/j.jplph.2021.153397` (Ribeiro et al. 2021) was
  removed from DESCRIPTION, `inst/CITATION` and every `@references` block,
  because `R CMD check --as-cran` reports it as a possibly invalid DOI. The work
  is cited by author and year until the identifier is verified. See the
  checklist at the top of `R/references.R`.
- Added `.gitattributes` (`* text=auto eol=lf`) and `tools/lf-endings.R` to
  enforce LF line endings and avoid the CRLF check note.

## New index families

- `recovery_period_metrics()`: recovery period and total reduction. Recovery
  that does not complete within the observation window is reported as
  right-censored (`NA` plus `recovery_censored = TRUE`) rather than truncated.
- `mean_reduction_metrics()`: mean reduction and mean recovery rate. The mean
  reduction is a time-weighted mean over the impaired window only, so it
  separates the intensity of the stress from its duration.
- Both families are documented by their mathematics rather than by attribution:
  the definitions are stated in full in the roxygen `@details`, and no
  literature is cited for them because the citations originally suggested for
  these components could not be verified. Citing an unverified work is worse
  than describing the computation precisely, which is what the help pages do.
- `curve_shape_metrics()`: shape descriptors read off the whole trajectory —
  area under the deficit curve (total, damage and recovery shares, normalized),
  time to half recovery (`t50`), the exponential recovery rate constant with its
  half-life and fit quality, response latency, damage latency, recovery
  symmetry, residual deficit and overshoot.
- `deficit_curve()`: the point-by-point deficit and its running area, so the
  shaded region of the figure can be reproduced or exported.
- All three families share one internal geometry routine, so they cannot
  disagree about the baseline or the trough. `stress_detected` flags cycles
  whose trough never left the baseline.

## Multi-cycle memory and experimental design

- `memory_trend()`: fits the whole sequence of cycles and returns the memory
  rate (average change per additional cycle) with its interval, p-value,
  cumulative change, `monotonic_fraction` and a `memory_class` label. Damage-type
  indices have their direction flipped automatically.
- `memory_direction()`: exposes the "lower is better" mapping used above.
- `rehydra_anova()`: fits the randomized-complete-block factorial model implied
  by the design on any index, returning the ANOVA table, marginal means based on
  the pooled residual error, all pairwise Tukey contrasts and normality and
  homogeneity diagnostics. One model per variable and per cycle.
- `rehydra_anova(random = )` supports **repeated measures across cycles**: put
  `cycle` among the `factors` and declare the plant as a random intercept, and
  every reported table - ANOVA, model-based marginal means and Tukey contrasts -
  comes from the mixed model. `diagnostics$model_type` and
  `diagnostics$p_value_method` say which model and which degrees of freedom
  produced them; p-values use Satterthwaite approximation when `lmerTest` is
  installed and are reported as `NA` when it is not, rather than being computed
  from residual degrees of freedom a mixed model does not have.

## Combining indices and variables

- `rehydra_indices()`: every index family joined on one group-and-cycle key, the
  natural input to `rehydra_anova()` and `memory_trend()`.
- `composite_resilience()` and `composite_loadings()`: collapse an index measured
  on several physiological variables into a single score, by PCA (with the
  variance explained reported as a diagnostic of whether one score is a fair
  summary), by an unweighted mean, or with user-supplied weights.

## Sub-daily series

- `detrend_diurnal()`: separates the daily rhythm from the drought signal in
  series measured several times a day, by harmonic regression or by time-of-day
  bin means. The rhythm is estimated jointly with an across-days component, so
  the multi-day drought course is preserved rather than partly absorbed into the
  daily wave.

## Experimental design

- `memory_power()` and `memory_power_pilot()`: replication needed to detect a
  given between-cycle effect, from the non-central t distribution, with paired
  and independent designs and Bonferroni or Tukey multiplicity corrections.

## Graphics

- `plot_deficit_auc()`: the full time series with the deficit area shaded and
  the trough and `t50` marked.
- `plot_curve_shape()`, `plot_extended_indices()`, `plot_memory_trend()`.

## Shiny application

- New panels: **Deficit curve (AUC)**, **Extended indices**, **Multi-cycle
  memory** and **Designed analysis**.
- The thirteen panels are grouped into six sections (Data, Dynamics, Indices,
  Memory, Statistics, Export) instead of one flat strip that overflowed the
  width of a normal screen.
- Flexbox layout with a collapsible control sidebar, summary tiles for the
  numbers worth seeing without opening a table (cycles, groups, cycles with
  detected stress, share of censored recoveries, mean segmentation confidence),
  rounded tables with `stress_detected` and `recovery_censored` highlighted, and
  a progress bar during the analysis.
- The CSV export now includes the period, reduction, curve-shape, consecutive-cycle
  and memory-trend tables, each tagged by `result_table`.

## Other

- `summarize_rehydra()` gained `memory_min_cycles` and now returns `recovery_period`,
  `mean_reduction`, `curve_shape` and `memory_trend` components; `print()`, `summary()`
  and `augment()` were updated accordingly.
- `rehydra_references()` and `rehydra_check_dois()` document and verify the
  literature behind each index family.
- New vignette: *How rehydra Differs from Existing Packages*.
- New test files for the curve-shape, period/reduction, memory-trend, design,
  reference and new-feature code, checked against closed-form values and against
  independent implementations (`lmerTest`, `power.t.test`, `aov`) rather than
  against previous output. The bundled Shiny app is now exercised end to end
  with `shiny::testServer()`.

# rehydra 0.1.1

- Improved direction-aware data preparation for ratio-based resilience metrics.
- Stabilized positive-scale transformation to avoid near-zero denominators in ratio indices.
- Hardened automatic drought-cycle segmentation for noisy and partially missing series.
- Added informative auto-segmentation failure behavior when fallback is disabled.
- Improved robustness of damage, recovery, resilience, and memory calculations with incomplete phases.
- Fixed overcompensation capping consistency in recovery metrics.
- Added index-specific bootstrap confidence interval columns for resilience and memory.
- Hardened plotting functions to return valid `ggplot` objects with partial inputs.
- Reworked the bundled Shiny interface with improved UX, validation, and error handling.
- Improved group-wise p-value adjustment in genotype comparisons.
- Expanded tests for segmentation confidence, direction handling, bootstrap columns, S3 methods, and Shiny app structure.
- Updated package metadata and citation placeholders for CRAN readiness.

# rehydra 0.1.0

- Initial functional release.
