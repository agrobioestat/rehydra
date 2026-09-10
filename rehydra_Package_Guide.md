# rehydra Package Guide

Version: 0.1.1  
Date: 2026-04-29

## 1) What rehydra does

rehydra provides a full workflow for drought-rewatering plant physiology analysis, from data preparation to segmentation, index calculation, memory effects, statistical comparisons, plots, and report generation.

The package was designed for repeated drought cycles and genotype-level interpretation of acclimation and priming signals.

## 2) Minimum data structure

rehydra supports wide and long input formats. A practical long-format structure includes:

- `plant_id`
- `genotype` (for example: `Sensitive`, `Tolerant`)
- `time_days` or numeric time variable
- `treatment` (for example: `Control`, `Drought`)
- `replicate`
- `cycle`
- `phase` (`PreDrought`, `Drought`, `Rewatering`, `PostDrought`)
- `variable`
- `value`

Bundled data (`rehydra_data`) and bundled CSV (`inst/extdata/rehydra_example.csv`) are aligned with this schema.

## 3) Core workflow

1. Prepare and standardize data:
   - `prepare_rehydra_data()`
2. Validate inputs:
   - `check_rehydra_data()`
3. Segment drought-rewatering cycles:
   - `segment_drought_cycle()` (automatic or manual)
4. Compute phase-level dynamics:
   - `damage_metrics()`
   - `recovery_metrics()`
5. Compute resilience indices:
   - `resilience_index()`
6. Compute memory effects:
   - `memory_effect()`
   - `priming_classification()`
7. Compare groups and summarize:
   - `compare_genotypes()`
   - `stability_index()`
   - `summarize_rehydra()`
8. Visualize and export:
   - plotting functions and `rehydra_report()`

## 4) Implemented formulas

rehydra uses ratio-based resilience definitions:

- Resistance: `Rt = Drought / PreDrought`
- Recovery: `Rc = PostDrought / Drought`
- Resilience: `Rs = PostDrought / PreDrought`

Memory terms:

- `Mem_Rt = Rt_C2 - Rt_C1`
- `Mem_Rc = Rc_C2 - Rc_C1`
- `Mem_Rs = Rs_C2 - Rs_C1`
- `Mem_Damage = damage_auc_C1 - damage_auc_C2`
- `Mem_RecRate = recovery_rate_C2 - recovery_rate_C1`
- `Mem_Residual = residual_cost_C1 - residual_cost_C2`

Direction-aware transformations and a positive performance shift are applied before ratio calculations to stabilize interpretation across variables with different sign conventions (for example, water potential).

## 5) Segmentation modes

### Automatic mode

- Detects cycle boundaries from the trajectory.
- Handles moderate noise and missing values.
- Returns cycle-phase assignments and confidence.
- Falls back to conservative behavior when boundaries are weak, with informative errors if fallback is disabled.

### Manual mode

- Uses explicit phase windows.
- Recommended when domain knowledge (irrigation events) is known.
- Suitable for QC and reproducible benchmarking.

## 6) Shiny app behavior

`run_rehydra_app()` launches a local app with:

- CSV upload.
- Column mapping for wide/long data.
- Automatic and manual segmentation.
- Trajectory, damage, recovery, resilience, memory, and comparison tabs.
- Downloadable CSV, figure, and HTML report.

The app now includes:

- **Use bundled example data** toggle.
- **Run bundled demo** button for one-click validation using a complete two-cycle dataset.

This is useful for onboarding, teaching, and user-side pipeline verification.

## 7) Statistical interpretation notes

- Ratio indices can become unstable if denominators approach zero; inspect distribution tails and NA patterns.
- Memory interpretation should be trait-aware and biologically contextual.
- Use bootstrap confidence intervals for uncertainty quantification whenever sample size allows.
- Interpret overcompensation (`Rs > 1`) cautiously; validate against biological plausibility and measurement noise.

## 8) Recommended analytical extensions

The package is already strong, but these additions would be valuable:

1. Time-to-recovery threshold metrics:
   - days to 50% and 90% return to baseline.
2. Integrated drought intensity:
   - cumulative deficit relative to control curve across drought windows.
3. Dynamic slope diagnostics:
   - maximum decline rate and maximum rehydration rate by cycle.
4. Hysteresis metrics:
   - separation between drying and rewatering trajectories over the same value range.
5. Mixed-model comparison helpers:
   - optional wrappers for repeated measures and random effects (`plant_id`, `block`).
6. Robust median-based alternatives:
   - sensitivity analyses for outlier-heavy physiological series.

## 9) Quality and CRAN-readiness checklist

Before CRAN submission, confirm:

1. `devtools::document()` updates docs and NAMESPACE automatically.
2. `devtools::test()` passes with no failures.
3. `devtools::check(args = '--as-cran')` returns 0 errors and 0 warnings.
4. `DESCRIPTION`, `inst/CITATION`, and `cran-comments.md` are final.
5. Examples are fast and do not require internet.
6. Shiny app launches only when explicitly called.
7. English-only package content is preserved.

## 10) Suggested immediate next step

Run one final full check on a clean R session and submit the generated source tarball from version `0.1.1` (or the next bumped version if additional changes are made).
