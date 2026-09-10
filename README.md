# rehydra

`rehydra` quantifies drought damage, resistance, recovery, resilience, and physiological memory (priming) in plant time series subjected to multiple drought and rewatering cycles.

It is built for **designed experiments on live plants** measured at daily or
sub-daily resolution, which is what separates it from `pointRes`: physiological
variables with different directions of improvement, several imposed cycles in one
experiment, curve-shape descriptors that need more than one point per cycle, an
analysis of variance on the indices themselves, and between-cycle memory indices.
See `vignette("rehydra-vs-existing-packages")`.

## Installation

```r
install.packages("rehydra")

# development version
# remotes::install_github("agrobioestat/rehydra")
```

## Minimal example

```r
data(rehydra_data)

prepared <- prepare_rehydra_data(
  data = rehydra_data,
  time = time_days,
  genotype = genotype,
  treatment = treatment,
  replicate = replicate,
  variables = c(water_potential, stomatal_conductance)
)

segments <- segment_drought_cycle(
  data = prepared,
  time = time,
  value = transformed_value,
  group_by = c("genotype", "treatment", "replicate", "variable"),
  method = "auto",
  response_direction = "auto"
)

resilience <- resilience_index(segments)
memory <- memory_effect(resilience)

plot_recovery_trajectory(
  data = segments,
  resilience_results = resilience,
  facet_by = c("genotype", "variable")
)
```

## Extended indices and curve shape

```r
recovery_period <- recovery_period_metrics(segments)     # recovery period, total reduction
mean_reduction <- mean_reduction_metrics(segments) # mean recovery rate, mean reduction
shape <- curve_shape_metrics(segments)

shape[, c("deficit_auc", "t50", "recovery_rate_k", "latency")]

# Full series with the area under the deficit curve shaded
plot_deficit_auc(segments[segments$treatment == "Drought" &
  segments$variable == "stomatal_conductance" &
  segments$replicate == "1", ])
```

## Memory across cycles and designed analysis

```r
# Every index family in one table
idx <- rehydra_indices(segments)

# Average change in the index per additional drought cycle
memory_trend(resilience, min_cycles = 2)

# The indices as responses in the experimental design
fit <- rehydra_anova(resilience, response = "Rs",
                     factors = c("genotype", "treatment"))
fit$anova
fit$comparisons

# Repeated measures: the same plants across cycles
rehydra_anova(resilience, response = "Rs",
              factors = c("genotype", "treatment", "cycle"),
              by = "variable", random = "plant_id")

# Overall stability: OSt = recovery rate / disturbance rate
st <- overall_stability(segments)
st[, c("genotype", "cycle", "disturbance_rate", "recovery_rate",
       "overall_stability")]

# A rising OSt across cycles is the signature of drought memory
memory_trend(st, metrics = "overall_stability", min_cycles = 2)

# One resilience score from several physiological variables
score <- composite_resilience(resilience, metric = "Rs")
composite_loadings(score)

# How many pots per cell to detect a 0.05 change in Rs?
memory_power(effect = 0.05, sd = 0.03, power = 0.8)

# Sub-daily data: strip the daily rhythm before segmenting
# detrend_diurnal(prepared, time = timestamp)
```

## Complete workflow

```r
analysis <- summarize_rehydra(
  data = rehydra_data,
  time = time_days,
  genotype = genotype,
  treatment = treatment,
  replicate = replicate,
  variables = c(water_potential, stomatal_conductance),
  response_direction = "auto",
  transform = "none",
  segmentation_method = "auto"
)

summary(analysis)
plot(analysis)
```

## Main functions

**Data and segmentation**

- `prepare_rehydra_data()`, `check_rehydra_data()`, `segment_drought_cycle()`

**Index families**

| Family | Function | Indices |
|---|---|---|
| Lloret et al. (2011) | `resilience_index()` | `Rt`, `Rc`, `Rs` |
| Duration and total loss | `recovery_period_metrics()` | `recovery_period`, `total_reduction` |
| Average intensity and rate | `mean_reduction_metrics()` | `mean_reduction`, `mean_recovery_rate` |
| Curve shape | `curve_shape_metrics()`, `deficit_curve()` | `deficit_auc`, `t50`, `recovery_rate_k`, `latency` |
| Damage / recovery | `damage_metrics()`, `recovery_metrics()`, `stability_index()` | drop, rate, AUC, residual cost |
| Stability | `overall_stability()`, `stability_index()` | `impact`, `disturbance_rate`, `recovery_rate`, `OSt` |
| Memory | `memory_effect()`, `memory_trend()`, `priming_classification()` | `Mem_*`, memory rate, priming class |
| Combined | `rehydra_indices()`, `composite_resilience()` | all families joined, multivariable score |

**Inference and reporting**

- `rehydra_anova()` (fixed, blocked or mixed / repeated measures),
  `compare_genotypes()`
- `memory_power()`, `memory_power_pilot()` for replication planning
- `detrend_diurnal()` for sub-daily series
- `summarize_rehydra()`, `rehydra_report()`, `run_rehydra_app()`
- `rehydra_references()`, `rehydra_check_dois()`

**Graphics**

- `plot_recovery_trajectory()`, `plot_deficit_auc()`, `plot_curve_shape()`,
  `plot_extended_indices()`, `plot_resilience_indices()`, `plot_memory_effect()`,
  `plot_memory_trend()`, `plot_damage_recovery()`, `plot_stability()`

## Example data

`rehydra_data` is a simulated wide-format dataset with:

- `Sensitive` and `Tolerant` genotypes
- `Control` and `Drought` treatments
- two complete drought-rewatering cycles
- `water_potential` and `stomatal_conductance`
- controlled missing values for validation examples

## Shiny app

```r
# interactive use
run_rehydra_app()
```

Six sections, each with its own panels:

| Section | Panels |
|---|---|
| Data | Data, Segmentation |
| Dynamics | Trajectory, Deficit curve (AUC) |
| Indices | Damage and Recovery, Extended indices, Resilience, Stability |
| Memory | Memory, Multi-cycle memory |
| Statistics | Designed analysis, Genotype comparison |
| Export | Downloads |

## References

- Lloret, F., Keeling, E. G., and Sala, A. (2011). <doi:10.1111/j.1600-0706.2011.19372.x>
- Xu, Z., Zhou, G., and Shimizu, H. (2010). <doi:10.4161/psb.5.6.11398>
- Ingrisch, J., and Bahn, M. (2018). <doi:10.1016/j.tree.2018.01.013>
- Ribeiro, R. V., Vitti, K. A., Marcos, F. C. C., Souza, G. M., Pissolato, M. D.,
  Almeida, L. F. R., and Machado, E. C. (2021). <doi:10.1016/j.jplph.2021.153397>

Run `rehydra_references()` for the machine-readable list; every DOI there was
confirmed against CrossRef and by resolving it. The recovery-period,
total-reduction, mean-reduction and mean-recovery-rate components are
deliberately uncited: their definitions are given as mathematics in the help
pages rather than attached to an unverified attribution.

## Development status

Version `0.2.0`: extended index families (recovery period, mean reduction,
curve shape), multi-cycle memory, designed-experiment analysis including mixed
models, multivariable composite scores, diurnal detrending, replication
planning, and CRAN-readiness fixes.

## License

MIT + file LICENSE

## Suggested citation

See `citation("rehydra")` after installation.
