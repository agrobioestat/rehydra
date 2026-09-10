example_segments <- function(vars = c("water_potential", "stomatal_conductance")) {
  data(rehydra_data, package = "rehydra", envir = environment())
  prepared <- suppressWarnings(prepare_rehydra_data(
    data = rehydra_data, time = time_days, genotype = genotype,
    treatment = treatment, replicate = replicate, variables = dplyr::all_of(vars)
  ))
  segment_drought_cycle(prepared, time = time, value = transformed_value)
}

# ---- rehydra_indices --------------------------------------------------------

test_that("rehydra_indices joins every family on one key without duplicates", {
  seg <- example_segments()
  idx <- rehydra_indices(seg)

  expect_false(anyDuplicated(names(idx)) > 0)
  expect_equal(nrow(idx), nrow(resilience_index(seg)))

  for (col in c("Rs", "damage_auc", "recovery_rate", "total_reduction",
                "mean_reduction", "deficit_auc", "t50")) {
    expect_true(col %in% names(idx), label = col)
  }
})

test_that("the shared landmarks agree across families, so the join is lossless", {
  seg <- example_segments()
  idx <- rehydra_indices(seg)

  # `baseline` (period/reduction/shape) and `PreDrought` (resilience/damage) are
  # the same quantity computed by different functions from one geometry.
  expect_equal(idx$baseline, idx$PreDrought, tolerance = 1e-8)

  # Values must match the individual families exactly.
  key <- c("genotype", "treatment", "replicate", "plant_id", "variable", "cycle")
  ref <- dplyr::arrange(curve_shape_metrics(seg), dplyr::across(dplyr::all_of(key)))
  got <- dplyr::arrange(idx, dplyr::across(dplyr::all_of(key)))
  expect_equal(got$deficit_auc, ref$deficit_auc, tolerance = 1e-10)
  expect_equal(got$t50, ref$t50, tolerance = 1e-10)
})

test_that("families can be selected and prefixed", {
  seg <- example_segments()

  only_two <- rehydra_indices(seg, families = c("resilience", "shape"))
  expect_true("Rs" %in% names(only_two))
  expect_false("total_reduction" %in% names(only_two))

  pref <- rehydra_indices(seg, families = c("resilience", "recovery_period"), prefix = TRUE)
  expect_true("resilience_Rs" %in% names(pref))
  expect_true("recovery_period_baseline" %in% names(pref))
})

# ---- composite_resilience ---------------------------------------------------

test_that("a composite of identical variables reproduces the variable itself", {
  base <- tibble::tibble(
    genotype = rep(c("A", "B"), each = 6),
    treatment = "Drought",
    replicate = rep(as.character(1:6), 2),
    plant_id = paste0("p", 1:12),
    cycle = 1L,
    Rs = c(seq(0.5, 1.0, length.out = 6), seq(0.6, 0.9, length.out = 6))
  )
  idx <- dplyr::bind_rows(
    dplyr::mutate(base, variable = "v1"),
    dplyr::mutate(base, variable = "v2")
  )

  sc <- composite_resilience(idx, metric = "Rs")

  # Two perfectly correlated variables: one component explains everything and the
  # composite is a monotone rescaling of the original index.
  expect_equal(attr(sc, "variance_explained"), 1, tolerance = 1e-8)
  ordered <- dplyr::arrange(dplyr::left_join(
    sc, dplyr::filter(idx, .data$variable == "v1"),
    by = c("genotype", "treatment", "replicate", "plant_id", "cycle")
  ), .data$Rs)
  expect_equal(stats::cor(ordered$composite, ordered$Rs), 1, tolerance = 1e-8)
})

test_that("the composite sign is aligned so higher always means better", {
  seg <- example_segments()
  res <- resilience_index(seg)

  sc <- composite_resilience(res, metric = "Rs")
  wide <- tidyr::pivot_wider(
    dplyr::select(res, dplyr::all_of(c("plant_id", "cycle", "variable", "Rs"))),
    names_from = "variable", values_from = "Rs"
  )
  joined <- dplyr::left_join(sc, wide, by = c("plant_id", "cycle"))
  mean_rs <- rowMeans(joined[, c("water_potential", "stomatal_conductance")])

  expect_gt(stats::cor(joined$composite, mean_rs, use = "complete.obs"), 0)
})

test_that("weighted and mean methods behave as declared", {
  seg <- example_segments()
  res <- resilience_index(seg)

  w <- composite_resilience(
    res, metric = "Rs", method = "weighted",
    weights = c(water_potential = 3, stomatal_conductance = 1)
  )
  expect_identical(unique(w$method), "weighted")

  ld <- composite_loadings(w)
  # Weights are rescaled to sum to one.
  expect_equal(sum(abs(ld$loading)), 1, tolerance = 1e-8)
  expect_gt(abs(ld$loading[ld$variable == "water_potential"]),
            abs(ld$loading[ld$variable == "stomatal_conductance"]))

  expect_error(
    composite_resilience(res, metric = "Rs", method = "weighted",
                         weights = c(water_potential = 1)),
    "missing"
  )
})

test_that("composite_resilience validates its input", {
  seg <- example_segments(vars = "water_potential")
  res <- resilience_index(seg)

  expect_error(composite_resilience(res, metric = "Rs"), "At least two variables")
  expect_error(
    composite_resilience(tibble::tibble(Rs = 1:3), metric = "Rs"),
    "variable"
  )
})

# ---- detrend_diurnal --------------------------------------------------------

sim_diurnal <- function(amp = 0.06, slope_per_hour = -0.004, by = 3, hours = 47) {
  t_hours <- seq(0, hours, by = by)
  phase <- (t_hours %% 24) / 24
  tibble::tibble(
    genotype = "G1", treatment = "Drought", replicate = "1",
    variable = "gs",
    time = t_hours / 24,
    transformed_value = 0.30 + slope_per_hour * t_hours + amp * sin(2 * pi * phase)
  )
}

test_that("detrending removes the daily rhythm and keeps the drought slope", {
  sim <- sim_diurnal()
  out <- detrend_diurnal(sim, group_by = c("genotype", "variable"))

  # The multi-day decline must survive untouched: -0.004 per hour is -0.096 per
  # day, and `time` is in days.
  slope <- stats::coef(stats::lm(detrended ~ time, data = out))[[2]]
  expect_equal(slope, -0.096, tolerance = 1e-6)

  # The fitted rhythm must have the amplitude that was put in.
  expect_equal(diff(range(out$diurnal)) / 2, 0.06, tolerance = 1e-6)
})

test_that("the raw series is genuinely contaminated, so the fix is doing work", {
  sim <- sim_diurnal()
  raw_slope <- stats::coef(stats::lm(transformed_value ~ time, data = sim))[[2]]

  # Guards the regression that motivated the trend basis: harmonics fitted
  # without a continuous time term recovered -0.076 instead of -0.096.
  expect_false(isTRUE(all.equal(raw_slope, -0.096, tolerance = 1e-3)))
  expect_lt(stats::sd(detrend_diurnal(sim, group_by = "genotype")$detrended),
            stats::sd(sim$transformed_value))
})

test_that("mean_by_hour also preserves the trend", {
  sim <- sim_diurnal(by = 2, hours = 71)
  out <- detrend_diurnal(sim, group_by = c("genotype", "variable"),
                         method = "mean_by_hour", bins = 12)

  slope <- stats::coef(stats::lm(detrended ~ time, data = out))[[2]]
  expect_equal(slope, -0.096, tolerance = 0.02)
})

test_that("a daily series is refused rather than detrended", {
  daily <- tibble::tibble(
    genotype = "G1", treatment = "D", replicate = "1", variable = "gs",
    time = 0:9, transformed_value = seq(1, 0.5, length.out = 10)
  )

  expect_warning(out <- detrend_diurnal(daily, group_by = "genotype"),
                 "sub-daily")
  expect_equal(out$detrended, daily$transformed_value)
})

test_that("POSIXct input is accepted", {
  sim <- sim_diurnal()
  sim$stamp <- as.POSIXct("2024-01-01 00:00", tz = "UTC") + sim$time * 86400

  out <- detrend_diurnal(sim, time = stamp, group_by = c("genotype", "variable"))
  expect_true(all(c("day", "phase_of_day", "diurnal", "detrended") %in% names(out)))
  expect_equal(stats::coef(stats::lm(detrended ~ time, data = out))[[2]],
               -0.096, tolerance = 1e-5)
})

# ---- memory_power -----------------------------------------------------------

test_that("power matches power.t.test up to the neglected lower tail", {
  ours <- memory_power(effect = 0.05, sd = 0.03, n = 6)$power
  ref <- stats::power.t.test(n = 6, delta = 0.05, sd = 0.03,
                             sig.level = 0.05, type = "two.sample")$power

  # `power.t.test` drops the far tail of the non-central t; this implementation
  # keeps both, so it is very slightly larger. The difference is below 1e-5.
  expect_equal(ours, ref, tolerance = 1e-4)
  expect_gte(ours, ref)
})

test_that("solving for n and for power are consistent", {
  need <- memory_power(effect = 0.05, sd = 0.03, power = 0.8)
  expect_gte(need$power, 0.8)

  # One replicate fewer must fall short, or `n` was not the smallest sufficient.
  short <- memory_power(effect = 0.05, sd = 0.03, n = need$n - 1)
  expect_lt(short$power, 0.8)
})

test_that("power increases with effect and n, and decreases with sd", {
  p_small <- memory_power(effect = 0.02, sd = 0.03, n = 6)$power
  p_large <- memory_power(effect = 0.10, sd = 0.03, n = 6)$power
  expect_lt(p_small, p_large)

  expect_lt(memory_power(effect = 0.05, sd = 0.03, n = 4)$power,
            memory_power(effect = 0.05, sd = 0.03, n = 10)$power)
  expect_gt(memory_power(effect = 0.05, sd = 0.02, n = 6)$power,
            memory_power(effect = 0.05, sd = 0.06, n = 6)$power)
})

test_that("a paired design needs fewer replicates than independent groups", {
  indep <- memory_power(effect = 0.05, sd = 0.03, power = 0.8)
  paired <- memory_power(effect = 0.05, sd = 0.03, power = 0.8,
                         design = "paired", correlation = 0.5)

  expect_lt(paired$n, indep$n)
  expect_equal(paired$effect_size, 0.05 / (0.03 * sqrt(2 * 0.5)), tolerance = 1e-8)
})

test_that("multiplicity corrections reduce power", {
  plain <- memory_power(effect = 0.05, sd = 0.03, n = 6)$power
  bonf <- memory_power(effect = 0.05, sd = 0.03, n = 6,
                       n_groups = 4, adjust = "bonferroni")$power
  tukey <- memory_power(effect = 0.05, sd = 0.03, n = 6,
                        n_groups = 4, adjust = "tukey")$power

  expect_lt(bonf, plain)
  expect_lt(tukey, plain)
  # Tukey is less conservative than Bonferroni for all-pairs comparisons.
  expect_gt(tukey, bonf)
})

test_that("memory_power is vectorized over scenarios and validates input", {
  grid <- memory_power(effect = c(0.03, 0.05, 0.10), sd = 0.03, power = 0.8)
  expect_equal(nrow(grid), 3L)
  expect_true(all(diff(grid$n) <= 0))

  expect_error(memory_power(effect = 0.05, sd = 0.03), "Give either")
  expect_error(memory_power(effect = 0.05, sd = 0.03, n = 4, power = 0.8), "only one")
  expect_error(memory_power(effect = 0.05, sd = 0.03, n = 1), "at least 2")
})

test_that("memory_power_pilot returns the pooled SD and the paired correlation", {
  seg <- example_segments()
  res <- resilience_index(seg)
  pilot <- memory_power_pilot(res, metric = "Rs")

  expect_true(all(c("sd", "correlation", "n_units", "n_cycles") %in% names(pilot)))
  expect_true(all(pilot$sd > 0))
  expect_true(all(abs(pilot$correlation) <= 1, na.rm = TRUE))
  expect_true(all(pilot$n_cycles == 2))

  # The pooled SD must equal the within-cell root mean square deviation.
  one <- dplyr::filter(res, .data$variable == pilot$variable[[1]])
  key <- interaction(one$genotype, one$treatment, one$cycle, drop = TRUE)
  parts <- split(one$Rs, key)
  ss <- sum(vapply(parts, function(v) sum((v - mean(v))^2), numeric(1)))
  df <- sum(vapply(parts, function(v) length(v) - 1L, numeric(1)))
  expect_equal(pilot$sd[[1]], sqrt(ss / df), tolerance = 1e-8)
})
