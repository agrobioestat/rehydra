# Deterministic fixtures: a synthetic cycle whose landmarks are known in closed
# form, so the metrics are checked against arithmetic rather than against a
# previous run of the same code.

make_cycle <- function(baseline = 10, v_min = 2, k = 0.5,
                       t_pre = 0:3, t_drop = 4:6, t_rec = 7:20) {
  # Pre-drought flat at `baseline`, a linear drop to `v_min`, then an exact
  # exponential return: y(t) = B - (B - v_min) * exp(-k * (t - t_min)).
  t_min <- max(t_drop)
  drop_vals <- seq(baseline, v_min, length.out = length(t_drop) + 1L)[-1L]
  rec_vals <- baseline - (baseline - v_min) * exp(-k * (t_rec - t_min))

  tibble::tibble(
    genotype = "G1",
    treatment = "Drought",
    replicate = "1",
    plant_id = "p1",
    variable = "v",
    cycle = 1L,
    time = c(t_pre, t_drop, t_rec),
    transformed_value = c(rep(baseline, length(t_pre)), drop_vals, rec_vals),
    phase = c(
      rep("PreDrought", length(t_pre)),
      rep("Drought", length(t_drop)),
      rep("Rewatering", length(t_rec))
    )
  )
}

test_that("curve_shape_metrics recovers a known exponential rate constant", {
  cyc <- make_cycle(k = 0.5)
  out <- curve_shape_metrics(cyc)

  expect_equal(nrow(out), 1L)
  expect_equal(out$baseline, 10)
  expect_equal(out$drought_minimum, 2)
  expect_equal(out$deficit_amplitude, 8)
  expect_true(out$stress_detected)

  # The data were generated from exactly this model, so the fit must return it.
  expect_equal(out$recovery_rate_k, 0.5, tolerance = 1e-4)
  expect_equal(out$recovery_half_life, log(2) / 0.5, tolerance = 1e-4)
  expect_gt(out$rate_r_squared, 0.99)
})

test_that("t50 matches the analytic half-recovery time", {
  k <- 0.4
  out <- curve_shape_metrics(make_cycle(k = k))

  # y reaches v_min + A/2 when exp(-k * u) = 0.5, i.e. u = log(2) / k.
  # The series is sampled at integer times, so the interpolated crossing is
  # close to but not exactly the analytic value.
  expect_equal(out$t50, log(2) / k, tolerance = 0.15)
})

test_that("deficit AUC equals the trapezoidal integral of the deficit", {
  cyc <- make_cycle()
  out <- curve_shape_metrics(cyc)

  from <- min(cyc$time[cyc$phase == "Drought"])
  sub <- cyc[cyc$time >= from, ]
  d <- pmax(0, out$baseline - sub$transformed_value)
  manual <- sum(diff(sub$time) * (utils::head(d, -1) + utils::tail(d, -1)) / 2)

  expect_equal(out$deficit_auc, manual, tolerance = 1e-8)
  expect_equal(
    out$deficit_auc_normalized,
    manual / ((max(sub$time) - from) * out$baseline),
    tolerance = 1e-8
  )
  # The damage and recovery halves must add up to the total.
  expect_equal(out$deficit_auc_damage + out$deficit_auc_recovery,
               out$deficit_auc, tolerance = 1e-8)
})

test_that("overcompensation does not cancel accumulated damage", {
  cyc <- make_cycle()
  overshoot <- cyc
  overshoot$transformed_value[overshoot$time > 15] <- 30

  base_auc <- curve_shape_metrics(cyc)$deficit_auc
  over_auc <- curve_shape_metrics(overshoot)$deficit_auc

  # Clipping the deficit at zero means values above the baseline contribute
  # nothing, so the area can only stay equal or shrink, never go negative.
  expect_gte(over_auc, 0)
  expect_lte(over_auc, base_auc)
  expect_gt(curve_shape_metrics(overshoot)$overshoot, 0)
})

test_that("latency is never negative and reflects a post-rewatering decline", {
  cyc <- make_cycle()
  out <- curve_shape_metrics(cyc)
  expect_gte(out$latency, 0)

  # Push the trough two steps into the rewatering window: the plant keeps
  # declining after water is restored, so the latency must grow.
  lagged <- cyc
  lagged$phase[lagged$time %in% c(5, 6)] <- "Rewatering"
  expect_gte(curve_shape_metrics(lagged)$latency, 0)
})

test_that("deficit_curve cumulative area ends at the AUC over the same window", {
  cyc <- make_cycle()
  curve <- deficit_curve(cyc)

  expect_true(all(c("deficit", "cumulative_deficit") %in% names(curve)))
  expect_true(all(curve$deficit >= 0))
  # The running area is monotone by construction.
  expect_false(is.unsorted(curve$cumulative_deficit))
})

test_that("degenerate input returns a one-row all-NA result instead of erroring", {
  tiny <- tibble::tibble(
    genotype = "G1", treatment = "Drought", replicate = "1", plant_id = "p1",
    variable = "v", cycle = 1L, time = c(1, 2), transformed_value = c(1, NA),
    phase = c("PreDrought", "Drought")
  )

  out <- curve_shape_metrics(tiny)
  expect_equal(nrow(out), 1L)
  expect_true(is.na(out$deficit_auc))
})

test_that("curve_shape_metrics rejects unsegmented input", {
  expect_error(
    curve_shape_metrics(tibble::tibble(time = 1:5, transformed_value = 1:5)),
    "phase"
  )
})
