make_linear_cycle <- function(baseline = 10, v_min = 2) {
  # Baseline for 4 steps, a linear drop over 3 steps, then a linear return that
  # reaches the baseline exactly at t = 14. Everything below is checkable by hand.
  t_pre <- 0:3
  t_drop <- 4:6
  t_rec <- 7:14

  drop_vals <- seq(baseline, v_min, length.out = length(t_drop) + 1L)[-1L]
  rec_vals <- seq(v_min, baseline, length.out = length(t_rec) + 1L)[-1L]

  tibble::tibble(
    genotype = "G1", treatment = "Drought", replicate = "1", plant_id = "p1",
    variable = "v", cycle = 1L,
    time = c(t_pre, t_drop, t_rec),
    transformed_value = c(rep(baseline, length(t_pre)), drop_vals, rec_vals),
    phase = c(
      rep("PreDrought", length(t_pre)),
      rep("Drought", length(t_drop)),
      rep("Rewatering", length(t_rec))
    )
  )
}

test_that("recovery_period_metrics reproduces the trapezoidal total reduction", {
  cyc <- make_linear_cycle()
  out <- recovery_period_metrics(cyc)

  expect_equal(nrow(out), 1L)
  expect_equal(out$baseline, 10)
  expect_equal(out$drought_minimum, 2)
  expect_true(out$stress_detected)

  sub <- cyc[cyc$time >= 4, ]
  d <- pmax(0, 10 - sub$transformed_value)
  manual <- sum(diff(sub$time) * (utils::head(d, -1) + utils::tail(d, -1)) / 2)

  expect_equal(out$total_reduction, manual, tolerance = 1e-8)
  expect_equal(out$total_reduction_relative,
               manual / ((max(sub$time) - 4) * 10), tolerance = 1e-8)
  expect_gt(out$recovery_period, 0)
  expect_false(out$recovery_censored)
})

test_that("an incomplete recovery is right-censored, not truncated", {
  cyc <- make_linear_cycle()
  # Cut the series off while the response is still well below the threshold.
  cyc <- cyc[cyc$time <= 9, ]

  out <- recovery_period_metrics(cyc)
  expect_true(out$recovery_censored)
  expect_true(is.na(out$recovery_period))

  # The rate is still reported over the observed part of the return, flagged.
  sw <- mean_reduction_metrics(cyc)
  expect_true(sw$recovery_censored)
  expect_true(is.na(sw$recovery_period))
  expect_true(is.finite(sw$mean_recovery_rate))
})

test_that("mean_reduction is a proportion and separates intensity from duration", {
  severe_short <- make_linear_cycle(baseline = 10, v_min = 1)
  mild_long <- make_linear_cycle(baseline = 10, v_min = 8)

  a <- mean_reduction_metrics(severe_short)
  b <- mean_reduction_metrics(mild_long)

  # A relative deficit can never exceed 1 for a non-negative response.
  expect_true(a$mean_reduction >= 0 && a$mean_reduction <= 1)
  expect_true(b$mean_reduction >= 0 && b$mean_reduction <= 1)

  # The deeper trough must give the larger mean and maximum reduction.
  expect_gt(a$mean_reduction, b$mean_reduction)
  expect_gt(a$max_reduction, b$max_reduction)
  expect_equal(a$max_reduction, (10 - 1) / 10, tolerance = 1e-8)
})

test_that("max_reduction is the complement of the resistance index Rt", {
  cyc <- make_linear_cycle()
  sw <- mean_reduction_metrics(cyc)
  res <- resilience_index(cyc, drought_summary = "minimum",
                          predrought_summary = "mean")

  expect_equal(sw$max_reduction, 1 - res$Rt, tolerance = 1e-8)
})

test_that("control-like series report stress_detected = FALSE", {
  flat <- make_linear_cycle()
  flat$transformed_value <- 10 + rnorm(nrow(flat), sd = 1e-4)

  th <- recovery_period_metrics(flat)
  sw <- mean_reduction_metrics(flat)

  expect_false(th$stress_detected)
  expect_false(sw$stress_detected)
  expect_true(is.na(sw$mean_recovery_rate))
})

test_that("threshold arguments are validated", {
  cyc <- make_linear_cycle()
  expect_error(recovery_period_metrics(cyc, recovery_level = 2), "recovery_level")
  expect_error(mean_reduction_metrics(cyc, reduction_level = -1), "reduction_level")
})

test_that("both families run on the bundled dataset and align on the trough", {
  data(rehydra_data, package = "rehydra")

  prepared <- suppressWarnings(prepare_rehydra_data(
    data = rehydra_data, time = time_days, genotype = genotype,
    treatment = treatment, replicate = replicate,
    variables = c(water_potential, stomatal_conductance)
  ))
  segments <- segment_drought_cycle(prepared, time = time, value = transformed_value)

  th <- recovery_period_metrics(segments)
  sw <- mean_reduction_metrics(segments)
  cs <- curve_shape_metrics(segments)

  expect_equal(nrow(th), nrow(sw))
  expect_equal(nrow(th), nrow(cs))
  # The three families share one geometry, so the trough must agree exactly.
  expect_equal(th$drought_minimum, sw$drought_minimum)
  expect_equal(th$drought_minimum, cs$drought_minimum)
  expect_equal(th$baseline, cs$baseline)
})
