# A cycle whose two rates are exact by construction: the trait falls from 10 to
# 2 over 8 time units (slope -1, so DR = 1) and returns from 2 to 10 over 4
# (slope +2, so RR = 2). Everything below is checkable by hand.
make_rate_cycle <- function(decline_slope = -1, recovery_slope = 2,
                            baseline = 10, trough = 2) {
  n_down <- round((baseline - trough) / abs(decline_slope))
  n_up <- round((baseline - trough) / recovery_slope)

  t_pre <- 0:2
  t_down <- seq_len(n_down) + max(t_pre)
  t_up <- seq_len(n_up) + max(t_down)

  tibble::tibble(
    genotype = "G1", treatment = "Drought", replicate = "1", plant_id = "p1",
    variable = "v", cycle = 1L,
    time = c(t_pre, t_down, t_up),
    transformed_value = c(
      rep(baseline, length(t_pre)),
      baseline + decline_slope * seq_len(n_down),
      trough + recovery_slope * seq_len(n_up)
    ),
    phase = c(
      rep("PreDrought", length(t_pre)),
      rep("Drought", length(t_down)),
      rep("Rewatering", length(t_up))
    )
  )
}

test_that("the two rates are the least-squares slopes of their windows", {
  out <- overall_stability(make_rate_cycle())

  expect_equal(nrow(out), 1L)
  expect_equal(out$baseline, 10)
  expect_equal(out$drought_minimum, 2)

  # DR is reported as a positive magnitude of decline.
  expect_equal(out$disturbance_rate, 1, tolerance = 1e-8)
  expect_equal(out$recovery_rate, 2, tolerance = 1e-8)

  # Both windows are exactly linear here, so the fits are perfect.
  expect_equal(out$disturbance_r_squared, 1, tolerance = 1e-8)
  expect_equal(out$recovery_r_squared, 1, tolerance = 1e-8)
})

test_that("OSt is the ratio of the recovery rate to the disturbance rate", {
  out <- overall_stability(make_rate_cycle())
  expect_equal(out$overall_stability, out$recovery_rate / out$disturbance_rate,
               tolerance = 1e-10)
  expect_equal(out$overall_stability, 2, tolerance = 1e-8)

  # It is dimensionless, so rescaling time changes both rates but not OSt.
  scaled <- make_rate_cycle()
  scaled$time <- scaled$time * 7
  expect_equal(overall_stability(scaled)$overall_stability, 2, tolerance = 1e-8)
})

test_that("OSt rises with resistance and with recovery speed", {
  reference <- overall_stability(make_rate_cycle())$overall_stability

  # More resistant: the same trough reached more slowly, so DR falls.
  resistant <- overall_stability(make_rate_cycle(decline_slope = -0.5))
  expect_lt(resistant$disturbance_rate, 1)
  expect_gt(resistant$overall_stability, reference)

  # More resilient: the same trough left more quickly, so RR rises.
  resilient <- overall_stability(make_rate_cycle(recovery_slope = 4))
  expect_gt(resilient$recovery_rate, 2)
  expect_gt(resilient$overall_stability, reference)
})

test_that("impact is the depth of the trough and is reported as a percentage", {
  out <- overall_stability(make_rate_cycle())
  expect_equal(out$impact, 100 * (10 - 2) / 10, tolerance = 1e-8)

  frac <- overall_stability(make_rate_cycle(), percent = FALSE)
  expect_equal(frac$impact, 0.8, tolerance = 1e-8)
  expect_equal(frac$integrated_impact, out$integrated_impact / 100,
               tolerance = 1e-8)
  # The rates are not percentages, so they must be unaffected by the switch.
  expect_equal(frac$disturbance_rate, out$disturbance_rate)
})

test_that("integrated impact and perturbation are time-averaged relative losses", {
  cyc <- make_rate_cycle()
  out <- overall_stability(cyc)

  manual_window <- function(from, to) {
    keep <- cyc$time >= from & cyc$time <= to
    x <- cyc$time[keep]
    y <- cyc$transformed_value[keep]
    d <- pmax(0, 10 - y)
    area <- sum(diff(x) * (utils::head(d, -1) + utils::tail(d, -1)) / 2)
    100 * area / ((to - from) * 10)
  }

  t_min <- cyc$time[which.min(cyc$transformed_value)]
  expect_equal(out$integrated_impact, manual_window(3, t_min), tolerance = 1e-8)
  expect_equal(out$perturbation, manual_window(t_min, max(cyc$time)),
               tolerance = 1e-8)

  # All three magnitude indices are bounded by the depth of the trough.
  expect_lte(out$integrated_impact, out$impact)
  expect_lte(out$perturbation, out$impact)
})

test_that("a window with too few points yields NA instead of a chord", {
  cyc <- make_rate_cycle()
  # Keep only two observations on the way down.
  thin <- cyc[cyc$phase != "Drought" | cyc$time %in% c(4, 11), ]

  out <- overall_stability(thin, min_points = 3)
  expect_true(is.na(out$disturbance_rate))
  expect_true(is.na(out$overall_stability))
  # The recovery window is untouched and still estimable.
  expect_true(is.finite(out$recovery_rate))
})

test_that("a non-linear return is flagged by a low recovery R squared", {
  cyc <- make_rate_cycle()
  # A plateau after a fast start: still recovering, but not at a constant rate.
  rec <- cyc$phase == "Rewatering"
  cyc$transformed_value[rec] <- c(8, 9.2, 9.6, 9.8)

  out <- overall_stability(cyc)
  expect_true(is.finite(out$recovery_rate))
  expect_lt(out$recovery_r_squared, 0.95)
})

test_that("overall_stability runs on the bundled data and shares the geometry", {
  data(rehydra_data, package = "rehydra")

  prepared <- suppressWarnings(prepare_rehydra_data(
    data = rehydra_data, time = time_days, genotype = genotype,
    treatment = treatment, replicate = replicate,
    variables = stomatal_conductance
  ))
  segments <- segment_drought_cycle(prepared, time = time, value = transformed_value)

  st <- overall_stability(segments)
  cs <- curve_shape_metrics(segments)

  expect_equal(nrow(st), nrow(cs))
  # The baseline and the trough come from the same shared routine.
  expect_equal(st$baseline, cs$baseline)
  expect_equal(st$drought_minimum, cs$drought_minimum)

  # `impact` is the percentage form of the trough depth used elsewhere.
  expect_equal(st$impact / 100, mean_reduction_metrics(segments)$max_reduction,
               tolerance = 1e-8)
})

test_that("OSt feeds memory_trend and detects the simulated memory", {
  data(rehydra_data, package = "rehydra")

  prepared <- suppressWarnings(prepare_rehydra_data(
    data = rehydra_data, time = time_days, genotype = genotype,
    treatment = treatment, replicate = replicate,
    variables = stomatal_conductance
  ))
  segments <- segment_drought_cycle(prepared, time = time, value = transformed_value)
  st <- overall_stability(segments[segments$treatment == "Drought", ])

  trend <- memory_trend(st, metrics = "overall_stability", min_cycles = 2)

  # The bundled dataset was simulated so that the tolerant genotype copes better
  # in the second cycle; a rising OSt is exactly that signature.
  tolerant <- trend[trend$genotype == "Tolerant", ]
  expect_gt(tolerant$slope, 0)
  expect_identical(tolerant$memory_class, "acquired_memory")
})

test_that("overall_stability is included in rehydra_indices", {
  data(rehydra_data, package = "rehydra")

  prepared <- suppressWarnings(prepare_rehydra_data(
    data = rehydra_data, time = time_days, genotype = genotype,
    treatment = treatment, replicate = replicate,
    variables = stomatal_conductance
  ))
  segments <- segment_drought_cycle(prepared, time = time, value = transformed_value)

  idx <- rehydra_indices(segments, families = c("resilience", "stability"))
  expect_true("overall_stability" %in% names(idx))
  expect_true("disturbance_rate" %in% names(idx))
  expect_equal(idx$overall_stability, overall_stability(segments)$overall_stability,
               tolerance = 1e-10)
})

test_that("degenerate input returns a one-row all-NA result", {
  tiny <- tibble::tibble(
    genotype = "G1", treatment = "Drought", replicate = "1", plant_id = "p1",
    variable = "v", cycle = 1L, time = c(1, 2), transformed_value = c(1, NA),
    phase = c("PreDrought", "Drought")
  )

  out <- overall_stability(tiny)
  expect_equal(nrow(out), 1L)
  expect_true(is.na(out$overall_stability))
})
