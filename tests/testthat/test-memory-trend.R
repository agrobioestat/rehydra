make_trend_indices <- function(slope = 0.05, n_cycles = 4, n_rep = 4, sd = 0.005) {
  set.seed(42)
  grid <- expand.grid(
    cycle = seq_len(n_cycles),
    replicate = as.character(seq_len(n_rep)),
    stringsAsFactors = FALSE
  )

  tibble::tibble(
    genotype = "G1",
    treatment = "Drought",
    variable = "v",
    replicate = grid$replicate,
    cycle = as.integer(grid$cycle),
    Rs = 0.8 + slope * (grid$cycle - 1) + stats::rnorm(nrow(grid), sd = sd),
    Rt = 0.7,
    Rc = 1.1
  )
}

test_that("memory_trend recovers a known per-cycle slope", {
  idx <- make_trend_indices(slope = 0.05)
  out <- memory_trend(idx, metrics = "Rs")

  expect_equal(nrow(out), 1L)
  # Simulated with noise, so the estimate is close to but not exactly 0.05.
  expect_equal(out$slope, 0.05, tolerance = 0.1)
  expect_equal(out$n_cycles, 4L)
  # The regression uses the replicate rows, not the four cycle means.
  expect_equal(out$n_obs, 16L)
  expect_lt(out$p_value, 0.05)
  expect_equal(out$monotonic_fraction, 1)
  expect_identical(out$memory_class, "acquired_memory")
})

test_that("cumulative_change equals last minus first cycle mean", {
  idx <- make_trend_indices(slope = 0.05)
  out <- memory_trend(idx, metrics = "Rs")

  means <- tapply(idx$Rs, idx$cycle, mean)
  expect_equal(out$cumulative_change,
               as.numeric(means[["4"]] - means[["1"]]), tolerance = 1e-8)
  expect_equal(out$first_value, as.numeric(means[["1"]]), tolerance = 1e-8)
  expect_equal(out$last_value, as.numeric(means[["4"]]), tolerance = 1e-8)
})

test_that("a flat sequence is classified as stable", {
  idx <- make_trend_indices(slope = 0, sd = 0.02)
  out <- memory_trend(idx, metrics = "Rs")

  expect_gt(out$p_value, 0.05)
  expect_identical(out$memory_class, "stable")
})

test_that("direction is flipped for damage-type metrics", {
  expect_false(memory_direction("Rs"))
  expect_true(memory_direction("deficit_auc"))
  expect_true(memory_direction("t50"))
  expect_true(memory_direction("total_reduction"))
  expect_true(memory_direction("mean_reduction"))
  expect_false(memory_direction("mean_recovery_rate"))

  # A falling damage index means the plant coped better in later cycles, so it
  # must be labelled as acquired memory, not eroded.
  idx <- make_trend_indices(slope = -0.05)
  names(idx)[names(idx) == "Rs"] <- "deficit_auc"
  out <- memory_trend(idx, metrics = "deficit_auc")

  expect_true(out$lower_is_better)
  expect_lt(out$slope, 0)
  expect_identical(out$memory_class, "acquired_memory")
})

test_that("too few cycles yields undetermined rather than a spurious fit", {
  idx <- make_trend_indices(slope = 0.05, n_cycles = 2)
  out <- memory_trend(idx, metrics = "Rs", min_cycles = 3)

  expect_identical(out$memory_class, "undetermined")
  expect_true(is.na(out$slope))
})

test_that("monotonic_fraction exposes an endpoint-driven slope", {
  # Cycles 1-3 flat, cycle 4 jumps: the slope is significant but only one of the
  # three steps moves with it.
  idx <- make_trend_indices(slope = 0, sd = 0.001)
  idx$Rs[idx$cycle == 4] <- idx$Rs[idx$cycle == 4] + 0.5

  out <- memory_trend(idx, metrics = "Rs")
  expect_lt(out$p_value, 0.05)
  expect_lt(out$monotonic_fraction, 0.7)
})

test_that("p-values are adjusted across metrics within a group", {
  idx <- make_trend_indices(slope = 0.05)
  out <- memory_trend(idx, metrics = c("Rs", "Rt", "Rc"))

  expect_equal(nrow(out), 3L)
  expect_true(all(out$p_value_adj >= out$p_value | is.na(out$p_value)))
})

test_that("memory_trend requires a cycle column and a numeric metric", {
  expect_error(memory_trend(tibble::tibble(Rs = 1:3)), "cycle")
  expect_error(
    memory_trend(tibble::tibble(cycle = 1:3, Rs = letters[1:3])),
    "numeric metric"
  )
})
