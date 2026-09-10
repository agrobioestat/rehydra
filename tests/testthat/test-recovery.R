test_that("recovery_metrics computes recovery indicators and overcompensation", {
  data(rehydra_data, package = "rehydra")

  prepared <- prepare_rehydra_data(
    data = rehydra_data,
    time = time_days,
    genotype = genotype,
    treatment = treatment,
    replicate = replicate,
    variables = c(stomatal_conductance)
  )

  seg <- segment_drought_cycle(
    data = prepared,
    time = time,
    value = transformed_value,
    group_by = c("genotype", "treatment", "replicate", "variable", "plant_id"),
    method = "auto"
  )

  rec <- recovery_metrics(seg)
  expect_true(all(c("recovery_rate", "recovery_completeness", "residual_cost", "time_to_recovery", "overcompensation") %in% names(rec)))

  r1 <- rec[1, ]
  if (is.finite(r1$duration_recovery) && r1$duration_recovery > 0) {
    expect_equal(
      r1$recovery_rate,
      (r1$post_rewatering_value - r1$drought_minimum) / r1$duration_recovery,
      tolerance = 1e-8
    )
  }

  expect_equal(r1$recovery_completeness, r1$post_rewatering_value / r1$PreDrought, tolerance = 1e-8)
  expect_equal(r1$residual_cost, 1 - (r1$post_rewatering_value / r1$PreDrought), tolerance = 1e-8)
  expect_true(is.logical(rec$overcompensation))
})

test_that("recovery_metrics applies overcompensation capping consistently", {
  synthetic <- tibble::tibble(
    genotype = "Tolerant",
    treatment = "Drought",
    replicate = "1",
    plant_id = "T_D_1",
    variable = "stomatal_conductance",
    cycle = 1L,
    phase = c("PreDrought", "PreDrought", "Drought", "Drought", "Rewatering", "PostDrought"),
    time = c(0, 1, 2, 3, 4, 5),
    transformed_value = c(1.0, 1.0, 0.4, 0.35, 1.2, 1.3)
  )

  rec_free <- recovery_metrics(synthetic, allow_overcompensation = TRUE)
  rec_cap <- recovery_metrics(synthetic, allow_overcompensation = FALSE)

  expect_true(rec_free$overcompensation[[1]])
  expect_gt(rec_free$post_rewatering_value[[1]], rec_free$PreDrought[[1]])

  expect_equal(rec_cap$post_rewatering_value[[1]], rec_cap$PreDrought[[1]], tolerance = 1e-8)
  expect_equal(rec_cap$recovery_completeness[[1]], 1, tolerance = 1e-8)
  expect_equal(rec_cap$residual_cost[[1]], 0, tolerance = 1e-8)
  expect_equal(
    rec_cap$recovery_rate[[1]],
    (rec_cap$post_rewatering_value[[1]] - rec_cap$drought_minimum[[1]]) / rec_cap$duration_recovery[[1]],
    tolerance = 1e-8
  )
})
