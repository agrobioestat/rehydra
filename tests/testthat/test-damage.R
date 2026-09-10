test_that("damage_metrics computes core drought-damage metrics", {
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

  dmg <- damage_metrics(seg)
  expect_true(all(c("drought_drop_absolute", "drought_drop_relative", "damage_rate", "damage_auc", "time_to_damage") %in% names(dmg)))

  r1 <- dmg[1, ]
  expect_equal(r1$drought_drop_absolute, r1$PreDrought - r1$drought_minimum, tolerance = 1e-8)
  expect_equal(r1$drought_drop_relative, (r1$PreDrought - r1$drought_minimum) / r1$PreDrought, tolerance = 1e-8)

  if (is.finite(r1$duration_drought) && r1$duration_drought > 0) {
    expect_equal(r1$damage_rate, (r1$drought_minimum - r1$PreDrought) / r1$duration_drought, tolerance = 1e-8)
  }

  expect_true(any(is.finite(dmg$damage_auc)))
  expect_true(any(is.finite(dmg$time_to_damage)))
})
