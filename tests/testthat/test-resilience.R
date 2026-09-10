test_that("resilience_index follows Rt, Rc, and Rs formulas", {
  data(rehydra_data, package = "rehydra")

  prepared <- prepare_rehydra_data(
    data = rehydra_data,
    time = time_days,
    genotype = genotype,
    treatment = treatment,
    replicate = replicate,
    variables = c(stomatal_conductance)
  )

  seg <- segment_drought_cycle(prepared, time = time, value = transformed_value, method = "auto")
  res <- resilience_index(seg)

  expect_true(all(c("Rt", "Rc", "Rs", "PreDrought", "Drought", "PostDrought") %in% names(res)))

  r1 <- res[1, ]
  expect_equal(r1$Rt, r1$Drought / r1$PreDrought, tolerance = 1e-8)
  expect_equal(r1$Rc, r1$PostDrought / r1$Drought, tolerance = 1e-8)
  expect_equal(r1$Rs, r1$PostDrought / r1$PreDrought, tolerance = 1e-8)
})

test_that("resilience_index handles negative-valued traits after direction-aware transforms", {
  data(rehydra_data, package = "rehydra")

  prepared <- prepare_rehydra_data(
    data = rehydra_data,
    time = time_days,
    genotype = genotype,
    treatment = treatment,
    replicate = replicate,
    variables = c(water_potential),
    response_direction = "auto",
    transform = "none"
  )

  expect_true(all(prepared$transformed_value[is.finite(prepared$transformed_value)] > 0))
  wp_vals <- prepared$transformed_value[prepared$variable == "water_potential" & is.finite(prepared$transformed_value)]
  expect_true(length(wp_vals) > 0)
  expect_true(min(wp_vals) >= 0.01)

  seg <- segment_drought_cycle(prepared, time = time, value = transformed_value, method = "auto")
  res <- resilience_index(seg, ci_method = "bootstrap", n_boot = 99)

  expect_true(all(c("conf_low_Rt", "conf_high_Rt", "conf_low_Rc", "conf_high_Rc", "conf_low_Rs", "conf_high_Rs") %in% names(res)))
  expect_true(any(is.finite(res$Rt)))
  expect_true(any(is.finite(res$Rc)))
  expect_true(any(is.finite(res$Rs)))
  expect_lt(max(res$Rc, na.rm = TRUE), 200)
})
