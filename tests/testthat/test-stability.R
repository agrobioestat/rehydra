test_that("stability_index returns normalized values and memory gain", {
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
  st <- stability_index(seg)

  expect_true("normalized_stability" %in% names(st))
  vals <- st$normalized_stability
  vals <- vals[is.finite(vals)]
  expect_true(all(vals >= 0 & vals <= 1))
  expect_true("memory_stability_gain" %in% names(st))
})
