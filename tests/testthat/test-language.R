test_that("example dataset uses English-only schema and levels", {
  data(rehydra_data, package = "rehydra")

  expected_cols <- c(
    "plant_id", "genotype", "time_days", "treatment", "replicate",
    "cycle", "phase", "water_potential", "stomatal_conductance"
  )

  expect_true(all(expected_cols %in% names(rehydra_data)))
  expect_false(any(grepl("[^ -~]", names(rehydra_data))))

  expect_setequal(unique(rehydra_data$genotype), c("Sensitive", "Tolerant"))
  expect_setequal(unique(rehydra_data$treatment), c("Control", "Drought"))
  expect_true(all(unique(rehydra_data$phase) %in% c("PreDrought", "Drought", "Rewatering", "PostDrought")))
})

test_that("automatic segmentation preserves complete cycles on example data", {
  data(rehydra_data, package = "rehydra")

  prepared <- suppressWarnings(
    prepare_rehydra_data(
      data = rehydra_data,
      time = time_days,
      genotype = genotype,
      treatment = treatment,
      replicate = replicate,
      variables = c(water_potential, stomatal_conductance)
    )
  )

  seg <- segment_drought_cycle(
    data = prepared,
    time = time,
    value = transformed_value,
    group_by = c("genotype", "treatment", "replicate", "variable", "plant_id"),
    method = "auto"
  )

  drought_cycles <- seg |>
    dplyr::filter(.data$treatment == "Drought") |>
    dplyr::summarise(n = dplyr::n_distinct(.data$cycle)) |>
    dplyr::pull(.data$n)

  expect_equal(drought_cycles, 2)
  expect_true(all(c("PreDrought", "Drought", "Rewatering", "PostDrought") %in% unique(seg$phase)))
})
