test_that("segment_drought_cycle works in manual and automatic modes", {
  data(rehydra_data, package = "rehydra")

  prepared <- prepare_rehydra_data(
    data = rehydra_data,
    time = time_days,
    genotype = genotype,
    treatment = treatment,
    replicate = replicate,
    variables = c(water_potential, stomatal_conductance)
  )

  seg_auto <- segment_drought_cycle(
    data = prepared,
    time = time,
    value = transformed_value,
    group_by = c("genotype", "treatment", "replicate", "variable"),
    method = "auto"
  )

  expect_true(all(c("cycle", "phase", "method") %in% names(seg_auto)))
  expect_true(all(c("PreDrought", "Drought", "Rewatering", "PostDrought") %in% unique(seg_auto$phase)))
  expect_true(all(seg_auto$confidence[is.finite(seg_auto$confidence)] >= 0 & seg_auto$confidence[is.finite(seg_auto$confidence)] <= 1))

  drought_cycles <- seg_auto |>
    dplyr::filter(treatment == "Drought") |>
    dplyr::pull(cycle) |>
    unique()
  expect_true(length(drought_cycles) >= 2)

  events <- tibble::tibble(
    cycle = c(1, 1, 1, 1, 2, 2, 2, 2),
    phase = c("PreDrought", "Drought", "Rewatering", "PostDrought", "PreDrought", "Drought", "Rewatering", "PostDrought"),
    start = c(0, 5, 11, 16, 21, 25, 31, 36),
    end = c(4, 10, 15, 20, 24, 30, 35, 40)
  )

  seg_manual <- segment_drought_cycle(
    data = prepared,
    time = time,
    value = transformed_value,
    group_by = c("genotype", "treatment", "replicate", "variable"),
    method = "manual",
    events = events
  )

  expect_true(all(seg_manual$method == "manual_events_interval"))
})

test_that("segment_drought_cycle handles missing values and fallback mode", {
  data(rehydra_data, package = "rehydra")

  prepared <- prepare_rehydra_data(
    data = rehydra_data,
    time = time_days,
    genotype = genotype,
    treatment = treatment,
    replicate = replicate,
    variables = c(water_potential, stomatal_conductance)
  )

  set.seed(123)
  prepared$value[sample(seq_len(nrow(prepared)), 30)] <- NA_real_

  seg <- segment_drought_cycle(
    data = prepared,
    time = time,
    value = transformed_value,
    method = "auto",
    fallback_manual = TRUE
  )

  expect_s3_class(seg, "tbl_df")
  expect_true(nrow(seg) > 0)

  short <- prepared |>
    dplyr::filter(time <= 2) |>
    dplyr::mutate(cycle = NA_integer_, phase = NA_character_)

  expect_warning(
    seg_short <- segment_drought_cycle(
      data = short,
      time = time,
      value = transformed_value,
      method = "auto",
      fallback_manual = TRUE
    )
  )
  expect_true(any(grepl("fallback", seg_short$method)))

  expect_error(
    segment_drought_cycle(
      data = short,
      time = time,
      value = transformed_value,
      method = "auto",
      fallback_manual = FALSE
    ),
    "Automatic segmentation could not confidently identify drought phases"
  )
})

test_that("segment_drought_cycle fails informatively for invalid time", {
  data(rehydra_data, package = "rehydra")

  bad <- rehydra_data
  bad$time_days <- paste0("day_", bad$time_days)

  prepared <- suppressWarnings(
    prepare_rehydra_data(
      data = bad,
      time = time_days,
      genotype = genotype,
      treatment = treatment,
      replicate = replicate,
      variables = c(water_potential, stomatal_conductance)
    )
  )

  expect_error(
    segment_drought_cycle(
      data = prepared,
      time = time,
      value = transformed_value,
      method = "auto"
    )
  )
})
