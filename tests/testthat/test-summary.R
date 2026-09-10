test_that("prepare_rehydra_data accepts wide and long formats", {
  data(rehydra_data, package = "rehydra")

  wide <- prepare_rehydra_data(
    data = rehydra_data,
    time = time_days,
    genotype = genotype,
    treatment = treatment,
    replicate = replicate,
    variables = c(water_potential, stomatal_conductance)
  )

  expect_true(all(c("plant_id", "genotype", "treatment", "replicate", "time", "variable", "value", "transformed_value") %in% names(wide)))

  vars <- c("water_potential", "stomatal_conductance")
  wide_chr <- prepare_rehydra_data(
    data = rehydra_data,
    time = time_days,
    genotype = genotype,
    treatment = treatment,
    replicate = replicate,
    variables = vars
  )
  expect_equal(sort(unique(wide_chr$variable)), sort(vars))

  long_input <- rehydra_data |>
    tidyr::pivot_longer(
      cols = c(water_potential, stomatal_conductance),
      names_to = "variable",
      values_to = "value"
    )

  long <- prepare_rehydra_data(
    data = long_input,
    time = time_days,
    genotype = genotype,
    treatment = treatment,
    replicate = replicate,
    variable = variable,
    value = value,
    cycle = cycle,
    phase = phase
  )

  expect_true(all(c("cycle", "phase") %in% names(long)))
  expect_equal(dplyr::n_distinct(wide$genotype), dplyr::n_distinct(rehydra_data$genotype))
})

test_that("prepare_rehydra_data accepts programmatic string mappings", {
  data(rehydra_data, package = "rehydra")

  time_nm <- "time_days"
  genotype_nm <- "genotype"
  treatment_nm <- "treatment"
  replicate_nm <- "replicate"
  vars_nm <- c("water_potential", "stomatal_conductance")

  out <- prepare_rehydra_data(
    data = rehydra_data,
    time = time_nm,
    genotype = genotype_nm,
    treatment = treatment_nm,
    replicate = replicate_nm,
    variables = vars_nm
  )

  expect_s3_class(out, "tbl_df")
  expect_true(nrow(out) > 0)
  expect_setequal(unique(out$variable), vars_nm)
})

test_that("summarize_rehydra returns a complete rehydra_analysis object", {
  data(rehydra_data, package = "rehydra")

  fit <- summarize_rehydra(
    data = rehydra_data,
    time = time_days,
    genotype = genotype,
    treatment = treatment,
    replicate = replicate,
    variables = c(water_potential, stomatal_conductance),
    segmentation_method = "auto"
  )

  expect_s3_class(fit, "rehydra_analysis")

  expected <- c(
    "data", "checks", "segments", "damage", "recovery", "resilience",
    "memory", "stability", "classification", "genotype_comparison", "parameters", "references"
  )

  expect_true(all(expected %in% names(fit)))
})
