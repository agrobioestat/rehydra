test_that("plot functions return ggplot objects", {
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
  dmg <- damage_metrics(seg)
  rec <- recovery_metrics(seg)
  mem <- memory_effect(res, dmg, rec)
  st <- stability_index(seg)

  p1 <- plot_recovery_trajectory(seg, resilience_results = res)
  p2 <- plot_memory_effect(mem)
  p3 <- plot_resilience_indices(res)
  p4 <- plot_damage_recovery(dmg, rec)
  p5 <- plot_stability(st)

  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
  expect_s3_class(p3, "ggplot")
  expect_s3_class(p4, "ggplot")
  expect_s3_class(p5, "ggplot")
})

test_that("plot_recovery_trajectory handles missing resilience columns", {
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
  res_bad <- tibble::tibble(genotype = "Sensitive")

  p <- plot_recovery_trajectory(seg, resilience_results = res_bad, annotate_indices = TRUE)
  expect_s3_class(p, "ggplot")
})
