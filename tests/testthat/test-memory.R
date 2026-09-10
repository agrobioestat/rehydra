test_that("memory_effect computes cycle differences correctly", {
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
  res <- resilience_index(seg, group_by = c("genotype", "treatment", "variable", "cycle"))
  dmg <- damage_metrics(seg, group_by = c("genotype", "treatment", "variable", "cycle"))
  rec <- recovery_metrics(seg, group_by = c("genotype", "treatment", "variable", "cycle"))

  mem <- memory_effect(
    resilience_results = res,
    damage_results = dmg,
    recovery_results = rec,
    cycle_ref = 1,
    cycle_test = 2,
    group_by = c("genotype", "treatment", "variable")
  )

  expect_true(all(c("Mem_Rt", "Mem_Rc", "Mem_Rs", "Mem_Damage", "Mem_RecRate", "Mem_Residual") %in% names(mem)))

  m1 <- mem[1, ]
  expect_equal(m1$Mem_Rt, m1$Rt_test - m1$Rt_ref, tolerance = 1e-8)
  expect_equal(m1$Mem_Rc, m1$Rc_test - m1$Rc_ref, tolerance = 1e-8)
  expect_equal(m1$Mem_Rs, m1$Rs_test - m1$Rs_ref, tolerance = 1e-8)
})

test_that("memory_effect bootstrap returns interval columns", {
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
  mem <- memory_effect(
    resilience_results = res,
    cycle_ref = 1,
    cycle_test = 2,
    group_by = c("genotype", "treatment", "variable"),
    ci_method = "bootstrap",
    n_boot = 99
  )

  expect_true(all(c("conf_low_Rt", "conf_high_Rt", "conf_low_Rc", "conf_high_Rc", "conf_low_Rs", "conf_high_Rs") %in% names(mem)))
})

test_that("priming_classification assigns expected categories", {
  mock <- tibble::tibble(
    Mem_Rt = c(0.10, -0.10, 0.01, 0.02, 0.00, -0.01),
    Mem_Rc = c(0.05, -0.08, 0.00, 0.00, 0.00, -0.01),
    Rs_test = c(0.95, 0.92, 1.00, 1.10, 0.80, 0.88)
  )

  out <- rehydra::priming_classification(mock, threshold = 0.05, recovery_threshold = 0.90)

  expect_equal(out$priming_class[[1]], "positive_priming")
  expect_equal(out$priming_class[[2]], "negative_priming")
  expect_equal(out$priming_class[[3]], "neutral_memory")
  expect_equal(out$priming_class[[4]], "overcompensation")
  expect_true(any(out$priming_class == "incomplete_recovery"))
})
