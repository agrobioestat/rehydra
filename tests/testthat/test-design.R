make_design_indices <- function(effect = 0.3, n_rep = 4, sd = 0.02) {
  set.seed(7)
  grid <- expand.grid(
    genotype = c("Tolerant", "Sensitive"),
    treatment = c("Control", "Drought"),
    replicate = as.character(seq_len(n_rep)),
    stringsAsFactors = FALSE
  )

  tibble::tibble(
    genotype = grid$genotype,
    treatment = grid$treatment,
    replicate = grid$replicate,
    variable = "v",
    cycle = 1L,
    Rs = 1 - effect * (grid$treatment == "Drought") +
      stats::rnorm(nrow(grid), sd = sd)
  )
}

test_that("rehydra_anova reproduces the ANOVA of the equivalent aov call", {
  idx <- make_design_indices()
  fit <- rehydra_anova(idx, response = "Rs", by = "variable")

  reference <- stats::anova(stats::aov(
    Rs ~ factor(genotype) * factor(treatment),
    data = as.data.frame(idx)
  ))

  expect_s3_class(fit, "rehydra_anova")
  expect_equal(fit$anova$df, as.numeric(reference[["Df"]]))
  expect_equal(fit$anova$sum_sq, as.numeric(reference[["Sum Sq"]]),
               tolerance = 1e-8)
  expect_equal(fit$anova$statistic, as.numeric(reference[["F value"]]),
               tolerance = 1e-8)
})

test_that("the treatment effect is detected and the null effect is not", {
  strong <- rehydra_anova(make_design_indices(effect = 0.3), response = "Rs",
                          by = "variable")
  null <- rehydra_anova(make_design_indices(effect = 0), response = "Rs",
                        by = "variable")

  p_strong <- strong$anova$p_value[strong$anova$term == "treatment"]
  p_null <- null$anova$p_value[null$anova$term == "treatment"]

  expect_lt(p_strong, 0.001)
  expect_gt(p_null, 0.05)
})

test_that("marginal means use the pooled residual error, not the cell SD", {
  idx <- make_design_indices()
  fit <- rehydra_anova(idx, response = "Rs", by = "variable")

  mse <- fit$diagnostics$residual_se^2
  n <- fit$means$n

  expect_equal(fit$means$std_error, sqrt(mse / n), tolerance = 1e-8)
  expect_true(all(fit$means$conf_low < fit$means$estimate))
  expect_true(all(fit$means$conf_high > fit$means$estimate))
})

test_that("pairwise comparisons cover every pair and are Tukey-adjusted", {
  idx <- make_design_indices()
  fit <- rehydra_anova(idx, response = "Rs", by = "variable")

  n_cells <- fit$diagnostics$n_cells
  expect_equal(nrow(fit$comparisons), choose(n_cells, 2))
  expect_true(all(fit$comparisons$p_value >= 0 & fit$comparisons$p_value <= 1))
  # Tukey p-values are already family-wise, and are then adjusted across the
  # subsets, so they can only grow.
  expect_true(all(fit$comparisons$p_value_adj >= fit$comparisons$p_value - 1e-12))
})

test_that("one model is fitted per subset of `by`", {
  idx <- rbind(
    make_design_indices(),
    transform(make_design_indices(), cycle = 2L)
  )

  fit <- rehydra_anova(idx, response = "Rs", by = c("variable", "cycle"))
  expect_equal(length(fit$models), 2L)
  expect_setequal(unique(fit$anova$cycle), c(1L, 2L))
})

test_that("a blocked design adds the block term", {
  idx <- make_design_indices()
  idx$block <- rep(c("B1", "B2"), length.out = nrow(idx))

  fit <- rehydra_anova(idx, response = "Rs", block = "block", by = "variable")
  expect_true("block" %in% fit$anova$term)
})

test_that("a single-level factor is dropped instead of aborting the fit", {
  idx <- make_design_indices()
  idx$treatment <- "Drought"

  fit <- rehydra_anova(idx, response = "Rs", by = "variable")
  expect_true("treatment" %in% fit$parameters$dropped_factors)
  expect_true("genotype" %in% fit$anova$term)
})

test_that("invalid arguments give informative errors", {
  idx <- make_design_indices()
  expect_error(rehydra_anova(idx, response = "missing"), "was not found")
  expect_error(rehydra_anova(idx, response = "genotype"), "must be numeric")
  expect_error(rehydra_anova(idx, factors = "nope"), "None of the")
})

test_that("S3 methods are registered for rehydra_anova", {
  fit <- rehydra_anova(make_design_indices(), response = "Rs", by = "variable")

  expect_output(print(fit), "rehydra_anova")
  expect_s3_class(summary(fit), "tbl_df")
  expect_identical(generics::tidy(fit), fit$anova)
})

# ---- mixed model / repeated measures ----------------------------------------

make_repeated_indices <- function(n_rep = 4, plant_sd = 0.05, sd = 0.02) {
  set.seed(11)
  plants <- expand.grid(
    genotype = c("Tolerant", "Sensitive"),
    treatment = c("Control", "Drought"),
    replicate = as.character(seq_len(n_rep)),
    stringsAsFactors = FALSE
  )
  plants$plant_id <- paste(plants$genotype, plants$treatment, plants$replicate,
                           sep = "_")
  # A per-plant offset is what makes the two cycles of one plant correlated.
  plants$offset <- stats::rnorm(nrow(plants), sd = plant_sd)

  grid <- merge(plants, data.frame(cycle = c(1L, 2L)), by = NULL)
  grid$Rs <- 1 - 0.3 * (grid$treatment == "Drought") -
    0.05 * (grid$cycle == 2L) + grid$offset +
    stats::rnorm(nrow(grid), sd = sd)
  grid$variable <- "v"
  tibble::as_tibble(grid)
}

test_that("a random effect drives every reported table, not just the stored model", {
  skip_if_not_installed("lme4")

  idx <- make_repeated_indices()
  fit <- rehydra_anova(idx, response = "Rs",
                       factors = c("genotype", "treatment", "cycle"),
                       by = "variable", random = "plant_id")

  expect_identical(unique(fit$diagnostics$model_type), "mixed")
  expect_true(fit$diagnostics$mixed_model)

  # The bug this guards against: reporting the fixed-effects ANOVA while a mixed
  # model sat unused. The mixed model spends degrees of freedom on the random
  # intercept, so its residual df must be strictly smaller than the aov's.
  aov_df <- stats::df.residual(stats::aov(
    Rs ~ factor(genotype) * factor(treatment) * factor(cycle),
    data = as.data.frame(idx)
  ))
  expect_lt(fit$diagnostics$df_residual, aov_df)

  # And the residual scale must be the mixed model's sigma, not the aov's.
  expect_false(isTRUE(all.equal(
    fit$diagnostics$residual_se,
    summary(stats::aov(Rs ~ factor(genotype) * factor(treatment) * factor(cycle),
                       data = as.data.frame(idx)))[[1]][["Mean Sq"]][4]
  )))
})

test_that("the mixed ANOVA matches lmerTest exactly", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("lmerTest")

  idx <- make_repeated_indices()
  fit <- rehydra_anova(idx, response = "Rs",
                       factors = c("genotype", "treatment", "cycle"),
                       by = "variable", random = "plant_id")

  d <- as.data.frame(idx)
  d$genotype <- factor(d$genotype)
  d$treatment <- factor(d$treatment)
  d$cycle <- factor(d$cycle)
  ref <- as.data.frame(stats::anova(
    lmerTest::lmer(Rs ~ genotype * treatment * cycle + (1 | plant_id), data = d)
  ))

  expect_equal(fit$anova$statistic, as.numeric(ref[["F value"]]), tolerance = 1e-8)
  expect_equal(fit$anova$p_value, as.numeric(ref[["Pr(>F)"]]), tolerance = 1e-8)
  expect_identical(unique(fit$diagnostics$p_value_method), "satterthwaite")
})

test_that("mixed marginal means are model based and reproduce the fixed effects", {
  skip_if_not_installed("lme4")

  idx <- make_repeated_indices()
  fit <- rehydra_anova(idx, response = "Rs",
                       factors = c("genotype", "treatment", "cycle"),
                       by = "variable", random = "plant_id")

  expect_equal(nrow(fit$means), 8L)
  expect_true(all(is.finite(fit$means$std_error)))
  expect_true(all(fit$means$std_error > 0))
  expect_true(all(fit$means$conf_low < fit$means$estimate))

  # With a balanced design and a random intercept only, the model-based cell
  # means coincide with the raw cell means; the standard errors do not.
  raw <- tapply(idx$Rs, interaction(idx$genotype, idx$treatment, idx$cycle,
                                    drop = TRUE, sep = " : "), mean)
  expect_equal(sort(unname(fit$means$estimate)), sort(unname(as.numeric(raw))),
               tolerance = 1e-6)

  expect_equal(nrow(fit$comparisons), choose(8, 2))
  expect_true(all(fit$comparisons$std_error > 0))
})

test_that("the fixed-effects path is untouched when no random effect is given", {
  idx <- make_design_indices()
  fit <- rehydra_anova(idx, response = "Rs", by = "variable")

  expect_identical(unique(fit$diagnostics$model_type), "fixed")
  expect_identical(unique(fit$diagnostics$p_value_method), "f-test")
  expect_false(fit$diagnostics$mixed_model)
})
