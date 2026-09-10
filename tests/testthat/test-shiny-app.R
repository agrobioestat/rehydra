test_that("run_rehydra_app performs dependency checks", {
  fn_txt <- paste(deparse(rehydra::run_rehydra_app), collapse = "\n")
  expect_match(fn_txt, "requireNamespace\\(\\\"shiny\\\"")
})

test_that("bundled shiny app can be parsed without launching", {
  skip_if_not_installed("shiny")

  app_path <- system.file("shiny", "app.R", package = "rehydra")
  expect_true(nzchar(app_path))
  expect_true(file.exists(app_path))

  env <- new.env(parent = baseenv())
  env$rehydra <- asNamespace("rehydra")

  app_obj <- source(app_path, local = env)$value
  expect_s3_class(app_obj, "shiny.appobj")
})

test_that("the app exposes the new AUC, extended-index, memory and design tabs", {
  skip_if_not_installed("shiny")

  app_path <- system.file("shiny", "app.R", package = "rehydra")
  skip_if(!nzchar(app_path))

  src <- paste(readLines(app_path, warn = FALSE), collapse = "\n")

  for (tab in c("Deficit curve (AUC)", "Extended indices",
                "Multi-cycle memory", "Designed analysis")) {
    expect_true(grepl(tab, src, fixed = TRUE), label = tab)
  }

  # Each new panel needs both an output placeholder in the UI and a renderer in
  # the server; a mismatch shows up as a silently blank panel at run time.
  outputs <- c(
    "deficit_plot", "curve_shape_plot", "curve_shape_table",
    "extended_plot", "period_table", "reduction_table",
    "memory_trend_plot", "memory_trend_table", "memory_pairs_table",
    "anova_table", "anova_means", "anova_comparisons", "anova_diagnostics",
    "overall_stability_table"
  )

  for (out in outputs) {
    expect_true(grepl(paste0('"', out, '"'), src, fixed = TRUE), label = out)
    expect_true(grepl(paste0("output[$]", out, " <-"), src), label = out)
  }
})

test_that("every function the app calls is exported by the package", {
  skip_if_not_installed("shiny")

  app_path <- system.file("shiny", "app.R", package = "rehydra")
  skip_if(!nzchar(app_path))

  src <- paste(readLines(app_path, warn = FALSE), collapse = "\n")
  calls <- regmatches(src, gregexpr('rehydra_call[(][[:space:]]*"[^"]+"', src))[[1]]
  fns <- unique(gsub('^rehydra_call[(][[:space:]]*"|"$', "", calls))

  expect_gt(length(fns), 0)
  expect_setequal(setdiff(fns, getNamespaceExports("rehydra")), character())
})

test_that("the tab strip is grouped into sections instead of one flat row", {
  skip_if_not_installed("shiny")

  app_path <- system.file("shiny", "app.R", package = "rehydra")
  skip_if(!nzchar(app_path))

  src <- paste(readLines(app_path, warn = FALSE), collapse = "\n")

  # Thirteen panels in a single strip overflow the width of a normal screen, so
  # they are nested one level under six sections.
  for (section in c('"Data"', '"Dynamics"', '"Indices"', '"Memory"',
                    '"Statistics"', '"Export"')) {
    expect_true(grepl(section, src, fixed = TRUE), label = section)
  }
  expect_true(grepl('type = "pills"', src, fixed = TRUE))

  # Layout affordances added alongside the grouping.
  expect_true(grepl("rehydra-sidebar", src, fixed = TRUE))
  expect_true(grepl("show_controls", src, fixed = TRUE))
  expect_true(grepl("summary_boxes", src, fixed = TRUE))
  expect_true(grepl("Progress[$]new", src))
})

test_that("the long/wide mapping test never returns NA", {
  skip_if_not_installed("shiny")

  app_path <- system.file("shiny", "app.R", package = "rehydra")
  skip_if(!nzchar(app_path))

  env <- new.env(parent = globalenv())
  suppressWarnings(source(app_path, local = env))
  is_long <- get(".is_long_mapping", envir = env)

  cols <- c("time", "variable", "value")

  # The failure this guards: with the selectors still empty,
  # `NULL %in% cols` is logical(0) and the surrounding `if` errored out.
  expect_false(is_long(NULL, NULL, cols))
  expect_false(is_long("variable", NULL, cols))
  expect_false(is_long("__wide__", "__wide__", cols))
  expect_false(is_long("missing", "value", cols))
  expect_false(is_long(character(0), "value", cols))
  expect_true(is_long("variable", "value", cols))

  for (case in list(list(NULL, NULL), list("variable", NULL),
                    list(character(0), "value"))) {
    expect_length(is_long(case[[1]], case[[2]], cols), 1L)
  }
})

test_that("the app runs the full pipeline and serves every new panel", {
  skip_if_not_installed("shiny")
  skip_on_cran()

  app_dir <- system.file("shiny", package = "rehydra")
  skip_if(!nzchar(app_dir))

  shiny::testServer(app_dir, {
    session$setInputs(use_example = TRUE)
    session$setInputs(run_demo = 1)
    expect_gt(nrow(raw_data()), 0)

    session$setInputs(
      time_col = "time_days", genotype_col = "genotype",
      treatment_col = "treatment", replicate_col = "replicate",
      variable_col = "__wide__", value_col = "__wide__",
      physiological_vars = c("water_potential", "stomatal_conductance"),
      response_direction = "auto", transform_mode = "none",
      seg_method = "auto", smoothing = TRUE, smooth_span = 0.25,
      min_drop = 0.15, min_recovery = 0.10, set_cycles = FALSE
    )
    session$setInputs(run_analysis = 1)

    res <- analysis()
    expect_false(is.null(res))
    for (tbl in c("recovery_period", "mean_reduction", "curve_shape", "trend",
                  "shape_trend", "memory_pairs", "overall_stability")) {
      expect_true(tbl %in% names(res), label = tbl)
    }
    expect_gt(nrow(res$curve_shape), 0)

    session$setInputs(auc_variable = "stomatal_conductance",
                      auc_treatment = "Drought", auc_replicate = "1")
    expect_gt(nrow(auc_subset()), 0)

    session$setInputs(trend_family = "shape")
    expect_true("t50" %in% trend_selection()$metrics)
    session$setInputs(trend_family = "resilience")
    expect_true("Rs" %in% trend_selection()$metrics)

    session$setInputs(anova_source = "resilience", anova_response = "Rs",
                      anova_interaction = TRUE)
    fit <- anova_fit()
    expect_s3_class(fit, "rehydra_anova")
    expect_gt(nrow(fit$anova), 0)
  })
})
