#' Prepare Data for rehydra Analysis
#'
#' Standardize plant drought-rewatering data into the internal long format used
#' by `rehydra`.
#'
#' @param data A data frame or tibble.
#' @param time Time column (numeric, `Date`, or `POSIXct`).
#' @param genotype Genotype column.
#' @param treatment Treatment column, usually including `"Control"` and
#'   `"Drought"`.
#' @param replicate Replicate column.
#' @param variables Wide-format variable columns to pivot into
#'   `variable` and `value`.
#' @param plant_id Optional plant identifier column.
#' @param block Optional block column.
#' @param plot_id Optional plot identifier column.
#' @param pot_id Optional pot identifier column.
#' @param cycle Optional cycle column.
#' @param phase Optional phase column.
#' @param variable Optional variable-name column if `data` is already long.
#' @param value Optional value column if `data` is already long.
#' @param response_direction One of `"higher_is_better"`,
#'   `"lower_is_better"`, `"more_negative_is_worse"`, or `"auto"`.
#' @param transform One of `"none"`, `"absolute"`, `"invert"`,
#'   `"stress_score"`, or `"relative_to_control"`.
#' @param control_label Label used for the control treatment when
#'   `transform = "relative_to_control"`.
#'
#' @details
#' Input can be wide or long:
#'
#' - Wide: pass columns in `variables = c(var1, var2, ...)`.
#' - Long: pass `variable = ...` and `value = ...`.
#'
#' `response_direction` and `transform` are applied before downstream
#' calculations. Rt, Rc, and Rs are directly interpretable when
#' `transformed_value` increases with better physiological performance.
#' For variables such as water potential, use direction-aware transformation
#' when needed. Internally, transformed values are shifted to a positive
#' performance scale to stabilize ratio-based indices.
#'
#' @return A standardized tibble with columns:
#' `plant_id`, `genotype`, `treatment`, `replicate`, `block`, `plot_id`,
#' `pot_id`, `time`, `cycle`, `phase`, `variable`, `value`,
#' `response_direction`, and `transformed_value`.
#'
#' @references
#' Lloret et al. (2011) <doi:10.1111/j.1600-0706.2011.19372.x>;
#' Xu et al. (2010) <doi:10.4161/psb.5.6.11398>;
#' Ribeiro et al. (2021) <doi:10.1016/j.jplph.2021.153397>.
#'
#' @examples
#' data(rehydra_data)
#'
#' prepared <- prepare_rehydra_data(
#'   data = rehydra_data,
#'   time = time_days,
#'   genotype = genotype,
#'   treatment = treatment,
#'   replicate = replicate,
#'   variables = c(water_potential, stomatal_conductance)
#' )
#'
#' head(prepared)
#' @export
prepare_rehydra_data <- function(
    data,
    time,
    genotype,
    treatment,
    replicate,
    variables = NULL,
    plant_id = NULL,
    block = NULL,
    plot_id = NULL,
    pot_id = NULL,
    cycle = NULL,
    phase = NULL,
    variable = NULL,
    value = NULL,
    response_direction = c("higher_is_better", "lower_is_better", "more_negative_is_worse", "auto"),
    transform = c("none", "absolute", "invert", "stress_score", "relative_to_control"),
    control_label = "Control"
) {
  response_direction <- match.arg(response_direction)
  transform <- match.arg(transform)

  if (is.character(data) && length(data) == 1 && file.exists(data)) {
    data <- readr::read_csv(data, show_col_types = FALSE)
  }

  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame, tibble, or path to a CSV file.")
  }

  dat <- tibble::as_tibble(data)

  time_col <- .as_col_name(dat, rlang::enquo(time), "time")
  geno_col <- .as_col_name(dat, rlang::enquo(genotype), "genotype")
  trt_col <- .as_col_name(dat, rlang::enquo(treatment), "treatment")
  rep_col <- .as_col_name(dat, rlang::enquo(replicate), "replicate", required = FALSE)
  plant_col <- .as_col_name(dat, rlang::enquo(plant_id), "plant_id", required = FALSE)
  block_col <- .as_col_name(dat, rlang::enquo(block), "block", required = FALSE)
  plot_col <- .as_col_name(dat, rlang::enquo(plot_id), "plot_id", required = FALSE)
  pot_col <- .as_col_name(dat, rlang::enquo(pot_id), "pot_id", required = FALSE)
  cycle_col <- .as_col_name(dat, rlang::enquo(cycle), "cycle", required = FALSE)
  phase_col <- .as_col_name(dat, rlang::enquo(phase), "phase", required = FALSE)
  var_col <- .as_col_name(dat, rlang::enquo(variable), "variable", required = FALSE)
  val_col <- .as_col_name(dat, rlang::enquo(value), "value", required = FALSE)
  var_cols <- .resolve_col_vector(dat, rlang::enquo(variables), arg_name = "variables")

  if (is.null(plant_col) && "plant_id" %in% names(dat)) plant_col <- "plant_id"
  if (is.null(block_col) && "block" %in% names(dat)) block_col <- "block"
  if (is.null(plot_col) && "plot_id" %in% names(dat)) plot_col <- "plot_id"
  if (is.null(pot_col) && "pot_id" %in% names(dat)) pot_col <- "pot_id"
  if (is.null(cycle_col) && "cycle" %in% names(dat)) cycle_col <- "cycle"
  if (is.null(phase_col) && "phase" %in% names(dat)) phase_col <- "phase"

  is_long <- FALSE
  if (!is.null(var_col) && !is.null(val_col)) {
    is_long <- TRUE
  }

  if (!is_long && length(var_cols) == 0 && all(c("variable", "value") %in% names(dat))) {
    is_long <- TRUE
    var_col <- "variable"
    val_col <- "value"
  }

  if (!is_long && length(var_cols) == 0) {
    rlang::abort("Provide either `variables` (wide input) or `variable`/`value` (long input).")
  }

  base_tbl <- tibble::tibble(
    plant_id = if (!is.null(plant_col)) {
      as.character(dat[[plant_col]])
    } else if (!is.null(rep_col)) {
      paste(
        as.character(dat[[geno_col]]),
        as.character(dat[[trt_col]]),
        as.character(dat[[rep_col]]),
        sep = "_"
      )
    } else {
      paste0("plant_", seq_len(nrow(dat)))
    },
    genotype = as.character(dat[[geno_col]]),
    treatment = as.character(dat[[trt_col]]),
    replicate = if (!is.null(rep_col)) as.character(dat[[rep_col]]) else "1",
    block = if (!is.null(block_col)) as.character(dat[[block_col]]) else NA_character_,
    plot_id = if (!is.null(plot_col)) as.character(dat[[plot_col]]) else NA_character_,
    pot_id = if (!is.null(pot_col)) as.character(dat[[pot_col]]) else NA_character_,
    time = .as_numeric_time(dat[[time_col]]),
    cycle = if (!is.null(cycle_col)) suppressWarnings(as.integer(dat[[cycle_col]])) else NA_integer_,
    phase = if (!is.null(phase_col)) as.character(dat[[phase_col]]) else NA_character_
  )

  long_tbl <- if (is_long) {
    dplyr::bind_cols(
      base_tbl,
      tibble::tibble(
        variable = as.character(dat[[var_col]]),
        value = suppressWarnings(as.numeric(dat[[val_col]]))
      )
    )
  } else {
    dplyr::bind_cols(base_tbl, dat[, var_cols, drop = FALSE]) |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(var_cols),
        names_to = "variable",
        values_to = "value"
      ) |>
      dplyr::mutate(value = suppressWarnings(as.numeric(.data$value)))
  }

  long_tbl <- long_tbl |>
    dplyr::mutate(
      response_direction = if (response_direction == "auto") {
        vapply(.data$variable, .detect_direction, FUN.VALUE = character(1))
      } else {
        response_direction
      }
    ) |>
    dplyr::mutate(
      .directional_value = purrr::map2_dbl(.data$value, .data$response_direction, .apply_direction)
    )

  if (transform == "none") {
    long_tbl <- dplyr::mutate(long_tbl, transformed_value = .data$.directional_value)
  } else if (transform == "absolute") {
    long_tbl <- dplyr::mutate(long_tbl, transformed_value = abs(.data$.directional_value))
  } else if (transform == "invert") {
    long_tbl <- dplyr::mutate(long_tbl, transformed_value = -1 * .data$.directional_value)
  } else if (transform == "stress_score") {
    long_tbl <- long_tbl |>
      dplyr::group_by(.data$variable) |>
      dplyr::mutate(transformed_value = .rescale01(.data$.directional_value)) |>
      dplyr::ungroup()
  } else if (transform == "relative_to_control") {
    ctrl <- long_tbl |>
      dplyr::filter(.data$treatment == control_label) |>
      dplyr::group_by(.data$genotype, .data$variable, .data$time) |>
      dplyr::summarise(.control_mean = .safe_mean(.data$.directional_value), .groups = "drop")

    long_tbl <- long_tbl |>
      dplyr::left_join(ctrl, by = c("genotype", "variable", "time")) |>
      dplyr::mutate(transformed_value = .safe_div(.data$.directional_value, .data$.control_mean)) |>
      dplyr::select(-.data$.control_mean)
  }

  # Keep performance scale strictly positive and directionally consistent
  # before ratio-based resilience calculations.
  long_tbl <- long_tbl |>
    dplyr::group_by(.data$variable) |>
    dplyr::mutate(transformed_value = .to_positive_performance(.data$transformed_value)) |>
    dplyr::ungroup()

  out_cols <- c(
    "plant_id", "genotype", "treatment", "replicate", "block", "plot_id", "pot_id",
    "time", "cycle", "phase", "variable", "value", "response_direction", "transformed_value"
  )

  out <- long_tbl |>
    dplyr::select(dplyr::all_of(out_cols))

  dup_n <- out |>
    dplyr::count(.data$plant_id, .data$replicate, .data$variable, .data$time, name = "n") |>
    dplyr::filter(.data$n > 1) |>
    nrow()
  if (dup_n > 0) {
    warning("Duplicated time points were found within plant_id, replicate, and variable.", call. = FALSE)
  }

  out
}

#' Validate rehydra Input Data
#'
#' Check common data-quality issues before segmentation and metric estimation.
#'
#' @param data A data frame in rehydra format.
#'
#' @return A tibble with columns `issue`, `severity`, `column`, `group`,
#'   `message`, and `suggestion`.
#'
#' @examples
#' data(rehydra_data)
#' prepared <- prepare_rehydra_data(
#'   data = rehydra_data,
#'   time = time_days,
#'   genotype = genotype,
#'   treatment = treatment,
#'   replicate = replicate,
#'   variables = c(water_potential, stomatal_conductance)
#' )
#' check_rehydra_data(prepared)
#' @export
check_rehydra_data <- function(data) {
  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame or tibble.")
  }

  dat <- tibble::as_tibble(data)

  add_issue <- function(tbl, issue, severity, column = NA_character_, group = NA_character_, message, suggestion) {
    dplyr::bind_rows(
      tbl,
      tibble::tibble(
        issue = issue,
        severity = severity,
        column = column,
        group = group,
        message = message,
        suggestion = suggestion
      )
    )
  }

  issues <- tibble::tibble(
    issue = character(),
    severity = character(),
    column = character(),
    group = character(),
    message = character(),
    suggestion = character()
  )

  required <- c("plant_id", "genotype", "treatment", "replicate", "time", "variable", "value")
  missing_required <- setdiff(required, names(dat))
  if (length(missing_required) > 0) {
    issues <- add_issue(
      issues,
      issue = "missing_required_columns",
      severity = "error",
      column = "multiple",
      message = paste("Missing required columns:", paste(missing_required, collapse = ", ")),
      suggestion = "Run prepare_rehydra_data() and verify column mappings."
    )
    return(issues)
  }

  for (col in intersect(c("time", "value", "transformed_value"), names(dat))) {
    n_miss <- sum(is.na(dat[[col]]))
    if (n_miss > 0) {
      issues <- add_issue(
        issues,
        issue = "missing_values",
        severity = "warning",
        column = col,
        message = paste(n_miss, "missing values were found."),
        suggestion = "Review missingness and decide on interpolation or exclusion."
      )
    }
  }

  dup <- dat |>
    dplyr::count(.data$plant_id, .data$replicate, .data$variable, .data$time, name = "n") |>
    dplyr::filter(.data$n > 1)
  if (nrow(dup) > 0) {
    issues <- add_issue(
      issues,
      issue = "duplicated_time_points",
      severity = "warning",
      column = "time",
      message = "Duplicated time points were detected within plant, replicate, and variable.",
      suggestion = "Aggregate duplicates or keep one observation per time point."
    )
  }

  if ("cycle" %in% names(dat) && "phase" %in% names(dat)) {
    cyc <- dat |>
      dplyr::filter(.data$treatment == "Drought") |>
      dplyr::group_by(.data$cycle) |>
      dplyr::summarise(n_phase = dplyr::n_distinct(.data$phase[!is.na(.data$phase)]), .groups = "drop") |>
      dplyr::filter(.data$n_phase > 0 & .data$n_phase < 4)

    if (nrow(cyc) > 0) {
      issues <- add_issue(
        issues,
        issue = "incomplete_cycles",
        severity = "warning",
        column = "cycle",
        message = "Some drought cycles do not contain the four required phases.",
        suggestion = "Re-segment using manual events or verify automatic boundaries."
      )
    }
  }

  trt_levels <- unique(as.character(dat$treatment))
  if (!any(tolower(trt_levels) == "control")) {
    issues <- add_issue(
      issues,
      issue = "missing_control_treatment",
      severity = "warning",
      column = "treatment",
      message = "No control treatment level was found.",
      suggestion = "Include a control group for relative and stability analyses."
    )
  }

  if (!any(tolower(trt_levels) == "drought")) {
    issues <- add_issue(
      issues,
      issue = "missing_drought_treatment",
      severity = "warning",
      column = "treatment",
      message = "No drought treatment level was found.",
      suggestion = "Include at least one drought group for cycle metrics."
    )
  }

  if ("phase" %in% names(dat)) {
    missing_phases <- setdiff(c("PreDrought", "Drought", "Rewatering", "PostDrought"), unique(dat$phase))
    if (length(missing_phases) > 0) {
      issues <- add_issue(
        issues,
        issue = "missing_recovery_phase",
        severity = "warning",
        column = "phase",
        message = paste("Missing phases:", paste(missing_phases, collapse = ", ")),
        suggestion = "Run segment_drought_cycle() or provide a complete manual events table."
      )
    }
  } else {
    issues <- add_issue(
      issues,
      issue = "missing_phase_column",
      severity = "warning",
      column = "phase",
      message = "No phase column was found in the input data.",
      suggestion = "Run segment_drought_cycle() before damage, recovery, or resilience metrics."
    )
  }

  if ("response_direction" %in% names(dat)) {
    valid_dir <- c("higher_is_better", "lower_is_better", "more_negative_is_worse")
    invalid_dir <- sum(is.na(dat$response_direction) | !dat$response_direction %in% valid_dir)
    if (invalid_dir > 0) {
      issues <- add_issue(
        issues,
        issue = "undefined_variable_direction",
        severity = "warning",
        column = "response_direction",
        message = paste(invalid_dir, "rows have undefined response direction."),
        suggestion = "Set response_direction explicitly in prepare_rehydra_data()."
      )
    }
  }

  few <- dat |>
    dplyr::group_by(
      .data$genotype,
      .data$treatment,
      .data$replicate,
      .data$variable,
      .data$cycle
    ) |>
    dplyr::summarise(n = sum(is.finite(.data$value)), .groups = "drop") |>
    dplyr::filter(.data$n < 3)
  if (nrow(few) > 0) {
    issues <- add_issue(
      issues,
      issue = "groups_with_too_few_observations",
      severity = "warning",
      column = "value",
      message = "Some groups have fewer than three finite observations.",
      suggestion = "Increase replication or reduce grouping granularity."
    )
  }

  if ("variable" %in% names(dat)) {
    stoma <- dat |>
      dplyr::filter(.data$variable == "stomatal_conductance")
    if (nrow(stoma) > 0 && any(stoma$value < 0, na.rm = TRUE)) {
      issues <- add_issue(
        issues,
        issue = "impossible_values",
        severity = "warning",
        column = "stomatal_conductance",
        message = "Negative stomatal conductance values were detected.",
        suggestion = "Check data entry and units for stomatal conductance."
      )
    }
  }

  if (nrow(issues) == 0) {
    issues <- tibble::tibble(
      issue = "no_issues_detected",
      severity = "ok",
      column = NA_character_,
      group = NA_character_,
      message = "No critical issues were detected.",
      suggestion = "Proceed with the full rehydra pipeline."
    )
  }

  issues
}

#' Summarize the Full rehydra Pipeline
#'
#' Run preparation, checks, segmentation, damage/recovery metrics,
#' resilience indices, memory metrics, priming classification, and genotype
#' comparison in one call.
#'
#' @inheritParams prepare_rehydra_data
#' @param segmentation_method Either `"auto"` or `"manual"`.
#' @param segmentation_events Optional events table for manual segmentation.
#' @param smoothing Logical; apply smoothing in auto segmentation.
#' @param smooth_span Loess smoothing span.
#' @param min_drop Minimum relative drop criterion for auto segmentation.
#' @param min_recovery Minimum relative recovery criterion for auto segmentation.
#' @param n_cycles Optional expected number of cycles.
#' @param cycle_ref Reference cycle for memory metrics.
#' @param cycle_test Test cycle for memory metrics.
#' @param ci_method Confidence interval method for resilience and memory.
#' @param n_boot Number of bootstrap iterations when `ci_method = "bootstrap"`.
#' @param conf_level Confidence level.
#' @param memory_min_cycles Minimum number of segmented cycles required before
#'   [memory_trend()] is fitted.
#'
#' @return An S3 object of class `rehydra_analysis` with components:
#' `data`, `checks`, `segments`, `damage`, `recovery`, `recovery_period`, `mean_reduction`,
#' `curve_shape`, `resilience`, `memory`, `memory_trend`, `stability`,
#' `classification`, `genotype_comparison`, `parameters`, and `references`.
#'
#' @examples
#' data(rehydra_data)
#'
#' # One variable keeps the example fast; pass several to `variables` to run
#' # the whole pipeline on all of them at once.
#' result <- summarize_rehydra(
#'   data = rehydra_data,
#'   time = time_days,
#'   genotype = genotype,
#'   treatment = treatment,
#'   replicate = replicate,
#'   variables = water_potential
#' )
#' result
#'
#' result$curve_shape[, c("genotype", "cycle", "deficit_auc", "t50")]
#' result$memory_trend[, c("genotype", "metric", "slope", "memory_class")]
#'
#' \donttest{
#' # Several variables in the same call
#' summarize_rehydra(
#'   data = rehydra_data,
#'   time = time_days,
#'   genotype = genotype,
#'   treatment = treatment,
#'   replicate = replicate,
#'   variables = c(water_potential, stomatal_conductance)
#' )
#' }
#' @export
summarize_rehydra <- function(
    data,
    time,
    genotype,
    treatment,
    replicate,
    variables = NULL,
    plant_id = NULL,
    block = NULL,
    plot_id = NULL,
    pot_id = NULL,
    cycle = NULL,
    phase = NULL,
    variable = NULL,
    value = NULL,
    response_direction = c("higher_is_better", "lower_is_better", "more_negative_is_worse", "auto"),
    transform = c("none", "absolute", "invert", "stress_score", "relative_to_control"),
    control_label = "Control",
    segmentation_method = c("auto", "manual"),
    segmentation_events = NULL,
    smoothing = TRUE,
    smooth_span = 0.25,
    min_drop = 0.15,
    min_recovery = 0.10,
    n_cycles = NULL,
    cycle_ref = 1,
    cycle_test = 2,
    ci_method = c("none", "bootstrap"),
    n_boot = 999,
    conf_level = 0.95,
    memory_min_cycles = 2
) {
  response_direction <- match.arg(response_direction)
  transform <- match.arg(transform)
  segmentation_method <- match.arg(segmentation_method)
  ci_method <- match.arg(ci_method)

  prepared <- prepare_rehydra_data(
    data = data,
    time = {{ time }},
    genotype = {{ genotype }},
    treatment = {{ treatment }},
    replicate = {{ replicate }},
    variables = {{ variables }},
    plant_id = {{ plant_id }},
    block = {{ block }},
    plot_id = {{ plot_id }},
    pot_id = {{ pot_id }},
    cycle = {{ cycle }},
    phase = {{ phase }},
    variable = {{ variable }},
    value = {{ value }},
    response_direction = response_direction,
    transform = transform,
    control_label = control_label
  )

  checks <- check_rehydra_data(prepared)

  segments <- segment_drought_cycle(
    data = prepared,
    time = time,
    value = transformed_value,
    group_by = c("genotype", "treatment", "replicate", "plant_id", "variable"),
    method = segmentation_method,
    events = segmentation_events,
    smoothing = smoothing,
    smooth_span = smooth_span,
    min_drop = min_drop,
    min_recovery = min_recovery,
    n_cycles = n_cycles,
    response_direction = response_direction
  )

  damage <- damage_metrics(segments)
  recovery <- recovery_metrics(segments)
  recovery_period <- recovery_period_metrics(segments)
  mean_reduction <- mean_reduction_metrics(segments)
  curve_shape <- curve_shape_metrics(segments)

  resilience <- resilience_index(
    data = segments,
    conf_level = conf_level,
    ci_method = ci_method,
    n_boot = n_boot
  )

  memory <- memory_effect(
    resilience_results = resilience,
    damage_results = damage,
    recovery_results = recovery,
    cycle_ref = cycle_ref,
    cycle_test = cycle_test,
    ci_method = ci_method,
    n_boot = n_boot,
    conf_level = conf_level
  )

  classification <- priming_classification(memory)
  stability <- stability_index(segments)
  genotype_comparison <- compare_genotypes(resilience, metric = "Rs")

  # Multi-cycle memory: only meaningful once `min_cycles` distinct cycles were
  # segmented, so the call is guarded rather than allowed to return empty rows.
  trend <- tryCatch(
    memory_trend(resilience, min_cycles = memory_min_cycles),
    error = function(e) tibble::tibble()
  )

  result <- list(
    data = prepared,
    checks = checks,
    segments = segments,
    damage = damage,
    recovery = recovery,
    recovery_period = recovery_period,
    mean_reduction = mean_reduction,
    curve_shape = curve_shape,
    resilience = resilience,
    memory = memory,
    memory_trend = trend,
    stability = stability,
    classification = classification,
    genotype_comparison = genotype_comparison,
    parameters = list(
      response_direction = response_direction,
      transform = transform,
      segmentation_method = segmentation_method,
      min_drop = min_drop,
      min_recovery = min_recovery,
      cycle_ref = cycle_ref,
      cycle_test = cycle_test,
      ci_method = ci_method,
      n_boot = n_boot,
      conf_level = conf_level,
      memory_min_cycles = memory_min_cycles
    ),
    references = c(
      "Lloret et al. (2011) doi:10.1111/j.1600-0706.2011.19372.x",
      "Xu et al. (2010) doi:10.4161/psb.5.6.11398",
      "Ribeiro et al. (2021) doi:10.1016/j.jplph.2021.153397"
    )
  )

  class(result) <- "rehydra_analysis"
  result
}

#' Print Method for rehydra Analysis Objects
#'
#' @param x A `rehydra_analysis` object.
#' @param ... Not used.
#'
#' @return Invisibly returns `x`.
#' @method print rehydra_analysis
#' @export
print.rehydra_analysis <- function(x, ...) {
  cat("<rehydra_analysis>\n")
  cat("Rows (prepared):", nrow(x$data), "\n")
  cat("Rows (segments):", nrow(x$segments), "\n")
  cat("Rows (damage):", nrow(x$damage), "\n")
  cat("Rows (recovery):", nrow(x$recovery), "\n")
  cat("Rows (curve shape):", nrow(x$curve_shape %||% tibble::tibble()), "\n")
  cat("Rows (period/reduction):", nrow(x$recovery_period %||% tibble::tibble()), "\n")
  cat("Rows (resilience):", nrow(x$resilience), "\n")
  cat("Rows (memory):", nrow(x$memory), "\n")
  cat("Rows (memory trend):", nrow(x$memory_trend %||% tibble::tibble()), "\n")
  invisible(x)
}

#' Summary Method for rehydra Analysis Objects
#'
#' @param object A `rehydra_analysis` object.
#' @param ... Not used.
#'
#' @return A one-row tibble with object dimensions.
#' @method summary rehydra_analysis
#' @export
summary.rehydra_analysis <- function(object, ...) {
  tibble::tibble(
    n_rows_prepared = nrow(object$data),
    n_rows_segments = nrow(object$segments),
    n_rows_damage = nrow(object$damage),
    n_rows_recovery = nrow(object$recovery),
    n_rows_recovery_period = nrow(object$recovery_period %||% tibble::tibble()),
    n_rows_mean_reduction = nrow(object$mean_reduction %||% tibble::tibble()),
    n_rows_curve_shape = nrow(object$curve_shape %||% tibble::tibble()),
    n_rows_resilience = nrow(object$resilience),
    n_rows_memory = nrow(object$memory),
    n_rows_memory_trend = nrow(object$memory_trend %||% tibble::tibble()),
    n_checks = nrow(object$checks)
  )
}

#' Plot Method for rehydra Analysis Objects
#'
#' @param x A `rehydra_analysis` object.
#' @param ... Additional arguments forwarded to
#'   [plot_recovery_trajectory()].
#'
#' @return A `ggplot2` object.
#' @method plot rehydra_analysis
#' @export
plot.rehydra_analysis <- function(x, ...) {
  plot_recovery_trajectory(
    data = x$segments,
    resilience_results = x$resilience,
    ...
  )
}

#' Tidy Method for rehydra Analysis Objects
#'
#' @param x A `rehydra_analysis` object.
#' @param ... Not used.
#'
#' @return A tibble combining resilience and memory metrics.
#' @method tidy rehydra_analysis
#' @export
tidy.rehydra_analysis <- function(x, ...) {
  x$resilience |>
    dplyr::left_join(
      x$memory |>
        dplyr::select(
          dplyr::any_of(c(
            "genotype", "treatment", "variable", "cycle_ref", "cycle_test",
            "Mem_Rt", "Mem_Rc", "Mem_Rs", "Mem_Damage", "Mem_RecRate", "Mem_Residual"
          ))
        ),
      by = c("genotype", "treatment", "variable")
    )
}

#' Augment Method for rehydra Analysis Objects
#'
#' @param x A `rehydra_analysis` object.
#' @param ... Not used.
#'
#' @return A tibble joining segmented trajectories and per-cycle indices.
#' @method augment rehydra_analysis
#' @export
augment.rehydra_analysis <- function(x, ...) {
  key <- c("genotype", "treatment", "replicate", "plant_id", "variable", "cycle")

  out <- x$segments |>
    dplyr::left_join(
      x$resilience |>
        dplyr::select(dplyr::any_of(c(key, "Rt", "Rc", "Rs"))),
      by = key
    )

  # The curve-shape landmarks are per cycle, so joining them onto the segmented
  # rows lets a single tibble carry both the trajectory and the indices derived
  # from it - convenient for custom plots and for export.
  shape <- x$curve_shape
  if (!is.null(shape) && nrow(shape) > 0) {
    out <- out |>
      dplyr::left_join(
        shape |>
          dplyr::select(dplyr::any_of(c(
            key, "deficit_auc", "deficit_auc_normalized", "t50",
            "recovery_rate_k", "latency", "residual_deficit"
          ))),
        by = key
      )
  }

  out
}

#' Glance Method for rehydra Analysis Objects
#'
#' @param x A `rehydra_analysis` object.
#' @param ... Not used.
#'
#' @return A one-row tibble with compact model diagnostics.
#' @method glance rehydra_analysis
#' @export
glance.rehydra_analysis <- function(x, ...) {
  tibble::tibble(
    n_genotypes = dplyr::n_distinct(x$data$genotype),
    n_treatments = dplyr::n_distinct(x$data$treatment),
    n_variables = dplyr::n_distinct(x$data$variable),
    n_cycles = dplyr::n_distinct(x$segments$cycle[!is.na(x$segments$cycle)]),
    n_memory_groups = nrow(x$memory)
  )
}
