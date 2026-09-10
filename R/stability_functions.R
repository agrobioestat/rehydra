#' Calculate Operational Stability Metrics
#'
#' Quantify temporal stability under repeated drought-rewatering cycles.
#'
#' @param data A prepared or segmented data frame.
#' @param time Time column.
#' @param value Value column.
#' @param group_by Grouping columns.
#' @param control_label Label used to identify control treatment.
#'
#' @details
#' This function implements operational stability metrics inspired by
#' drought-memory concepts:
#'
#' - `temporal_cv`
#' - `control_deviation`
#' - `normalized_stability = 1 - normalized_deviation`
#' - `memory_stability_gain = stability_C2 - stability_C1`
#'
#' These metrics are practical, analysis-oriented indicators and not universal
#' ecological formulas. For the published stability index of Ribeiro et al.
#' (2021) - the ratio of the recovery rate to the disturbance rate, with its
#' impact, integrated-impact and perturbation components - use
#' [overall_stability()] instead.
#'
#' @return A tibble with stability metrics by group and cycle.
#'
#' @references
#' Ribeiro et al. (2021) <doi:10.1016/j.jplph.2021.153397>.
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
#' segments <- segment_drought_cycle(prepared, time = time, value = transformed_value)
#' stability_index(segments)
#' @export
stability_index <- function(
    data,
    time = time,
    value = transformed_value,
    group_by = c("genotype", "treatment", "replicate", "plant_id", "variable", "cycle"),
    control_label = "Control"
) {
  dat <- tibble::as_tibble(data)
  time_col <- .as_col_name(dat, rlang::enquo(time), "time")
  value_col <- .as_col_name(dat, rlang::enquo(value), "value")
  group_cols <- .resolve_group_cols(dat, group_by)

  if (!"cycle" %in% names(dat)) {
    dat$cycle <- 1L
  }

  dat <- dat |>
    dplyr::mutate(
      .time = suppressWarnings(as.numeric(.data[[time_col]])),
      .value = suppressWarnings(as.numeric(.data[[value_col]]))
    )

  ctl <- dat |>
    dplyr::filter(.data$treatment == control_label) |>
    dplyr::group_by(.data$genotype, .data$variable, .data$.time) |>
    dplyr::summarise(control_mean = mean(.data$.value, na.rm = TRUE), .groups = "drop")

  joined <- dat |>
    dplyr::left_join(ctl, by = c("genotype", "variable", ".time")) |>
    dplyr::mutate(
      deviation = abs(.data$.value - .data$control_mean)
    )

  base_tbl <- if (length(group_cols) == 0) {
    joined |>
      dplyr::summarise(
        temporal_cv = .safe_div(stats::sd(.data$.value, na.rm = TRUE), abs(mean(.data$.value, na.rm = TRUE))),
        control_deviation = mean(.data$deviation, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    joined |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      dplyr::summarise(
        temporal_cv = .safe_div(stats::sd(.data$.value, na.rm = TRUE), abs(mean(.data$.value, na.rm = TRUE))),
        control_deviation = mean(.data$deviation, na.rm = TRUE),
        .groups = "drop"
      )
  }

  base_tbl <- base_tbl |>
    dplyr::mutate(
      normalized_deviation = .rescale01(.data$control_deviation),
      normalized_stability = .clamp01(1 - .data$normalized_deviation)
    )

  if (!"cycle" %in% names(base_tbl)) {
    base_tbl$memory_stability_gain <- NA_real_
    return(base_tbl)
  }

  gain_tbl <- base_tbl |>
    dplyr::group_by(dplyr::across(dplyr::all_of(intersect(c("genotype", "treatment", "variable"), names(base_tbl))))) |>
    dplyr::arrange(.data$cycle, .by_group = TRUE) |>
    dplyr::mutate(
      memory_stability_gain = .data$normalized_stability - dplyr::lag(.data$normalized_stability)
    ) |>
    dplyr::ungroup()

  gain_tbl
}
