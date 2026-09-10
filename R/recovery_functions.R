#' Calculate Recovery Metrics After Rewatering
#'
#' Estimate the magnitude and speed of physiological recovery following drought.
#'
#' @param data Segmented data from [segment_drought_cycle()].
#' @param time Time column.
#' @param value Value column used for calculations.
#' @param group_by Grouping columns.
#' @param recovery_threshold Fraction of pre-drought performance used for
#'   `time_to_recovery`.
#' @param post_summary Summary used to define post-drought level:
#'   `"mean"` or `"median"`.
#' @param allow_overcompensation Logical; whether values above pre-drought
#'   baseline are allowed and flagged.
#'
#' @details
#' Metrics include:
#'
#' - `post_rewatering_raw`
#' - `post_rewatering_value`
#' - `recovery_rate = (PostDrought - DroughtMinimum) / duration_recovery`
#' - `max_recovery_rate` (maximum first derivative during recovery)
#' - `recovery_completeness = PostDrought / PreDrought`
#' - `residual_cost = 1 - (PostDrought / PreDrought)`
#' - `recovery_auc`
#' - `normalized_recovery_auc`
#' - `time_to_recovery`
#'
#' @return A tibble of recovery metrics by group and cycle.
#'
#' @references
#' Xu et al. (2010) <doi:10.4161/psb.5.6.11398>.
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
#' recovery_metrics(segments)
#' @export
recovery_metrics <- function(
    data,
    time = time,
    value = transformed_value,
    group_by = c("genotype", "treatment", "replicate", "plant_id", "variable", "cycle"),
    recovery_threshold = 0.90,
    post_summary = c("mean", "median"),
    allow_overcompensation = TRUE
) {
  post_summary <- match.arg(post_summary)

  dat <- tibble::as_tibble(data)

  if (!"phase" %in% names(dat)) {
    rlang::abort("`phase` column is required. Run segment_drought_cycle() first.")
  }

  time_col <- .as_col_name(dat, rlang::enquo(time), "time")
  value_col <- .as_col_name(dat, rlang::enquo(value), "value")
  group_cols <- .resolve_group_cols(dat, group_by)

  dat <- dat |>
    dplyr::mutate(
      .time = suppressWarnings(as.numeric(.data[[time_col]])),
      .value = suppressWarnings(as.numeric(.data[[value_col]]))
    )

  compute_group <- function(tbl) {
    pre <- tbl |>
      dplyr::filter(.data$phase == "PreDrought")
    drought <- tbl |>
      dplyr::filter(.data$phase == "Drought") |>
      dplyr::arrange(.data$.time)
    rew <- tbl |>
      dplyr::filter(.data$phase %in% c("Rewatering", "PostDrought", "RecoveryPlateau")) |>
      dplyr::arrange(.data$.time)
    post <- tbl |>
      dplyr::filter(.data$phase == "PostDrought")

    pre_ref <- .safe_mean(pre$.value)
    drought_min <- .safe_min(drought$.value)

    post_value_raw <- if (post_summary == "mean") {
      .safe_mean(post$.value)
    } else {
      .safe_median(post$.value)
    }

    if (!is.finite(post_value_raw)) {
      post_value_raw <- .safe_mean(rew$.value)
    }

    overcomp <- isTRUE(post_value_raw > pre_ref)
    post_value <- post_value_raw
    if (!allow_overcompensation && overcomp && is.finite(pre_ref)) {
      post_value <- pre_ref
    }

    rew_ok <- is.finite(rew$.time) & is.finite(rew$.value)

    duration_rec <- if (sum(rew_ok) > 1) {
      .safe_max(rew$.time[rew_ok]) - .safe_min(rew$.time[rew_ok])
    } else {
      NA_real_
    }

    rec_rate <- .safe_div(post_value - drought_min, duration_rec)

    deriv <- diff(rew$.value[rew_ok]) / diff(rew$.time[rew_ok])
    max_rec_rate <- if (length(deriv) > 0 && any(is.finite(deriv))) .safe_max(deriv) else NA_real_

    completeness <- .safe_div(post_value, pre_ref)
    residual <- 1 - completeness

    rec_curve <- rew$.value[rew_ok] - drought_min
    rec_curve[rec_curve < 0] <- 0
    rec_auc <- .trapezoid_integral(rew$.time[rew_ok], rec_curve)
    norm_rec_auc <- .safe_div(rec_auc, duration_rec * abs(pre_ref))

    threshold_value <- pre_ref * recovery_threshold
    t_recovery <- NA_real_
    if (is.finite(threshold_value) && any(rew_ok)) {
      hit <- which(rew_ok & rew$.value >= threshold_value)
      if (length(hit) > 0) {
        t_recovery <- rew$.time[hit[[1]]] - .safe_min(rew$.time[rew_ok])
      }
    }

    tibble::tibble(
      PreDrought = pre_ref,
      drought_minimum = drought_min,
      post_rewatering_raw = post_value_raw,
      post_rewatering_value = post_value,
      recovery_rate = rec_rate,
      max_recovery_rate = max_rec_rate,
      recovery_completeness = completeness,
      residual_cost = residual,
      recovery_auc = rec_auc,
      normalized_recovery_auc = norm_rec_auc,
      time_to_recovery = t_recovery,
      duration_recovery = duration_rec,
      overcompensation = overcomp
    )
  }

  if (length(group_cols) == 0) {
    return(compute_group(dat))
  }

  dat |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::group_modify(~compute_group(.x)) |>
    dplyr::ungroup()
}
