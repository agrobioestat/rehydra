#' Calculate Drought Damage Metrics
#'
#' Quantify intensity and dynamics of physiological damage during drought.
#'
#' @param data Segmented data from [segment_drought_cycle()].
#' @param time Time column.
#' @param value Value column used for calculations.
#' @param group_by Grouping columns.
#' @param threshold Relative drop threshold used to compute
#'   `time_to_damage`. If `NULL`, time to the absolute minimum is used.
#'
#' @details
#' Metrics include:
#'
#' - `drought_minimum`
#' - `drought_drop_absolute = PreDrought - DroughtMinimum`
#' - `drought_drop_relative = (PreDrought - DroughtMinimum) / PreDrought`
#' - `damage_rate = (DroughtMinimum - PreDrought) / duration_drought`
#' - `max_decline_rate` (minimum first derivative during drought)
#' - `damage_auc = integral(PreDrought_reference - observed_value) dt`
#' - `normalized_damage_auc = damage_auc / (duration_drought * |PreDrought|)`
#' - `time_to_damage`
#'
#' @return A tibble of drought-damage metrics by group and cycle.
#'
#' @references
#' Lloret et al. (2011) <doi:10.1111/j.1600-0706.2011.19372.x>.
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
#' damage_metrics(segments)
#' @export
damage_metrics <- function(
    data,
    time = time,
    value = transformed_value,
    group_by = c("genotype", "treatment", "replicate", "plant_id", "variable", "cycle"),
    threshold = NULL
) {
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
    dr <- tbl |>
      dplyr::filter(.data$phase == "Drought") |>
      dplyr::arrange(.data$.time)

    pre_ref <- .safe_mean(pre$.value)
    drought_min <- .safe_min(dr$.value)

    drop_abs <- pre_ref - drought_min
    drop_rel <- .safe_div(drop_abs, pre_ref)

    dr_t <- dr$.time
    dr_v <- dr$.value
    dr_ok <- is.finite(dr_t) & is.finite(dr_v)
    duration <- if (sum(dr_ok) > 1) {
      .safe_max(dr_t[dr_ok]) - .safe_min(dr_t[dr_ok])
    } else {
      NA_real_
    }

    damage_rate <- .safe_div(drought_min - pre_ref, duration)

    deriv <- diff(dr_v[dr_ok]) / diff(dr_t[dr_ok])
    max_decline <- if (length(deriv) > 0 && any(is.finite(deriv))) .safe_min(deriv) else NA_real_

    damage_curve <- pre_ref - dr_v[dr_ok]
    damage_curve[damage_curve < 0] <- 0
    auc <- .trapezoid_integral(dr_t[dr_ok], damage_curve)

    norm_auc <- .safe_div(auc, duration * abs(pre_ref))

    t0 <- if (nrow(pre) > 0 && any(is.finite(pre$.time))) {
      .safe_max(pre$.time)
    } else {
      .safe_min(dr_t[dr_ok])
    }
    t_min <- if (sum(dr_ok) == 0) NA_real_ else dr_t[dr_ok][which.min(dr_v[dr_ok])[1]]

    time_to_damage <- t_min - t0

    if (!is.null(threshold) && is.finite(pre_ref) && is.finite(threshold)) {
      thr_val <- pre_ref * (1 - threshold)
      hit <- which(dr_ok & dr_v <= thr_val)
      if (length(hit) > 0) {
        time_to_damage <- dr_t[hit[1]] - t0
      }
    }

    tibble::tibble(
      PreDrought = pre_ref,
      drought_minimum = drought_min,
      drought_drop_absolute = drop_abs,
      drought_drop_relative = drop_rel,
      damage_rate = damage_rate,
      max_decline_rate = max_decline,
      damage_auc = auc,
      normalized_damage_auc = norm_auc,
      time_to_damage = time_to_damage,
      duration_drought = duration
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
