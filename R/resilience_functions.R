#' Calculate Resistance, Recovery, and Resilience Indices
#'
#' Compute Rt, Rc, and Rs following Lloret et al. (2011).
#'
#' @param data Segmented data from [segment_drought_cycle()].
#' @param time Time column.
#' @param value Value column used for calculations.
#' @param group_by Grouping columns.
#' @param drought_summary Summary method for drought level:
#'   `"minimum"`, `"mean"`, or `"last"`.
#' @param predrought_summary Summary method for pre-drought level:
#'   `"mean"`, `"median"`, or `"last"`.
#' @param postdrought_summary Summary method for post-drought level:
#'   `"mean"`, `"median"`, or `"last"`.
#' @param conf_level Confidence level for bootstrap intervals.
#' @param ci_method Either `"none"` or `"bootstrap"`.
#' @param n_boot Number of bootstrap resamples.
#'
#' @details
#' Formulas:
#'
#' - `Rt = Drought / PreDrought`
#' - `Rc = PostDrought / Drought`
#' - `Rs = PostDrought / PreDrought`
#'
#' These ratios are directly interpretable when higher values indicate better
#' physiological performance. For variables such as water potential,
#' apply a direction-aware transformation before interpretation.
#'
#' @return A tibble with `PreDrought`, `Drought`, `PostDrought`, `Rt`, `Rc`,
#'   `Rs`, `standard_error`, `conf_low`, `conf_high`, index-specific bootstrap
#'   interval columns (`*_Rt`, `*_Rc`, `*_Rs`), and `method`.
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
#' resilience_index(segments)
#' @export
resilience_index <- function(
    data,
    time = time,
    value = transformed_value,
    group_by = c("genotype", "treatment", "replicate", "plant_id", "variable", "cycle"),
    drought_summary = c("minimum", "mean", "last"),
    predrought_summary = c("mean", "median", "last"),
    postdrought_summary = c("mean", "median", "last"),
    conf_level = 0.95,
    ci_method = c("none", "bootstrap"),
    n_boot = 999
) {
  drought_summary <- match.arg(drought_summary)
  predrought_summary <- match.arg(predrought_summary)
  postdrought_summary <- match.arg(postdrought_summary)
  ci_method <- match.arg(ci_method)

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
    ) |>
    dplyr::arrange(.data$.time)

  summarize_phase <- function(tbl, phase_name, summary_name) {
    vec <- tbl$.value[tbl$phase == phase_name]
    .pick_summary(vec, method = summary_name)
  }

  compute_group <- function(tbl) {
    pre <- summarize_phase(tbl, "PreDrought", predrought_summary)
    drought <- summarize_phase(tbl, "Drought", ifelse(drought_summary == "minimum", "minimum", drought_summary))
    post <- summarize_phase(tbl, "PostDrought", postdrought_summary)

    rt <- .safe_div(drought, pre)
    rc <- .safe_div(post, drought)
    rs <- .safe_div(post, pre)

    se_rt <- NA_real_
    se_rc <- NA_real_
    se_rs <- NA_real_
    ci_low_rt <- NA_real_
    ci_high_rt <- NA_real_
    ci_low_rc <- NA_real_
    ci_high_rc <- NA_real_
    ci_low_rs <- NA_real_
    ci_high_rs <- NA_real_
    method <- "analytic"

    if (ci_method == "bootstrap") {
      pre_vec <- tbl$.value[tbl$phase == "PreDrought"]
      dr_vec <- tbl$.value[tbl$phase == "Drought"]
      post_vec <- tbl$.value[tbl$phase == "PostDrought"]

      if (length(pre_vec) >= 2 && length(dr_vec) >= 2 && length(post_vec) >= 2) {
        boot_mat <- replicate(n_boot, {
          pre_b <- sample(pre_vec, replace = TRUE)
          dr_b <- sample(dr_vec, replace = TRUE)
          post_b <- sample(post_vec, replace = TRUE)

          pre_s <- .pick_summary(pre_b, predrought_summary)
          dr_s <- .pick_summary(dr_b, ifelse(drought_summary == "minimum", "minimum", drought_summary))
          post_s <- .pick_summary(post_b, postdrought_summary)

          c(
            Rt = .safe_div(dr_s, pre_s),
            Rc = .safe_div(post_s, dr_s),
            Rs = .safe_div(post_s, pre_s)
          )
        })

        probs <- c((1 - conf_level) / 2, 1 - (1 - conf_level) / 2)
        rt_boot <- as.numeric(boot_mat["Rt", ])
        rc_boot <- as.numeric(boot_mat["Rc", ])
        rs_boot <- as.numeric(boot_mat["Rs", ])

        q_rt <- stats::quantile(rt_boot, probs = probs, na.rm = TRUE)
        q_rc <- stats::quantile(rc_boot, probs = probs, na.rm = TRUE)
        q_rs <- stats::quantile(rs_boot, probs = probs, na.rm = TRUE)

        se_rt <- stats::sd(rt_boot, na.rm = TRUE)
        se_rc <- stats::sd(rc_boot, na.rm = TRUE)
        se_rs <- stats::sd(rs_boot, na.rm = TRUE)
        ci_low_rt <- as.numeric(q_rt[[1]])
        ci_high_rt <- as.numeric(q_rt[[2]])
        ci_low_rc <- as.numeric(q_rc[[1]])
        ci_high_rc <- as.numeric(q_rc[[2]])
        ci_low_rs <- as.numeric(q_rs[[1]])
        ci_high_rs <- as.numeric(q_rs[[2]])
        method <- "bootstrap"
      }
    }

    tibble::tibble(
      PreDrought = pre,
      Drought = drought,
      PostDrought = post,
      Rt = rt,
      Rc = rc,
      Rs = rs,
      standard_error = se_rs,
      conf_low = ci_low_rs,
      conf_high = ci_high_rs,
      standard_error_Rt = se_rt,
      standard_error_Rc = se_rc,
      standard_error_Rs = se_rs,
      conf_low_Rt = ci_low_rt,
      conf_high_Rt = ci_high_rt,
      conf_low_Rc = ci_low_rc,
      conf_high_Rc = ci_high_rc,
      conf_low_Rs = ci_low_rs,
      conf_high_Rs = ci_high_rs,
      method = method
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
