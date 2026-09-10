#' Calculate Drought Memory (Priming) Effects
#'
#' Compare resilience and optional damage/recovery metrics across drought cycles
#' to quantify physiological memory.
#'
#' @param resilience_results Output from [resilience_index()].
#' @param damage_results Optional output from [damage_metrics()].
#' @param recovery_results Optional output from [recovery_metrics()].
#' @param cycle_ref Reference cycle.
#' @param cycle_test Test cycle, or `"previous"`, or `"mean_later"`.
#' @param group_by Grouping columns.
#' @param ci_method Confidence interval method: `"none"` or `"bootstrap"`.
#' @param n_boot Number of bootstrap iterations.
#' @param conf_level Confidence level.
#'
#' @details
#' Core memory metrics:
#'
#' - `Mem_Rt = Rt_test - Rt_ref`
#' - `Mem_Rc = Rc_test - Rc_ref`
#' - `Mem_Rs = Rs_test - Rs_ref`
#' - `Mem_Damage = damage_auc_ref - damage_auc_test`
#' - `Mem_RecRate = recovery_rate_test - recovery_rate_ref`
#' - `Mem_Residual = residual_cost_ref - residual_cost_test`
#' - Relative ratios (`Mem_Ratio_Rt`, `Mem_Ratio_Rc`, `Mem_Ratio_Rs`)
#'
#' Positive values for `Mem_Rt` and `Mem_Rc` indicate improved resistance or
#' recovery in later cycles.
#'
#' @return A tibble with cycle comparison metrics, optional index-specific
#'   interval columns (`conf_low_*`, `conf_high_*`), and interpretation labels.
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
#' res <- resilience_index(segments)
#' memory_effect(res)
#' @export
memory_effect <- function(
    resilience_results,
    damage_results = NULL,
    recovery_results = NULL,
    cycle_ref = 1,
    cycle_test = 2,
    group_by = c("genotype", "treatment", "variable"),
    ci_method = c("none", "bootstrap"),
    n_boot = 999,
    conf_level = 0.95
) {
  ci_method <- match.arg(ci_method)

  res <- tibble::as_tibble(resilience_results)
  req <- c("cycle", "Rt", "Rc", "Rs")
  miss <- setdiff(req, names(res))
  if (length(miss) > 0) {
    rlang::abort(
      paste0(
        "`resilience_results` is missing required columns: ",
        paste(miss, collapse = ", "),
        "."
      )
    )
  }

  grp <- intersect(group_by, names(res))
  if (length(grp) == 0) {
    grp <- character()
  }

  res_group <- res |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(grp, "cycle")))) |>
    dplyr::summarise(
      Rt = mean(.data$Rt, na.rm = TRUE),
      Rc = mean(.data$Rc, na.rm = TRUE),
      Rs = mean(.data$Rs, na.rm = TRUE),
      .groups = "drop"
    )

  get_pairs <- function(df) {
    cycles <- sort(unique(df$cycle))

    if (is.character(cycle_test) && identical(cycle_test, "previous")) {
      pairs <- tibble::tibble(
        cycle_ref = cycles[-length(cycles)],
        cycle_test = cycles[-1]
      )
      return(pairs)
    }

    if (is.character(cycle_test) && identical(cycle_test, "mean_later")) {
      later <- cycles[cycles > cycle_ref]
      if (length(later) == 0) {
        return(tibble::tibble(cycle_ref = integer(), cycle_test = integer()))
      }
      return(tibble::tibble(cycle_ref = cycle_ref, cycle_test = max(later)))
    }

    tests <- as.integer(cycle_test)
    tibble::tibble(
      cycle_ref = as.integer(cycle_ref),
      cycle_test = tests
    )
  }

  compare_one <- function(df, key_vals = NULL) {
    pairs <- get_pairs(df)
    if (nrow(pairs) == 0) {
      return(tibble::tibble())
    }

    out <- lapply(seq_len(nrow(pairs)), function(i) {
      cref <- pairs$cycle_ref[[i]]
      ctest <- pairs$cycle_test[[i]]

      if (is.character(cycle_test) && identical(cycle_test, "mean_later")) {
        ref_row <- df |>
          dplyr::filter(.data$cycle == cref)
        test_rows <- df |>
          dplyr::filter(.data$cycle > cref)

        if (nrow(ref_row) == 0 || nrow(test_rows) == 0) {
          return(NULL)
        }

        rt_ref <- mean(ref_row$Rt, na.rm = TRUE)
        rc_ref <- mean(ref_row$Rc, na.rm = TRUE)
        rs_ref <- mean(ref_row$Rs, na.rm = TRUE)

        rt_test <- mean(test_rows$Rt, na.rm = TRUE)
        rc_test <- mean(test_rows$Rc, na.rm = TRUE)
        rs_test <- mean(test_rows$Rs, na.rm = TRUE)
      } else {
        ref_row <- df |>
          dplyr::filter(.data$cycle == cref)
        test_row <- df |>
          dplyr::filter(.data$cycle == ctest)

        if (nrow(ref_row) == 0 || nrow(test_row) == 0) {
          return(NULL)
        }

        rt_ref <- mean(ref_row$Rt, na.rm = TRUE)
        rc_ref <- mean(ref_row$Rc, na.rm = TRUE)
        rs_ref <- mean(ref_row$Rs, na.rm = TRUE)

        rt_test <- mean(test_row$Rt, na.rm = TRUE)
        rc_test <- mean(test_row$Rc, na.rm = TRUE)
        rs_test <- mean(test_row$Rs, na.rm = TRUE)
      }

      mem_rt <- rt_test - rt_ref
      mem_rc <- rc_test - rc_ref
      mem_rs <- rs_test - rs_ref

      conf_low <- NA_real_
      conf_high <- NA_real_
      conf_low_rt <- NA_real_
      conf_high_rt <- NA_real_
      conf_low_rc <- NA_real_
      conf_high_rc <- NA_real_
      conf_low_rs <- NA_real_
      conf_high_rs <- NA_real_
      if (ci_method == "bootstrap") {
        # Bootstrap from per-cycle replicate rows if available in input.
        raw <- res |>
          dplyr::filter(.data$cycle %in% c(cref, ctest) | .data$cycle > cref)

        if (length(grp) > 0) {
          keys <- key_vals
          if (is.null(keys)) {
            keys <- df[1, grp, drop = FALSE]
          }
          for (nm in names(keys)) {
            raw <- raw |>
              dplyr::filter(.data[[nm]] == keys[[nm]])
          }
        }

        ref_vals <- raw$Rt[raw$cycle == cref]
        if (is.character(cycle_test) && identical(cycle_test, "mean_later")) {
          test_vals_rt <- raw$Rt[raw$cycle > cref]
          test_vals_rc <- raw$Rc[raw$cycle > cref]
          test_vals_rs <- raw$Rs[raw$cycle > cref]
        } else {
          test_vals_rt <- raw$Rt[raw$cycle == ctest]
          test_vals_rc <- raw$Rc[raw$cycle == ctest]
          test_vals_rs <- raw$Rs[raw$cycle == ctest]
        }
        ref_vals_rc <- raw$Rc[raw$cycle == cref]
        ref_vals_rs <- raw$Rs[raw$cycle == cref]

        if (length(ref_vals) > 1 && length(test_vals_rt) > 1 &&
            length(ref_vals_rc) > 1 && length(test_vals_rc) > 1 &&
            length(ref_vals_rs) > 1 && length(test_vals_rs) > 1) {
          boot_rt <- replicate(n_boot, mean(sample(test_vals_rt, replace = TRUE)) - mean(sample(ref_vals, replace = TRUE)))
          boot_rc <- replicate(n_boot, mean(sample(test_vals_rc, replace = TRUE)) - mean(sample(ref_vals_rc, replace = TRUE)))
          boot_rs <- replicate(n_boot, mean(sample(test_vals_rs, replace = TRUE)) - mean(sample(ref_vals_rs, replace = TRUE)))
          probs <- c((1 - conf_level) / 2, 1 - (1 - conf_level) / 2)
          q_rt <- stats::quantile(boot_rt, probs = probs, na.rm = TRUE)
          q_rc <- stats::quantile(boot_rc, probs = probs, na.rm = TRUE)
          q_rs <- stats::quantile(boot_rs, probs = probs, na.rm = TRUE)
          conf_low_rt <- as.numeric(q_rt[[1]])
          conf_high_rt <- as.numeric(q_rt[[2]])
          conf_low_rc <- as.numeric(q_rc[[1]])
          conf_high_rc <- as.numeric(q_rc[[2]])
          conf_low_rs <- as.numeric(q_rs[[1]])
          conf_high_rs <- as.numeric(q_rs[[2]])
          conf_low <- conf_low_rt
          conf_high <- conf_high_rt
        }
      }

      tibble::tibble(
        cycle_ref = cref,
        cycle_test = ctest,
        Rt_ref = rt_ref,
        Rt_test = rt_test,
        Mem_Rt = mem_rt,
        Rc_ref = rc_ref,
        Rc_test = rc_test,
        Mem_Rc = mem_rc,
        Rs_ref = rs_ref,
        Rs_test = rs_test,
        Mem_Rs = mem_rs,
        Mem_Ratio_Rt = .safe_div(rt_test, rt_ref),
        Mem_Ratio_Rc = .safe_div(rc_test, rc_ref),
        Mem_Ratio_Rs = .safe_div(rs_test, rs_ref),
        conf_low = conf_low,
        conf_high = conf_high,
        conf_low_Rt = conf_low_rt,
        conf_high_Rt = conf_high_rt,
        conf_low_Rc = conf_low_rc,
        conf_high_Rc = conf_high_rc,
        conf_low_Rs = conf_low_rs,
        conf_high_Rs = conf_high_rs,
        method = ci_method
      )
    })

    dplyr::bind_rows(out)
  }

  memory_tbl <- if (length(grp) == 0) {
    compare_one(res_group)
  } else {
    res_group |>
      dplyr::group_by(dplyr::across(dplyr::all_of(grp))) |>
      dplyr::group_modify(~compare_one(.x, key_vals = .y)) |>
      dplyr::ungroup()
  }

  if (nrow(memory_tbl) == 0) {
    return(memory_tbl)
  }

  if (!is.null(damage_results)) {
    damage_tbl <- tibble::as_tibble(damage_results)
    if ("damage_auc" %in% names(damage_tbl) && "cycle" %in% names(damage_tbl)) {
      damage_avg <- damage_tbl |>
        dplyr::group_by(dplyr::across(dplyr::all_of(intersect(c(grp, "cycle"), names(damage_tbl))))) |>
        dplyr::summarise(damage_auc = mean(.data$damage_auc, na.rm = TRUE), .groups = "drop")

      key_ref <- c(intersect(grp, names(damage_avg)), "cycle_ref")
      key_test <- c(intersect(grp, names(damage_avg)), "cycle_test")

      damage_ref <- damage_avg |>
        dplyr::rename(cycle_ref = "cycle", damage_auc_ref = "damage_auc")
      damage_test <- damage_avg |>
        dplyr::rename(cycle_test = "cycle", damage_auc_test = "damage_auc")

      memory_tbl <- memory_tbl |>
        dplyr::left_join(damage_ref, by = key_ref) |>
        dplyr::left_join(damage_test, by = key_test) |>
        dplyr::mutate(Mem_Damage = .data$damage_auc_ref - .data$damage_auc_test) |>
        dplyr::select(-dplyr::any_of(c("damage_auc_ref", "damage_auc_test")))
    }
  }

  if (!is.null(recovery_results)) {
    rec_tbl <- tibble::as_tibble(recovery_results)
    if (all(c("recovery_rate", "residual_cost", "cycle") %in% names(rec_tbl))) {
      rec_avg <- rec_tbl |>
        dplyr::group_by(dplyr::across(dplyr::all_of(intersect(c(grp, "cycle"), names(rec_tbl))))) |>
        dplyr::summarise(
          recovery_rate = mean(.data$recovery_rate, na.rm = TRUE),
          residual_cost = mean(.data$residual_cost, na.rm = TRUE),
          .groups = "drop"
        )

      key_ref <- c(intersect(grp, names(rec_avg)), "cycle_ref")
      key_test <- c(intersect(grp, names(rec_avg)), "cycle_test")

      rec_ref <- rec_avg |>
        dplyr::rename(
          cycle_ref = "cycle",
          recovery_rate_ref = "recovery_rate",
          residual_cost_ref = "residual_cost"
        )
      rec_test <- rec_avg |>
        dplyr::rename(
          cycle_test = "cycle",
          recovery_rate_test = "recovery_rate",
          residual_cost_test = "residual_cost"
        )

      memory_tbl <- memory_tbl |>
        dplyr::left_join(rec_ref, by = key_ref) |>
        dplyr::left_join(rec_test, by = key_test) |>
        dplyr::mutate(
          Mem_RecRate = .data$recovery_rate_test - .data$recovery_rate_ref,
          Mem_Residual = .data$residual_cost_ref - .data$residual_cost_test
        ) |>
        dplyr::select(-dplyr::any_of(c(
          "recovery_rate_ref", "recovery_rate_test",
          "residual_cost_ref", "residual_cost_test"
        )))
    }
  }

  if (!"Mem_Damage" %in% names(memory_tbl)) memory_tbl$Mem_Damage <- NA_real_
  if (!"Mem_RecRate" %in% names(memory_tbl)) memory_tbl$Mem_RecRate <- NA_real_
  if (!"Mem_Residual" %in% names(memory_tbl)) memory_tbl$Mem_Residual <- NA_real_

  memory_tbl |>
    dplyr::mutate(
      interpretation = dplyr::case_when(
        .data$Mem_Rt > 0 & .data$Mem_Rc >= 0 ~ "positive_priming_signal",
        .data$Mem_Rt < 0 | .data$Mem_Rc < 0 ~ "negative_memory_signal",
        TRUE ~ "neutral_memory_signal"
      )
    )
}

#' Classify Priming Response Types
#'
#' Assign categorical priming classes based on memory and recovery metrics.
#'
#' @param memory_results Output from [memory_effect()].
#' @param threshold Minimum absolute change used to detect memory shifts.
#' @param recovery_threshold Threshold for incomplete recovery classification.
#'
#' @return Input tibble with `priming_class` appended.
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
#' res <- resilience_index(segments)
#' mem <- memory_effect(res)
#' priming_classification(mem)
#' @export
priming_classification <- function(
    memory_results,
    threshold = 0.05,
    recovery_threshold = 0.90
) {
  dat <- tibble::as_tibble(memory_results)

  required <- c("Mem_Rt", "Mem_Rc", "Rs_test")
  if (!all(required %in% names(dat))) {
    dat$priming_class <- NA_character_
    return(dat)
  }

  dat |>
    dplyr::mutate(
      priming_class = dplyr::case_when(
        is.na(.data$Rs_test) | is.na(.data$Mem_Rt) | is.na(.data$Mem_Rc) ~ NA_character_,
        .data$Rs_test > 1 ~ "overcompensation",
        .data$Rs_test < recovery_threshold ~ "incomplete_recovery",
        .data$Mem_Rt > threshold & .data$Mem_Rc >= 0 ~ "positive_priming",
        .data$Mem_Rt < -threshold | .data$Mem_Rc < -threshold ~ "negative_priming",
        abs(.data$Mem_Rt) <= threshold & abs(.data$Mem_Rc) <= threshold ~ "neutral_memory",
        TRUE ~ "maladaptation"
      )
    )
}
