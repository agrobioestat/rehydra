#' Plot Drought-Rewatering Trajectories
#'
#' Plot ecophysiological trajectories with phase shading and optional resilience
#' annotations.
#'
#' @param data Segmented data.
#' @param time Time column.
#' @param value Value column.
#' @param id_col Column used for individual lines.
#' @param phase_colors Named vector of colors for phases.
#' @param show_points Logical; show points.
#' @param show_mean Logical; overlay group means.
#' @param show_ci Logical; include standard error ribbon for group means.
#' @param facet_by Faceting columns.
#' @param annotate_indices Logical; annotate Rt and Rc when
#'   `resilience_results` is provided.
#' @param resilience_results Optional output from [resilience_index()].
#'
#' @return A `ggplot2` object.
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
#' plot_recovery_trajectory(segments)
#' @export
plot_recovery_trajectory <- function(
    data,
    time = time,
    value = transformed_value,
    id_col = "plant_id",
    phase_colors = NULL,
    show_points = TRUE,
    show_mean = TRUE,
    show_ci = TRUE,
    facet_by = c("genotype", "variable"),
    annotate_indices = TRUE,
    resilience_results = NULL
) {
  dat <- tibble::as_tibble(data)
  time_col <- .as_col_name(dat, rlang::enquo(time), "time")
  value_col <- .as_col_name(dat, rlang::enquo(value), "value")

  if (!"phase" %in% names(dat)) {
    rlang::abort("`phase` column is required for phase shading.")
  }

  if (is.null(phase_colors)) {
    phase_colors <- c(
      PreDrought = "#e5e5e5",
      Drought = "#f4b6b6",
      Rewatering = "#b9d7f5",
      PostDrought = "#cdeccb",
      RecoveryPlateau = "#d8f2d5"
    )
  }

  dat <- dat |>
    dplyr::mutate(
      .time = suppressWarnings(as.numeric(.data[[time_col]])),
      .value = suppressWarnings(as.numeric(.data[[value_col]]))
    )

  dat <- dat |>
    dplyr::filter(is.finite(.data$.time), is.finite(.data$.value))

  if (nrow(dat) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::labs(
          title = "Drought-Rewatering Trajectory",
          subtitle = "No finite values were available for plotting."
        )
    )
  }

  shade_cols <- intersect(c("genotype", "treatment", "variable", "cycle", "phase"), names(dat))
  shade <- dat |>
    dplyr::group_by(dplyr::across(dplyr::all_of(shade_cols))) |>
    dplyr::summarise(
      xmin = min(.data$.time, na.rm = TRUE),
      xmax = max(.data$.time, na.rm = TRUE),
      .groups = "drop"
    )

  p <- ggplot2::ggplot(dat, ggplot2::aes(x = .data$.time, y = .data$.value))

  p <- p +
    ggplot2::geom_rect(
      data = shade,
      ggplot2::aes(
        xmin = .data$xmin,
        xmax = .data$xmax,
        ymin = -Inf,
        ymax = Inf,
        fill = .data$phase
      ),
      inherit.aes = FALSE,
      alpha = 0.18,
      show.legend = TRUE
    )

  if (show_mean) {
    mean_cols <- intersect(c("genotype", "treatment", "variable", "cycle", ".time"), names(dat))
    mean_df <- dat |>
      dplyr::group_by(dplyr::across(dplyr::all_of(mean_cols))) |>
      dplyr::summarise(
        mean_value = mean(.data$.value, na.rm = TRUE),
        se_value = .safe_div(stats::sd(.data$.value, na.rm = TRUE), sqrt(sum(is.finite(.data$.value)))),
        .groups = "drop"
      )

    if (show_ci) {
      p <- p +
        ggplot2::geom_ribbon(
          data = mean_df,
          ggplot2::aes(
            x = .data$.time,
            ymin = .data$mean_value - .data$se_value,
            ymax = .data$mean_value + .data$se_value,
            group = interaction(.data$genotype, .data$treatment, .data$variable, .data$cycle)
          ),
          inherit.aes = FALSE,
          alpha = 0.20,
          fill = "#4d4d4d"
        )
    }

    p <- p +
      ggplot2::geom_line(
        data = mean_df,
        ggplot2::aes(
          y = .data$mean_value,
          group = interaction(.data$genotype, .data$treatment, .data$variable, .data$cycle)
        ),
        linewidth = 1.1,
        color = "#1b1b1b"
      )
  }

  if (show_points) {
    p <- p +
      ggplot2::geom_point(size = 1.4, alpha = 0.45)
  }

  if (id_col %in% names(dat)) {
    if ("treatment" %in% names(dat)) {
      p <- p +
        ggplot2::geom_line(
          ggplot2::aes(group = .data[[id_col]], color = .data$treatment),
          alpha = 0.35,
          linewidth = 0.6
        )
    } else {
      p <- p +
        ggplot2::geom_line(
          ggplot2::aes(group = .data[[id_col]]),
          alpha = 0.35,
          linewidth = 0.6,
          color = "#3a3a3a"
        )
    }
  }

  if (!is.null(resilience_results) && annotate_indices) {
    res <- tibble::as_tibble(resilience_results)
    ann_cols <- intersect(c("genotype", "treatment", "variable", "cycle", "Rt", "Rc"), names(res))
    if (all(c("Rt", "Rc") %in% ann_cols)) {
      ann <- res |>
        dplyr::select(dplyr::all_of(ann_cols)) |>
        dplyr::mutate(label = paste0("Rt=", round(.data$Rt, 2), ", Rc=", round(.data$Rc, 2)))

      ann_pos <- dat |>
        dplyr::group_by(.data$genotype, .data$treatment, .data$variable, .data$cycle) |>
        dplyr::summarise(
          x = max(.data$.time, na.rm = TRUE),
          y = max(.data$.value, na.rm = TRUE),
          .groups = "drop"
        )
      ann <- ann |>
        dplyr::left_join(ann_pos, by = intersect(c("genotype", "treatment", "variable", "cycle"), names(ann_pos)))

      ann_grp <- intersect(c("genotype", "treatment", "variable"), names(ann))
      if (length(ann_grp) > 0) {
        ann <- ann |>
          dplyr::group_by(dplyr::across(dplyr::all_of(ann_grp))) |>
          dplyr::arrange(.data$cycle, .by_group = TRUE) |>
          dplyr::mutate(y = .data$y - 0.04 * dplyr::row_number() * stats::sd(dat$.value, na.rm = TRUE)) |>
          dplyr::ungroup()
      }

      if (all(c("x", "y", "label") %in% names(ann))) {
        p <- p +
          ggplot2::geom_text(
            data = ann,
            ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
            inherit.aes = FALSE,
            size = 3,
            hjust = 1,
            vjust = 1,
            color = "#202020"
          )
      }
    }
  }

  facet_cols <- intersect(facet_by, names(dat))
  if (length(facet_cols) > 0) {
    facet_formula <- stats::as.formula(paste("~", paste(facet_cols, collapse = "+")))
    p <- p + ggplot2::facet_wrap(facet_formula, scales = "free_y")
  }

  p +
    ggplot2::scale_fill_manual(values = phase_colors, drop = FALSE) +
    ggplot2::labs(
      x = "Time",
      y = value_col,
      title = "Drought-Rewatering Trajectory",
      fill = "Phase",
      color = "Treatment"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

#' Plot Damage, Recovery, and Residual Costs
#'
#' @param damage_results Output from [damage_metrics()].
#' @param recovery_results Output from [recovery_metrics()].
#' @param facet_by Faceting columns.
#'
#' @return A `ggplot2` object.
#' @export
plot_damage_recovery <- function(
    damage_results,
    recovery_results,
    facet_by = c("genotype", "variable")
) {
  dmg <- tibble::as_tibble(damage_results)
  rec <- tibble::as_tibble(recovery_results)

  key <- intersect(c("genotype", "treatment", "replicate", "plant_id", "variable", "cycle"), intersect(names(dmg), names(rec)))

  dat <- dmg |>
    dplyr::left_join(
      rec |>
        dplyr::select(dplyr::any_of(c(key, "recovery_auc", "residual_cost", "recovery_rate"))),
      by = key
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::any_of(c("damage_auc", "recovery_auc", "residual_cost")),
      names_to = "metric",
      values_to = "value"
    )

  dat <- dat |>
    dplyr::filter(is.finite(.data$value))

  if (nrow(dat) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::labs(
          title = "Damage, Recovery, and Residual Cost",
          subtitle = "No finite metric values were available for plotting."
        )
    )
  }

  p <- ggplot2::ggplot(dat, ggplot2::aes(x = .data$metric, y = .data$value, fill = .data$metric)) +
    ggplot2::geom_boxplot(alpha = 0.75, outlier.alpha = 0.4) +
    ggplot2::geom_jitter(width = 0.12, alpha = 0.25) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(x = "Metric", y = "Value", title = "Damage, Recovery, and Residual Cost")

  facet_cols <- intersect(facet_by, names(dat))
  if (length(facet_cols) > 0) {
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", paste(facet_cols, collapse = "+"))), scales = "free_y")
  }

  p
}

#' Plot Memory Effect Metrics
#'
#' @param memory_results Output from [memory_effect()].
#' @param facet_by Faceting columns.
#'
#' @return A `ggplot2` object.
#' @export
plot_memory_effect <- function(memory_results, facet_by = c("treatment", "variable")) {
  dat <- tibble::as_tibble(memory_results)
  mem_cols <- intersect(c("Mem_Rt", "Mem_Rc", "Mem_Rs", "Mem_Damage"), names(dat))

  if (length(mem_cols) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::labs(
          title = "Physiological Memory Across Cycles",
          subtitle = "No memory-effect columns were available for plotting."
        )
    )
  }

  dat <- dat |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(mem_cols),
      names_to = "metric",
      values_to = "value"
    ) |>
    dplyr::filter(is.finite(.data$value))

  if (nrow(dat) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::labs(
          title = "Physiological Memory Across Cycles",
          subtitle = "No finite memory values were available for plotting."
        )
    )
  }

  p <- ggplot2::ggplot(dat, ggplot2::aes(x = .data$metric, y = .data$value, fill = .data$genotype)) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2, color = "#444444") +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), alpha = 0.8) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(x = "Memory metric", y = "Difference", title = "Physiological Memory Across Cycles")

  facet_cols <- intersect(facet_by, names(dat))
  if (length(facet_cols) > 0) {
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", paste(facet_cols, collapse = "+"))), scales = "free_y")
  }

  p
}

#' Plot Resilience Indices
#'
#' @param resilience_results Output from [resilience_index()].
#' @param facet_by Faceting columns.
#'
#' @return A `ggplot2` object.
#' @export
plot_resilience_indices <- function(resilience_results, facet_by = c("treatment", "variable")) {
  dat <- tibble::as_tibble(resilience_results)
  idx_cols <- intersect(c("Rt", "Rc", "Rs"), names(dat))
  if (length(idx_cols) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::labs(
          title = "Resistance, Recovery, and Resilience",
          subtitle = "No resilience-index columns were available for plotting."
        )
    )
  }

  dat <- dat |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(idx_cols),
      names_to = "index",
      values_to = "value"
    ) |>
    dplyr::filter(is.finite(.data$value))

  if (nrow(dat) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::labs(
          title = "Resistance, Recovery, and Resilience",
          subtitle = "No finite resilience-index values were available for plotting."
        )
    )
  }

  p <- ggplot2::ggplot(dat, ggplot2::aes(x = .data$index, y = .data$value, fill = .data$genotype)) +
    ggplot2::geom_boxplot(alpha = 0.8, outlier.alpha = 0.4) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(x = "Index", y = "Value", title = "Resistance, Recovery, and Resilience")

  facet_cols <- intersect(facet_by, names(dat))
  if (length(facet_cols) > 0) {
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", paste(facet_cols, collapse = "+"))), scales = "free_y")
  }

  p
}

#' Plot Stability Metrics
#'
#' @param stability_results Output from [stability_index()].
#' @param facet_by Faceting columns.
#'
#' @return A `ggplot2` object.
#' @export
plot_stability <- function(stability_results, facet_by = c("treatment", "variable")) {
  dat <- tibble::as_tibble(stability_results)
  st_cols <- intersect(c("temporal_cv", "normalized_stability", "memory_stability_gain"), names(dat))
  if (length(st_cols) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::labs(
          title = "Temporal Stability and Memory Gain",
          subtitle = "No stability columns were available for plotting."
        )
    )
  }

  dat <- dat |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(st_cols),
      names_to = "metric",
      values_to = "value"
    ) |>
    dplyr::filter(is.finite(.data$value))

  if (nrow(dat) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::labs(
          title = "Temporal Stability and Memory Gain",
          subtitle = "No finite stability values were available for plotting."
        )
    )
  }

  if (!"cycle" %in% names(dat)) {
    dat$cycle <- 1L
  }

  p <- ggplot2::ggplot(dat, ggplot2::aes(x = .data$cycle, y = .data$value, color = .data$genotype)) +
    ggplot2::geom_line(alpha = 0.65) +
    ggplot2::geom_point(size = 2) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(x = "Cycle", y = "Value", title = "Temporal Stability and Memory Gain")

  facet_cols <- intersect(facet_by, names(dat))
  if (length(facet_cols) == 0) {
    p <- p + ggplot2::facet_wrap(~metric, scales = "free_y")
  } else {
    facet_formula <- stats::as.formula(paste("metric ~", paste(facet_cols, collapse = "+")))
    p <- p + ggplot2::facet_grid(facet_formula, scales = "free_y")
  }

  p
}
