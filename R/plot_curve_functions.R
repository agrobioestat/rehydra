#' Plot the Time Series with the Deficit Area Highlighted
#'
#' Draw the full trajectory together with the pre-drought baseline and shade the
#' area between them: the area under the deficit curve that
#' [curve_shape_metrics()] integrates. The trough, the rewatering onset and the
#' time to half recovery are marked so the numeric indices can be read straight
#' off the figure.
#'
#' @param data Segmented data from [segment_drought_cycle()].
#' @param time Time column.
#' @param value Value column.
#' @param group_by Grouping columns used to define one deficit curve.
#' @param facet_by Faceting columns.
#' @param baseline_summary How the baseline is summarized: `"mean"`,
#'   `"median"` or `"last"`.
#' @param show_landmarks Logical; mark the trough, the rewatering onset and
#'   `t50`.
#' @param shape_results Optional pre-computed [curve_shape_metrics()] output.
#'   Supplying it avoids recomputing the landmarks.
#' @param fill Fill colour of the shaded deficit area.
#'
#' @return A `ggplot2` object.
#'
#' @seealso [curve_shape_metrics()], [deficit_curve()].
#'
#' @examples
#' data(rehydra_data)
#' prepared <- prepare_rehydra_data(
#'   data = rehydra_data,
#'   time = time_days,
#'   genotype = genotype,
#'   treatment = treatment,
#'   replicate = replicate,
#'   variables = stomatal_conductance
#' )
#' segments <- segment_drought_cycle(prepared, time = time, value = transformed_value)
#'
#' drought_only <- segments[segments$treatment == "Drought" &
#'   segments$replicate == "1", ]
#' plot_deficit_auc(drought_only)
#' @export
plot_deficit_auc <- function(
    data,
    time = time,
    value = transformed_value,
    group_by = c("genotype", "treatment", "replicate", "plant_id", "variable", "cycle"),
    facet_by = c("genotype", "variable"),
    baseline_summary = c("mean", "median", "last"),
    show_landmarks = TRUE,
    shape_results = NULL,
    fill = "#d98b8b"
) {
  baseline_summary <- match.arg(baseline_summary)

  curve <- tryCatch(
    deficit_curve(
      data = data,
      time = {{ time }},
      value = {{ value }},
      group_by = group_by,
      baseline_summary = baseline_summary
    ),
    error = function(e) NULL
  )

  if (is.null(curve) || nrow(curve) == 0) {
    return(.empty_plot("Area Under the Deficit Curve",
                       "No finite values were available for plotting."))
  }

  curve <- dplyr::filter(curve, is.finite(.data$time), is.finite(.data$value))
  if (nrow(curve) == 0) {
    return(.empty_plot("Area Under the Deficit Curve",
                       "No finite values were available for plotting."))
  }

  group_cols <- intersect(group_by, names(curve))
  curve$.series <- if (length(group_cols) > 0) {
    do.call(paste, c(unname(as.list(curve[group_cols])), list(sep = " | ")))
  } else {
    "series"
  }

  p <- ggplot2::ggplot(curve, ggplot2::aes(x = .data$time, y = .data$value)) +
    # The shaded band is exactly the region integrated by `deficit_auc`: it is
    # bounded above by the baseline and below by the trajectory, and it closes
    # wherever the trajectory rises back above the baseline.
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = .data$value,
        ymax = pmax(.data$value, .data$baseline),
        group = .data$.series
      ),
      fill = fill,
      alpha = 0.45
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$baseline, group = .data$.series),
      linetype = 2,
      colour = "#3c5466"
    ) +
    ggplot2::geom_line(ggplot2::aes(group = .data$.series), linewidth = 0.7) +
    ggplot2::geom_point(size = 1.1, alpha = 0.8) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(
      x = "Time",
      y = "Response",
      title = "Area Under the Deficit Curve",
      subtitle = "Shaded area = integral of max(0, baseline - response); dashed line = baseline"
    )

  if (isTRUE(show_landmarks)) {
    shape <- shape_results
    if (is.null(shape)) {
      shape <- tryCatch(
        curve_shape_metrics(
          data = data,
          time = {{ time }},
          value = {{ value }},
          group_by = group_by,
          baseline_summary = baseline_summary,
          fit_rate = FALSE
        ),
        error = function(e) NULL
      )
    }

    if (!is.null(shape) && nrow(shape) > 0) {
      marks <- shape |>
        dplyr::filter(is.finite(.data$time_minimum), is.finite(.data$drought_minimum))

      if (nrow(marks) > 0) {
        p <- p +
          ggplot2::geom_point(
            data = marks,
            ggplot2::aes(x = .data$time_minimum, y = .data$drought_minimum),
            inherit.aes = FALSE,
            shape = 4, size = 2.6, stroke = 1.1, colour = "#8c2f39"
          )
      }

      t50_marks <- marks |>
        dplyr::filter(is.finite(.data$t50)) |>
        dplyr::mutate(
          .t50_time = .data$time_minimum + .data$t50,
          .t50_value = .data$drought_minimum + 0.5 * .data$deficit_amplitude
        )

      if (nrow(t50_marks) > 0) {
        p <- p +
          ggplot2::geom_point(
            data = t50_marks,
            ggplot2::aes(x = .data$.t50_time, y = .data$.t50_value),
            inherit.aes = FALSE,
            shape = 21, size = 2.4, fill = "#ffffff", colour = "#1f6f5d", stroke = 1.1
          )
      }
    }
  }

  .add_facets(p, facet_by, curve)
}

#' Plot Curve-Shape Metrics
#'
#' Summarize the [curve_shape_metrics()] output as one panel per metric, so the
#' area, the speed and the latency of the response can be compared side by side
#' across genotypes and cycles.
#'
#' @param shape_results Output from [curve_shape_metrics()].
#' @param metrics Metric columns to display.
#' @param facet_by Faceting columns.
#' @param drop_undetected Logical; drop rows where no stress was detected
#'   (control series), which otherwise dominate the scale with noise.
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
#'   variables = water_potential
#' )
#' segments <- segment_drought_cycle(prepared, time = time, value = transformed_value)
#' plot_curve_shape(curve_shape_metrics(segments))
#' @export
plot_curve_shape <- function(
    shape_results,
    metrics = c("deficit_auc_normalized", "t50", "recovery_rate_k", "latency"),
    facet_by = "variable",
    drop_undetected = TRUE
) {
  dat <- tibble::as_tibble(shape_results)

  if (isTRUE(drop_undetected) && "stress_detected" %in% names(dat)) {
    dat <- dplyr::filter(dat, .data$stress_detected %in% TRUE)
  }

  long <- .metrics_to_long(dat, metrics)
  if (is.null(long)) {
    return(.empty_plot("Damage and Recovery Curve Shape",
                       "No finite curve-shape values were available for plotting."))
  }

  fill_var <- intersect(c("genotype", "treatment"), names(long))[1]

  p <- ggplot2::ggplot(
    long,
    ggplot2::aes(
      x = factor(.data$cycle),
      y = .data$value,
      fill = if (is.na(fill_var)) NULL else .data[[fill_var]]
    )
  ) +
    ggplot2::geom_boxplot(alpha = 0.75, outlier.size = 0.9) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(
      x = "Cycle",
      y = "Value",
      fill = fill_var,
      title = "Damage and Recovery Curve Shape"
    )

  facet_cols <- c("metric", intersect(facet_by, names(long)))
  p + ggplot2::facet_wrap(
    stats::as.formula(paste("~", paste(facet_cols, collapse = "+"))),
    scales = "free_y"
  )
}

#' Plot the Period and Reduction Components
#'
#' @param period_results Output from [recovery_period_metrics()].
#' @param reduction_results Output from [mean_reduction_metrics()].
#' @param facet_by Faceting columns.
#' @param drop_undetected Logical; drop rows where no stress was detected.
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
#'   variables = water_potential
#' )
#' segments <- segment_drought_cycle(prepared, time = time, value = transformed_value)
#' plot_extended_indices(recovery_period_metrics(segments), mean_reduction_metrics(segments))
#' @export
plot_extended_indices <- function(
    period_results,
    reduction_results = NULL,
    facet_by = "variable",
    drop_undetected = TRUE
) {
  key <- c("genotype", "treatment", "replicate", "plant_id", "variable", "cycle")

  dat <- tibble::as_tibble(period_results)
  period_cols <- intersect(
    c("total_reduction_relative", "recovery_period", "reduction_period"),
    names(dat)
  )
  dat <- dplyr::select(dat, dplyr::any_of(c(key, "stress_detected", period_cols)))

  if (!is.null(reduction_results)) {
    sw <- tibble::as_tibble(reduction_results)
    sw_cols <- intersect(
      c("mean_reduction", "max_reduction", "mean_recovery_rate_relative"),
      names(sw)
    )
    join_key <- intersect(key, intersect(names(dat), names(sw)))
    if (length(join_key) > 0 && length(sw_cols) > 0) {
      dat <- dplyr::left_join(
        dat,
        dplyr::select(sw, dplyr::all_of(c(join_key, sw_cols))),
        by = join_key
      )
    }
  }

  if (isTRUE(drop_undetected) && "stress_detected" %in% names(dat)) {
    dat <- dplyr::filter(dat, .data$stress_detected %in% TRUE)
  }

  metrics <- setdiff(names(dat), c(key, "stress_detected"))
  long <- .metrics_to_long(dat, metrics)

  if (is.null(long)) {
    return(.empty_plot("Period and Reduction Components",
                       "No finite index values were available for plotting."))
  }

  fill_var <- intersect(c("genotype", "treatment"), names(long))[1]

  p <- ggplot2::ggplot(
    long,
    ggplot2::aes(
      x = factor(.data$cycle),
      y = .data$value,
      fill = if (is.na(fill_var)) NULL else .data[[fill_var]]
    )
  ) +
    ggplot2::geom_boxplot(alpha = 0.75, outlier.size = 0.9) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(
      x = "Cycle",
      y = "Value",
      fill = fill_var,
      title = "Period and Reduction Components"
    )

  facet_cols <- c("metric", intersect(facet_by, names(long)))
  p + ggplot2::facet_wrap(
    stats::as.formula(paste("~", paste(facet_cols, collapse = "+"))),
    scales = "free_y"
  )
}

#' Plot the Between-Cycle Memory Trend
#'
#' Show each index against the cycle number with the fitted memory slope, the
#' quantity [memory_trend()] tests.
#'
#' @param indices A per-cycle index table, as passed to [memory_trend()].
#' @param trend_results Optional pre-computed [memory_trend()] output, used to
#'   colour the panels by memory class.
#' @param metrics Metric columns to plot.
#' @param group_by Grouping columns.
#' @param facet_by Faceting columns.
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
#'   variables = water_potential
#' )
#' segments <- segment_drought_cycle(prepared, time = time, value = transformed_value)
#' res <- resilience_index(segments)
#' plot_memory_trend(res, memory_trend(res, min_cycles = 2))
#' @export
plot_memory_trend <- function(
    indices,
    trend_results = NULL,
    metrics = NULL,
    group_by = c("genotype", "treatment", "variable"),
    facet_by = c("variable")
) {
  dat <- tibble::as_tibble(indices)

  if (!"cycle" %in% names(dat)) {
    return(.empty_plot("Between-Cycle Memory Trend",
                       "A `cycle` column is required."))
  }

  if (is.null(metrics)) {
    metrics <- intersect(c("Rt", "Rc", "Rs"), names(dat))
  }

  long <- .metrics_to_long(dat, metrics)
  if (is.null(long)) {
    return(.empty_plot("Between-Cycle Memory Trend",
                       "No finite index values were available for plotting."))
  }

  colour_var <- intersect(c("genotype", "treatment"), names(long))[1]

  p <- ggplot2::ggplot(
    long,
    ggplot2::aes(
      x = .data$cycle,
      y = .data$value,
      colour = if (is.na(colour_var)) NULL else .data[[colour_var]]
    )
  ) +
    ggplot2::geom_point(alpha = 0.55, size = 1.6,
                        position = ggplot2::position_jitter(width = 0.06, height = 0)) +
    # The line is the same simple regression on cycle that memory_trend() fits,
    # so the figure and the table cannot disagree.
    ggplot2::stat_summary(fun = mean, geom = "point", size = 2.8, shape = 18) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                         linewidth = 0.7, alpha = 0.15) +
    ggplot2::scale_x_continuous(breaks = sort(unique(long$cycle))) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(
      x = "Cycle",
      y = "Index value",
      colour = colour_var,
      title = "Between-Cycle Memory Trend",
      subtitle = "Slope of the fitted line = average change in the index per additional cycle"
    )

  if (!is.null(trend_results) && nrow(trend_results) > 0) {
    tr <- tibble::as_tibble(trend_results)
    key <- intersect(c(group_by, "metric"), intersect(names(tr), names(long)))
    if ("memory_class" %in% names(tr) && length(key) > 0) {
      labels <- tr |>
        dplyr::filter(!is.na(.data$memory_class)) |>
        dplyr::distinct(dplyr::across(dplyr::all_of(c(key, "memory_class"))))
      attr(p, "rehydra_memory_class") <- labels
    }
  }

  facet_cols <- c("metric", intersect(facet_by, names(long)))
  p + ggplot2::facet_wrap(
    stats::as.formula(paste("~", paste(facet_cols, collapse = "+"))),
    scales = "free_y"
  )
}

# ---- internal plot helpers ---------------------------------------------------

.empty_plot <- function(title, subtitle) {
  ggplot2::ggplot() +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(title = title, subtitle = subtitle)
}

.add_facets <- function(p, facet_by, data) {
  facet_cols <- intersect(facet_by, names(data))
  if (length(facet_cols) == 0) {
    return(p)
  }
  p + ggplot2::facet_wrap(
    stats::as.formula(paste("~", paste(facet_cols, collapse = "+"))),
    scales = "free_y"
  )
}

.metrics_to_long <- function(data, metrics) {
  dat <- tibble::as_tibble(data)
  metrics <- intersect(metrics, names(dat))
  metrics <- metrics[vapply(dat[metrics], is.numeric, logical(1))]

  if (length(metrics) == 0 || nrow(dat) == 0) {
    return(NULL)
  }

  if (!"cycle" %in% names(dat)) {
    dat$cycle <- 1L
  }

  long <- dat |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(metrics),
      names_to = "metric",
      values_to = "value"
    ) |>
    dplyr::filter(is.finite(.data$value))

  if (nrow(long) == 0) {
    return(NULL)
  }

  long$metric <- factor(long$metric, levels = metrics)
  long
}
