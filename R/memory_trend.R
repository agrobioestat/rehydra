#' Between-Cycle Memory Trend Across Three or More Cycles
#'
#' Quantify how an index changes as drought-rewatering cycles accumulate.
#' [memory_effect()] compares two cycles at a time; `memory_trend()` fits the
#' whole sequence and answers whether memory is being *built up*, *eroded* or is
#' *stable* over repeated exposure.
#'
#' @param indices A per-cycle index table with a `cycle` column, typically the
#'   output of [resilience_index()], [curve_shape_metrics()],
#'   [recovery_period_metrics()] or [mean_reduction_metrics()].
#' @param metrics Character vector of numeric columns to analyse. Defaults to
#'   whichever of `Rt`, `Rc`, `Rs` are present.
#' @param group_by Grouping columns defining an independent memory trajectory.
#' @param min_cycles Minimum number of distinct cycles required to fit a trend.
#' @param conf_level Confidence level for the slope interval.
#' @param p_adjust Method passed to [stats::p.adjust()] for the slope p-values,
#'   applied across metrics within each group.
#' @param alpha Significance level used by the `memory_class` label.
#'
#' @details
#' For each group and metric the index values of the individual replicates are
#' regressed on the cycle number,
#'
#' \deqn{y_{ij} = \beta_0 + \beta_1 c_i + \varepsilon_{ij},}
#'
#' where \eqn{c_i} is the cycle index and \eqn{j} runs over replicates. The
#' slope \eqn{\beta_1} is the **memory rate**: the average change in the index
#' per additional drought cycle. Fitting on the replicate-level rows rather than
#' on the per-cycle means keeps the residual degrees of freedom that the
#' experiment actually has, so the standard error and the p-value are honest;
#' regressing three cycle means on three cycle numbers would leave one degree of
#' freedom and produce a p-value that means nothing.
#'
#' Three complementary numbers are reported because a slope alone can be
#' misleading:
#'
#' - `slope` with its interval and p-value: the average per-cycle change.
#' - `cumulative_change` = value in the last cycle minus value in the first:
#'   the total memory accumulated, which is what a physiologist reports.
#' - `monotonic_fraction`: the proportion of consecutive cycle-to-cycle steps
#'   that move in the same direction as the slope. A significant slope with a
#'   `monotonic_fraction` near `0.5` is being driven by the endpoints rather
#'   than by a consistent progression, which usually means one anomalous cycle,
#'   not memory.
#'
#' `slope_relative` divides the slope by the mean level of the index, giving a
#' per-cycle proportional change that is comparable between metrics measured on
#' different scales (a slope of `0.02` means one thing for `Rs` and another for
#' `deficit_auc`).
#'
#' The `memory_class` label combines direction and significance:
#' `"acquired_memory"` (index improving significantly across cycles),
#' `"eroded_memory"` (index deteriorating significantly), `"stable"` (no
#' significant change) and `"undetermined"` (too few cycles or a degenerate
#' fit). For metrics where a *lower* value is better — `deficit_auc`,
#' `total_reduction`, `mean_reduction`, `t50`, `residual_deficit` and the like —
#' the direction is flipped automatically, so `"acquired_memory"` always means
#' "the plant handled the later cycles better". Pass `metrics` explicitly with
#' [memory_direction()] if you need to override that mapping.
#'
#' @return A tibble with one row per group and metric, with columns for the
#'   grouping variables plus `metric`, `n_cycles`, `n_obs`, `first_cycle`,
#'   `last_cycle`, `first_value`, `last_value`, `cumulative_change`, `slope`,
#'   `slope_se`, `slope_conf_low`, `slope_conf_high`, `slope_relative`,
#'   `statistic`, `p_value`, `p_value_adj`, `r_squared`, `monotonic_fraction`,
#'   `lower_is_better` and `memory_class`.
#'
#' @references
#' Ribeiro et al. (2021) <doi:10.1016/j.jplph.2021.153397>; Lloret et al. (2011)
#' <doi:10.1111/j.1600-0706.2011.19372.x>.
#'
#' @seealso [memory_effect()] for pairwise cycle contrasts,
#'   [plot_memory_trend()] for the figure.
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
#'
#' # The bundled example has two cycles, so min_cycles must be lowered.
#' memory_trend(res, min_cycles = 2)
#' @export
memory_trend <- function(
    indices,
    metrics = NULL,
    group_by = c("genotype", "treatment", "variable"),
    min_cycles = 3,
    conf_level = 0.95,
    p_adjust = "holm",
    alpha = 0.05
) {
  dat <- tibble::as_tibble(indices)

  if (!"cycle" %in% names(dat)) {
    rlang::abort("`indices` must contain a `cycle` column.")
  }

  if (is.null(metrics)) {
    metrics <- intersect(c("Rt", "Rc", "Rs"), names(dat))
  }
  metrics <- intersect(metrics, names(dat))
  metrics <- metrics[vapply(dat[metrics], is.numeric, logical(1))]

  if (length(metrics) == 0) {
    rlang::abort(
      "No numeric metric column was found. Pass `metrics` explicitly, for example metrics = \"Rs\"."
    )
  }

  grp <- intersect(group_by, names(dat))

  fit_one <- function(tbl, metric) {
    y <- suppressWarnings(as.numeric(tbl[[metric]]))
    cyc <- suppressWarnings(as.numeric(tbl$cycle))

    ok <- is.finite(y) & is.finite(cyc)
    y <- y[ok]
    cyc <- cyc[ok]

    lower_better <- .memory_lower_is_better(metric)
    empty <- tibble::tibble(
      metric = metric,
      n_cycles = dplyr::n_distinct(cyc),
      n_obs = length(y),
      first_cycle = NA_real_, last_cycle = NA_real_,
      first_value = NA_real_, last_value = NA_real_,
      cumulative_change = NA_real_,
      slope = NA_real_, slope_se = NA_real_,
      slope_conf_low = NA_real_, slope_conf_high = NA_real_,
      slope_relative = NA_real_,
      statistic = NA_real_, p_value = NA_real_,
      r_squared = NA_real_, monotonic_fraction = NA_real_,
      lower_is_better = lower_better,
      memory_class = "undetermined"
    )

    if (length(y) < 3L || dplyr::n_distinct(cyc) < min_cycles) {
      return(empty)
    }

    cycle_means <- tapply(y, cyc, mean, na.rm = TRUE)
    cycles_sorted <- sort(unique(cyc))
    means_sorted <- as.numeric(cycle_means[as.character(cycles_sorted)])

    # An index that does not vary at all across cycles is stable by definition.
    # Regressing it would produce a zero residual variance, an "essentially
    # perfect fit" warning and a NaN p-value, so the case is answered directly.
    if (isTRUE(all.equal(max(y) - min(y), 0))) {
      out <- empty
      out$first_cycle <- cycles_sorted[[1]]
      out$last_cycle <- cycles_sorted[[length(cycles_sorted)]]
      out$first_value <- means_sorted[[1]]
      out$last_value <- means_sorted[[length(means_sorted)]]
      out$cumulative_change <- 0
      out$slope <- 0
      out$monotonic_fraction <- NA_real_
      out$memory_class <- "stable"
      return(out)
    }

    fit <- suppressWarnings(tryCatch(stats::lm(y ~ cyc), error = function(e) NULL))
    if (is.null(fit) || nrow(suppressWarnings(stats::coef(summary(fit)))) < 2L) {
      return(empty)
    }

    cf <- suppressWarnings(stats::coef(summary(fit)))
    slope <- cf[2L, 1L]
    slope_se <- cf[2L, 2L]
    statistic <- cf[2L, 3L]
    p_value <- cf[2L, 4L]

    ci <- suppressWarnings(tryCatch(
      stats::confint(fit, parm = "cyc", level = conf_level),
      error = function(e) matrix(NA_real_, 1, 2)
    ))

    steps <- diff(means_sorted)
    expected_sign <- sign(slope)
    monotonic <- if (length(steps) == 0 || expected_sign == 0) {
      NA_real_
    } else {
      mean(sign(steps) == expected_sign)
    }

    mean_level <- mean(y, na.rm = TRUE)

    # A "better later cycle" means a larger index for most metrics but a smaller
    # one for damage-type metrics, so the direction is flipped before labelling.
    improving <- if (lower_better) slope < 0 else slope > 0

    memory_class <- if (!is.finite(p_value)) {
      "undetermined"
    } else if (p_value < alpha && improving) {
      "acquired_memory"
    } else if (p_value < alpha) {
      "eroded_memory"
    } else {
      "stable"
    }

    tibble::tibble(
      metric = metric,
      n_cycles = length(cycles_sorted),
      n_obs = length(y),
      first_cycle = cycles_sorted[[1]],
      last_cycle = cycles_sorted[[length(cycles_sorted)]],
      first_value = means_sorted[[1]],
      last_value = means_sorted[[length(means_sorted)]],
      cumulative_change = means_sorted[[length(means_sorted)]] - means_sorted[[1]],
      slope = slope,
      slope_se = slope_se,
      slope_conf_low = as.numeric(ci[1L, 1L]),
      slope_conf_high = as.numeric(ci[1L, 2L]),
      slope_relative = .safe_div(slope, abs(mean_level)),
      statistic = statistic,
      p_value = p_value,
      r_squared = suppressWarnings(summary(fit)$r.squared),
      monotonic_fraction = monotonic,
      lower_is_better = lower_better,
      memory_class = memory_class
    )
  }

  fit_group <- function(tbl) {
    out <- dplyr::bind_rows(lapply(metrics, function(m) fit_one(tbl, m)))
    out$p_value_adj <- stats::p.adjust(out$p_value, method = p_adjust)
    out
  }

  res <- if (length(grp) == 0) {
    fit_group(dat)
  } else {
    dat |>
      dplyr::group_by(dplyr::across(dplyr::all_of(grp))) |>
      dplyr::group_modify(~fit_group(.x)) |>
      dplyr::ungroup()
  }

  dplyr::relocate(res, dplyr::any_of("p_value_adj"), .after = dplyr::any_of("p_value"))
}

#' Direction of Improvement for an Index
#'
#' Report whether a lower value of a given rehydra index means a better
#' physiological outcome. Used by [memory_trend()] to sign its memory classes.
#'
#' @param metric Character vector of metric names.
#'
#' @return A logical vector, `TRUE` where a lower value is better.
#'
#' @examples
#' memory_direction(c("Rs", "deficit_auc", "t50"))
#' @export
memory_direction <- function(metric) {
  vapply(as.character(metric), .memory_lower_is_better, logical(1),
         USE.NAMES = FALSE)
}

# Damage-type indices: larger means worse. Matched on whole names first, then on
# informative fragments so that derived columns (`deficit_auc_normalized`,
# `mean_reduction_absolute`, ...) inherit the right direction.
.memory_lower_is_better <- function(metric) {
  nm <- tolower(as.character(metric)[[1]])

  exact_lower <- c(
    "t50", "t50_damage", "latency", "damage_latency", "lag_to_minimum",
    "recovery_period", "reduction_period", "time_to_damage", "time_to_recovery",
    "time_to_minimum", "residual_cost", "residual_deficit", "temporal_cv"
  )
  if (nm %in% exact_lower) {
    return(TRUE)
  }

  fragments_lower <- c("deficit", "reduction", "damage", "decline")
  any(vapply(fragments_lower, function(f) grepl(f, nm, fixed = TRUE), logical(1)))
}
