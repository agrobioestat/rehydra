#' Overall Stability Index and Its Components
#'
#' Compute the six drought-stability indices of Ribeiro et al. (2021): impact,
#' integrated impact, disturbance rate, perturbation, recovery rate, and the
#' overall stability index `OSt` that combines the last two.
#'
#' @param data Segmented data from [segment_drought_cycle()].
#' @param time Time column.
#' @param value Value column used for the calculations.
#' @param group_by Grouping columns.
#' @param baseline_summary How the pre-drought reference is summarized:
#'   `"mean"`, `"median"` or `"last"`.
#' @param percent Logical; report `impact`, `integrated_impact` and
#'   `perturbation` as percentages (the convention used in the source) rather
#'   than as fractions.
#' @param min_points Minimum number of observations required in a window before
#'   its regression slope is estimated.
#'
#' @details
#' Let `B` be the reference level measured before the stress, `y(t)` the
#' trajectory, `t_str` the onset of the stress, `t_min` the time of the trough
#' with value `v_min`, and `t_end` the last observation of the cycle.
#'
#' ## The two rates, and why they are regression slopes
#'
#' \deqn{DR = -\,\mathrm{slope}\big(y \sim t;\; t \in [t_{str},\, t_{min}]\big),
#'   \qquad
#'   RR = \mathrm{slope}\big(y \sim t;\; t \in [t_{min},\, t_{end}]\big).}
#'
#' `DR` is the rate at which the trait is driven down by the water deficit and
#' `RR` the rate at which it climbs back. Both are ordinary least-squares slopes
#' over their window, not endpoint differences, and the distinction matters: a
#' chord between two points is fixed by those two points alone, so a single
#' noisy reading at the trough moves it entirely, whereas the slope uses every
#' observation in the window. `DR` is negated so that it is reported as a
#' positive magnitude of decline, which is how it is tabulated in the source.
#'
#' `disturbance_r_squared` and `recovery_r_squared` report how well a straight
#' line actually described each window. A low value is not a failure of the
#' index but a warning about its interpretation: the decline or the return was
#' not linear, and the slope is then an average over a changing rate.
#'
#' ## The overall stability index
#'
#' \deqn{OSt = \frac{RR}{DR}.}
#'
#' The reasoning behind the ratio is that the two rates measure different halves
#' of stability. `DR` is inverse to homeostasis: a plant that resists change is
#' pushed down slowly, so a *low* `DR` means high resistance. `RR` is a direct
#' measure of resilience: a *high* `RR` means a fast return. A stable plant is
#' therefore one with low `DR` and high `RR`, and their ratio rises with both.
#' `OSt` is dimensionless, because both rates carry the same
#' \eqn{[\mathrm{value}]/[\mathrm{time}]} units, which is what makes it
#' comparable across variables measured on different scales.
#'
#' Values above 1 mean the plant recovered faster than it declined. Under
#' repeated cycles, an increase in `OSt` across cycles is the signature of
#' drought memory, whether it comes from a lower `DR`, a higher `RR`, or both.
#' Feed the result to [memory_trend()] to test that increase.
#'
#' ## The three magnitude indices
#'
#' \deqn{I = \frac{B - v_{min}}{B}, \qquad
#'   I_i = \frac{1}{B\,(t_{min} - t_{str})}
#'     \int_{t_{str}}^{t_{min}} \big(B - y(t)\big)\, dt, \qquad
#'   P = \frac{1}{B\,(t_{end} - t_{min})}
#'     \int_{t_{min}}^{t_{end}} \big(B - y(t)\big)\, dt.}
#'
#' `impact` is the depth of the trough, the single worst moment. `integrated
#' impact` is the average relative loss sustained *while the stress lasted*, and
#' `perturbation` the average relative loss still carried *through the recovery*.
#' The integrals are evaluated by the trapezoidal rule and normalized by
#' \eqn{B} times the length of their window, so all three are dimensionless and
#' on the same footing; with `percent = TRUE` they are multiplied by 100.
#'
#' Reporting `impact` and `integrated_impact` together is the point: two cycles
#' can reach the same trough and differ completely in how long the plant stayed
#' there, and only the integrated form sees that.
#'
#' @return A tibble with one row per group and cycle, with columns `baseline`,
#'   `drought_minimum`, `stress_detected`, `impact`, `integrated_impact`,
#'   `disturbance_rate`, `disturbance_r_squared`, `perturbation`,
#'   `recovery_rate`, `recovery_r_squared`, `overall_stability`, `n_disturbance`
#'   and `n_recovery`.
#'
#' @references
#' Ribeiro, R. V., Vitti, K. A., Marcos, F. C. C., Souza, G. M., Pissolato,
#' M. D., Almeida, L. F. R., Machado, E. C. (2021) Proposal of an index of
#' stability for evaluating plant drought memory: a case study in sugarcane.
#' Journal of Plant Physiology 260, 153397
#' <doi:10.1016/j.jplph.2021.153397>;
#' Ingrisch, J., Bahn, M. (2018) Towards a comparable quantification of
#' resilience. Trends in Ecology and Evolution 33, 251-259
#' <doi:10.1016/j.tree.2018.01.013>.
#'
#' @seealso [stability_index()] for operational stability metrics,
#'   [memory_trend()] to test whether `OSt` rises across cycles,
#'   [curve_shape_metrics()] for the shape descriptors of the same curve.
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
#' st <- overall_stability(segments)
#' st[, c("genotype", "treatment", "cycle", "disturbance_rate",
#'        "recovery_rate", "overall_stability")]
#'
#' # Drought memory: does the overall stability rise across cycles?
#' memory_trend(st, metrics = "overall_stability", min_cycles = 2)
#' @export
overall_stability <- function(
    data,
    time = time,
    value = transformed_value,
    group_by = c("genotype", "treatment", "replicate", "plant_id", "variable", "cycle"),
    baseline_summary = c("mean", "median", "last"),
    percent = TRUE,
    min_points = 3
) {
  baseline_summary <- match.arg(baseline_summary)

  scale_factor <- if (isTRUE(percent)) 100 else 1

  compute_group <- function(tbl) {
    g <- .cycle_geometry(tbl, baseline_summary = baseline_summary)

    if (!isTRUE(g$valid)) {
      return(.overall_stability_empty())
    }

    B <- g$baseline
    from <- if (is.finite(g$t_stress)) g$t_stress else g$t_start

    # ---- rates ---------------------------------------------------------------
    # The decline window runs from the onset of the stress to the trough, and
    # the return window from the trough to the end of the cycle, matching how
    # the two regression lines are drawn in the source.
    dist <- .stability_slope(g$x, g$y, from, g$t_min, min_points)
    rec <- .stability_slope(g$x, g$y, g$t_min, g$t_end, min_points)

    # DR is tabulated as a positive rate of decline, so the negative slope of a
    # falling trajectory is negated.
    disturbance_rate <- if (is.finite(dist$slope)) -dist$slope else NA_real_
    recovery_rate <- rec$slope

    overall <- if (is.finite(disturbance_rate) && disturbance_rate > 0) {
      .safe_div(recovery_rate, disturbance_rate)
    } else {
      NA_real_
    }

    # ---- magnitudes ----------------------------------------------------------
    impact <- .safe_div(B - g$v_min, abs(B))

    integrated <- .stability_area(g$x, g$y, B, from, g$t_min)
    perturbation <- .stability_area(g$x, g$y, B, g$t_min, g$t_end)

    reduction_period <- .time_below(g$x[g$x >= from], g$y[g$x >= from],
                                    level = 0.95 * B)
    stress_detected <- isTRUE(is.finite(reduction_period) && reduction_period > 0)

    tibble::tibble(
      baseline = B,
      drought_minimum = g$v_min,
      stress_detected = stress_detected,
      impact = impact * scale_factor,
      integrated_impact = integrated * scale_factor,
      disturbance_rate = disturbance_rate,
      disturbance_r_squared = dist$r_squared,
      perturbation = perturbation * scale_factor,
      recovery_rate = recovery_rate,
      recovery_r_squared = rec$r_squared,
      overall_stability = overall,
      n_disturbance = dist$n,
      n_recovery = rec$n
    )
  }

  .metrics_by_group(
    data = data,
    time_quo = rlang::enquo(time),
    value_quo = rlang::enquo(value),
    group_by = group_by,
    fun = compute_group
  )
}

.overall_stability_empty <- function() {
  tibble::tibble(
    baseline = NA_real_,
    drought_minimum = NA_real_,
    stress_detected = NA,
    impact = NA_real_,
    integrated_impact = NA_real_,
    disturbance_rate = NA_real_,
    disturbance_r_squared = NA_real_,
    perturbation = NA_real_,
    recovery_rate = NA_real_,
    recovery_r_squared = NA_real_,
    overall_stability = NA_real_,
    n_disturbance = NA_integer_,
    n_recovery = NA_integer_
  )
}

#' Least-Squares Slope Over a Time Window
#'
#' @return A list with `slope`, `r_squared` and `n`.
#' @noRd
.stability_slope <- function(x, y, from, to, min_points = 3) {
  out <- list(slope = NA_real_, r_squared = NA_real_, n = NA_integer_)

  if (!is.finite(from) || !is.finite(to) || to <= from) {
    return(out)
  }

  keep <- is.finite(x) & is.finite(y) & x >= from & x <= to
  n <- sum(keep)
  out$n <- as.integer(n)

  # Two points define a line exactly, which is a chord and not a fitted rate;
  # `min_points` keeps the slope from being reported when it carries no more
  # information than the endpoints already do.
  if (n < max(2L, min_points)) {
    return(out)
  }

  xx <- x[keep]
  yy <- y[keep]
  if (length(unique(xx)) < 2L) {
    return(out)
  }

  fit <- stats::lm.fit(x = cbind(`(Intercept)` = 1, t = xx), y = yy)
  slope <- unname(fit$coefficients[["t"]])
  if (!is.finite(slope)) {
    return(out)
  }

  ss_res <- sum(fit$residuals^2)
  ss_tot <- sum((yy - mean(yy))^2)

  out$slope <- slope
  out$r_squared <- if (is.finite(ss_tot) && ss_tot > 0) 1 - ss_res / ss_tot else NA_real_
  out
}

#' Mean Relative Deficit Over a Time Window
#'
#' Trapezoidal integral of `B - y(t)` over `[from, to]`, divided by
#' `B * (to - from)`, i.e. the time-averaged relative loss.
#'
#' @noRd
.stability_area <- function(x, y, baseline, from, to) {
  if (!is.finite(from) || !is.finite(to) || to <= from || !is.finite(baseline)) {
    return(NA_real_)
  }

  keep <- is.finite(x) & is.finite(y) & x >= from & x <= to
  if (sum(keep) < 2L) {
    return(NA_real_)
  }

  area <- .trapezoid_integral(x[keep], .deficit(y[keep], baseline))
  .safe_div(area, (to - from) * abs(baseline))
}
