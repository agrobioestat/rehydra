#' Recovery-Period and Total-Reduction Indices
#'
#' Compute how long a plant takes to return to its pre-drought level and how
#' much performance it loses in total over the episode.
#'
#' @param data Segmented data from [segment_drought_cycle()].
#' @param time Time column.
#' @param value Value column used for the calculations.
#' @param group_by Grouping columns.
#' @param recovery_level Fraction of the baseline that defines "recovered".
#'   The default `0.95` means the response has to climb back to within 5% of the
#'   pre-drought level.
#' @param reduction_level Fraction of the baseline below which the response is
#'   considered impaired, used for `reduction_period`.
#' @param baseline_summary How the pre-drought baseline is summarized:
#'   `"mean"`, `"median"` or `"last"`.
#'
#' @details
#' Write `B` for the baseline (the pre-drought level), `y(t)` for the observed
#' trajectory, `t_str` for the onset of the stress, `t_min` for the time of the
#' trough and `t_end` for the last observation of the cycle. The deficit
#' function is
#'
#' \deqn{d(t) = \max(0,\; B - y(t)).}
#'
#' **Recovery period.** The time the plant needs to climb back to the
#' pre-drought level, measured from the trough:
#'
#' \deqn{RP = t_{rec} - t_{min}, \qquad
#'   t_{rec} = \min\{t \ge t_{min} : y(t) \ge \alpha B\}}
#'
#' with \eqn{\alpha =} `recovery_level`. The crossing time \eqn{t_{rec}} is
#' obtained by linear interpolation between the two observations that bracket
#' it, so `recovery_period` is not rounded to the sampling grid. When the series
#' ends before the threshold is reached the observation is **right-censored**:
#' `recovery_period` is `NA` and `recovery_censored` is `TRUE`. Treating those
#' as `NA` rather than as "the length of the experiment" avoids a downward bias
#' that would make the slowest genotypes look like the fastest.
#'
#' **Total reduction.** The whole performance loss caused by the episode, the
#' area between the baseline and the trajectory:
#'
#' \deqn{TR = \int_{t_{str}}^{t_{end}} d(t)\, dt}
#'
#' evaluated by the trapezoidal rule. It is expressed in
#' \eqn{[\mathrm{value}] \times [\mathrm{time}]} units, so it is comparable only
#' within a variable; `total_reduction_relative` divides it by the undisturbed
#' integral \eqn{B \times (t_{end} - t_{str})} and is a dimensionless fraction of
#' the potential performance that was lost, comparable across variables and
#' experiments.
#'
#' Floring the deficit at zero matters: without it, overcompensation after
#' rewatering (`y > B`) would subtract from the accumulated damage and a plant
#' that suffered badly and then overshot could report a total reduction close to
#' zero.
#'
#' Both quantities are read off a continuously sampled physiological trajectory:
#' the integral replaces the sum over discrete observation periods used when the
#' same components are computed on annual growth series, and the crossing that
#' closes the recovery period is interpolated rather than rounded to the
#' sampling grid.
#'
#' @return A tibble with one row per group and cycle and columns `baseline`,
#'   `drought_minimum`, `stress_detected`, `time_to_minimum`, `recovery_period`,
#'   `recovery_censored`, `time_recovered`, `reduction_period`,
#'   `total_reduction`, `total_reduction_relative` and `recovery_level`.
#'
#' @references
#' Lloret et al. (2011) <doi:10.1111/j.1600-0706.2011.19372.x>.
#'
#' @seealso [mean_reduction_metrics()], [curve_shape_metrics()], [resilience_index()].
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
#' recovery_period_metrics(segments)
#' @export
recovery_period_metrics <- function(
    data,
    time = time,
    value = transformed_value,
    group_by = c("genotype", "treatment", "replicate", "plant_id", "variable", "cycle"),
    recovery_level = 0.95,
    reduction_level = 0.95,
    baseline_summary = c("mean", "median", "last")
) {
  baseline_summary <- match.arg(baseline_summary)
  .check_fraction(recovery_level, "recovery_level")
  .check_fraction(reduction_level, "reduction_level")

  compute_group <- function(tbl) {
    g <- .cycle_geometry(tbl, baseline_summary = baseline_summary)

    if (!isTRUE(g$valid)) {
      return(.recovery_period_empty(recovery_level))
    }

    B <- g$baseline

    # ---- total reduction ----------------------------------------------------
    from <- if (is.finite(g$t_stress)) g$t_stress else g$t_start
    window <- is.finite(g$x) & g$x >= from
    d <- .deficit(g$y[window], B)
    total_reduction <- .trapezoid_integral(g$x[window], d)

    span <- g$t_end - from
    total_reduction_relative <- .safe_div(total_reduction, span * abs(B))

    # ---- period spent below the impairment threshold ------------------------
    reduction_period <- .time_below(g$x[window], g$y[window],
                                    level = reduction_level * B)
    stress_detected <- isTRUE(is.finite(reduction_period) && reduction_period > 0)

    # ---- recovery period ----------------------------------------------------
    t_rec <- .first_crossing(g$x, g$y, level = recovery_level * B,
                             direction = "up", after = g$t_min)
    censored <- stress_detected && !is.finite(t_rec)
    recovery_period <- if (!stress_detected || censored) {
      NA_real_
    } else {
      t_rec - g$t_min
    }

    tibble::tibble(
      baseline = B,
      drought_minimum = g$v_min,
      stress_detected = stress_detected,
      time_to_minimum = g$t_min - from,
      recovery_period = recovery_period,
      recovery_censored = censored,
      time_recovered = t_rec,
      reduction_period = reduction_period,
      total_reduction = total_reduction,
      total_reduction_relative = total_reduction_relative,
      recovery_level = recovery_level
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

.recovery_period_empty <- function(recovery_level) {
  tibble::tibble(
    baseline = NA_real_,
    drought_minimum = NA_real_,
    stress_detected = NA,
    time_to_minimum = NA_real_,
    recovery_period = NA_real_,
    recovery_censored = NA,
    time_recovered = NA_real_,
    reduction_period = NA_real_,
    total_reduction = NA_real_,
    total_reduction_relative = NA_real_,
    recovery_level = recovery_level
  )
}

#' Mean-Recovery-Rate and Mean-Reduction Indices
#'
#' Compute the average rate at which a plant returns to its pre-drought level
#' and the average intensity of the impairment while it lasted.
#'
#' @inheritParams recovery_period_metrics
#' @param recovery_level Fraction of the baseline that closes the recovery
#'   period.
#' @param reduction_level Fraction of the baseline below which the response
#'   counts as impaired.
#'
#' @details
#' With the same notation as [recovery_period_metrics()] (`B` baseline, `y(t)` trajectory,
#' `t_min` the trough, `v_min = y(t_min)`):
#'
#' **Mean recovery rate.** Total amplitude regained divided by the time it took
#' to regain it,
#'
#' \deqn{MRR = \frac{y(t_{rec}) - v_{min}}{t_{rec} - t_{min}},}
#'
#' the average slope of the chord that joins the trough to the moment of
#' recovery. Unlike `max_recovery_rate` in [recovery_metrics()], which is the
#' steepest observed first difference and is therefore sensitive to a single
#' noisy pair of points, `MRR` integrates over the whole return and is stable.
#' `mean_recovery_rate_relative` divides it by `B`, giving a
#' fraction-of-baseline per time unit that is comparable across variables.
#'
#' When the series ends before the recovery threshold is reached, the rate is
#' computed over the observed part of the return,
#' \eqn{(y(t_{end}) - v_{min}) / (t_{end} - t_{min})}, and `recovery_censored`
#' is `TRUE`. The value is a valid lower bound on the true rate only if recovery
#' is monotone, so censored rows should be reported as such rather than pooled
#' silently with complete ones.
#'
#' **Mean reduction.** The average relative deficit over the time the plant
#' actually spent impaired,
#'
#' \deqn{MR = \frac{1}{T_{red}} \int_{\{t\,:\,y(t) \le \beta B\}}
#'   \frac{B - y(t)}{B}\, dt,}
#'
#' with \eqn{\beta =} `reduction_level` and \eqn{T_{red}} the length of that
#' impaired window (`reduction_period`). Dividing by \eqn{T_{red}} rather than by
#' the full cycle is what separates *intensity* from *duration*: a short severe
#' episode and a long mild one can share the same `total_reduction` from
#' [recovery_period_metrics()] but are told apart by `mean_reduction` (high, short
#' `reduction_period`) versus (low, long `reduction_period`). Reporting the pair
#' is more informative than either alone.
#'
#' `max_reduction` is the depth of the trough, \eqn{(B - v_{min})/B}, the
#' complement of the resistance index `Rt` of [resilience_index()].
#'
#' @return A tibble with one row per group and cycle and columns `baseline`,
#'   `drought_minimum`, `stress_detected`, `reduction_period`, `mean_reduction`,
#'   `mean_reduction_absolute`, `max_reduction`, `recovery_period`,
#'   `recovery_censored`, `total_recovery`, `mean_recovery_rate` and
#'   `mean_recovery_rate_relative`.
#'
#' @references
#' Lloret et al. (2011) <doi:10.1111/j.1600-0706.2011.19372.x>.
#'
#' @seealso [recovery_period_metrics()], [curve_shape_metrics()].
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
#' mean_reduction_metrics(segments)
#' @export
mean_reduction_metrics <- function(
    data,
    time = time,
    value = transformed_value,
    group_by = c("genotype", "treatment", "replicate", "plant_id", "variable", "cycle"),
    recovery_level = 0.95,
    reduction_level = 0.95,
    baseline_summary = c("mean", "median", "last")
) {
  baseline_summary <- match.arg(baseline_summary)
  .check_fraction(recovery_level, "recovery_level")
  .check_fraction(reduction_level, "reduction_level")

  compute_group <- function(tbl) {
    g <- .cycle_geometry(tbl, baseline_summary = baseline_summary)

    if (!isTRUE(g$valid)) {
      return(.mean_reduction_empty())
    }

    B <- g$baseline

    from <- if (is.finite(g$t_stress)) g$t_stress else g$t_start
    window <- is.finite(g$x) & g$x >= from
    xw <- g$x[window]
    yw <- g$y[window]

    # ---- mean reduction over the impaired window ----------------------------
    thr <- reduction_level * B
    below <- .integrate_below(xw, yw, level = thr, ref = B)
    reduction_period <- below$duration

    mean_reduction <- NA_real_
    mean_reduction_abs <- NA_real_

    if (is.finite(reduction_period) && reduction_period > 0) {
      # Time-weighted mean deficit over the impaired window (not the unweighted
      # mean of the observations), so irregular sampling is handled correctly.
      mean_reduction_abs <- .safe_div(below$area, reduction_period)
      mean_reduction <- .safe_div(mean_reduction_abs, abs(B))
    }

    max_reduction <- .safe_div(B - g$v_min, abs(B))

    # A cycle whose trough never crossed the impairment threshold (a control
    # series, or a stress too mild to register) has no recovery to measure; the
    # flag lets those rows be filtered instead of silently averaged in.
    stress_detected <- isTRUE(is.finite(reduction_period) && reduction_period > 0)

    # ---- mean recovery rate -------------------------------------------------
    t_rec <- .first_crossing(g$x, g$y, level = recovery_level * B,
                             direction = "up", after = g$t_min)
    censored <- stress_detected && !is.finite(t_rec)

    if (!stress_detected) {
      recovery_period <- NA_real_
      total_recovery <- NA_real_
      mean_rate <- NA_real_
    } else {
      if (censored) {
        t_ref <- g$t_end
        v_ref <- g$y[[which.max(g$x)]]
      } else {
        t_ref <- t_rec
        v_ref <- recovery_level * B
      }

      recovery_period <- t_ref - g$t_min
      total_recovery <- v_ref - g$v_min
      mean_rate <- .safe_div(total_recovery, recovery_period)
    }

    tibble::tibble(
      baseline = B,
      drought_minimum = g$v_min,
      stress_detected = stress_detected,
      reduction_period = reduction_period,
      mean_reduction = mean_reduction,
      mean_reduction_absolute = mean_reduction_abs,
      max_reduction = max_reduction,
      recovery_period = if (isTRUE(censored)) NA_real_ else recovery_period,
      recovery_censored = censored,
      total_recovery = total_recovery,
      mean_recovery_rate = mean_rate,
      mean_recovery_rate_relative = .safe_div(mean_rate, abs(B))
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

.mean_reduction_empty <- function() {
  tibble::tibble(
    baseline = NA_real_,
    drought_minimum = NA_real_,
    stress_detected = NA,
    reduction_period = NA_real_,
    mean_reduction = NA_real_,
    mean_reduction_absolute = NA_real_,
    max_reduction = NA_real_,
    recovery_period = NA_real_,
    recovery_censored = NA,
    total_recovery = NA_real_,
    mean_recovery_rate = NA_real_,
    mean_recovery_rate_relative = NA_real_
  )
}
