#' Damage and Recovery Curve-Shape Metrics
#'
#' Describe the *shape* of a drought-rewatering trajectory rather than only its
#' endpoints: how much performance was lost (area under the deficit curve), how
#' fast half of it came back (`t50`), how steeply the remaining deficit decays
#' (the exponential rate constant `k`) and how long the plant kept falling after
#' water was restored (latency).
#'
#' @param data Segmented data from [segment_drought_cycle()].
#' @param time Time column.
#' @param value Value column used for the calculations.
#' @param group_by Grouping columns.
#' @param latency_fraction Fraction of the deficit amplitude that the response
#'   must regain before recovery counts as started. Default `0.05`.
#' @param damage_fraction Fraction of the deficit amplitude that must be lost
#'   before damage counts as started. Default `0.05`.
#' @param baseline_summary How the pre-drought baseline is summarized:
#'   `"mean"`, `"median"` or `"last"`.
#' @param fit_rate Logical; fit the exponential recovery rate constant. Set to
#'   `FALSE` to skip the nonlinear fit on very large datasets.
#'
#' @details
#' # The mathematics behind the landmarks
#'
#' Every metric below is read off the same five landmarks, extracted from the
#' *whole* time series of one cycle rather than from a handful of
#' phase averages. Let `B` be the baseline (pre-drought level), `y(t)` the
#' trajectory, `t_str` the onset of the stress, `t_rew` the onset of rewatering,
#' and
#'
#' \deqn{(t_{min},\, v_{min}) = \arg\min_{t \in \mathrm{Drought} \cup
#'   \mathrm{Rewatering}} y(t)}
#'
#' the trough. Searching the trough across *both* phases is deliberate: leaf
#' water potential and gas exchange routinely keep falling for some hours after
#' irrigation resumes, so restricting the search to the drought phase would
#' place the minimum too early and inflate every recovery metric. Write
#' \eqn{A = B - v_{min}} for the deficit amplitude.
#'
#' ## Area under the deficit curve (AUC)
#'
#' The deficit function is the non-negative distance to the baseline,
#'
#' \deqn{d(t) = \max(0,\; B - y(t)),}
#'
#' and its integral is evaluated with the trapezoidal rule on the observed grid
#' \eqn{t_1 < \dots < t_n},
#'
#' \deqn{\int d(t)\,dt \approx \sum_{i=1}^{n-1}
#'   \frac{d(t_i) + d(t_{i+1})}{2}\,(t_{i+1} - t_i),}
#'
#' which is exact for a piecewise-linear trajectory and, unlike a simple mean of
#' the observations, is correct under irregular sampling. Three areas are
#' reported: `deficit_auc` over the whole cycle, `deficit_auc_damage` over
#' \eqn{[t_{str}, t_{min}]} and `deficit_auc_recovery` over
#' \eqn{[t_{min}, t_{end}]}. Their ratio `auc_damage_share` says whether the cost
#' of the episode was paid while drying down or while recovering — two plants
#' with the same total AUC but opposite shares behave very differently.
#' `deficit_auc_normalized` divides the total by \eqn{B \times (t_{end} -
#' t_{str})}, making it a dimensionless fraction of the undisturbed integral.
#'
#' Clipping at zero is what makes the AUC a damage measure: without it,
#' overcompensation after rewatering would cancel out earlier losses.
#'
#' ## Time to half recovery (t50)
#'
#' \deqn{t_{50} = \min\{t \ge t_{min} : y(t) \ge v_{min} + A/2\} - t_{min},}
#'
#' the time needed to close half of the gap. The crossing is found by linear
#' interpolation between the two bracketing observations,
#'
#' \deqn{t^{*} = t_i + (L - y_i)\frac{t_{i+1} - t_i}{y_{i+1} - y_i},}
#'
#' so `t50` has finer resolution than the sampling interval. Because it is
#' anchored on the amplitude and not on `B`, `t50` is a pure speed descriptor:
#' a plant that recovers halfway quickly and then stalls has a small `t50` and a
#' large `residual_deficit`, and the pair identifies that behaviour, which a
#' single endpoint ratio such as `Rs` cannot.
#'
#' The mirror-image `t50_damage` is the time from `t_str` to lose half of the
#' amplitude, and `recovery_symmetry` = \eqn{t_{50} / (t_{50} + t_{50,damage})}
#' places the cycle on a scale where `0.5` means damage and recovery took
#' equally long, above `0.5` means recovery lagged behind the collapse.
#'
#' ## Recovery rate constant (k)
#'
#' The remaining deficit is assumed to decay exponentially once rewatering takes
#' effect,
#'
#' \deqn{y(t) = B - A\,e^{-k\,(t - t_{min})},}
#'
#' so that the deficit ratio is scale free,
#'
#' \deqn{\frac{B - y(t)}{A} = e^{-k (t - t_{min})}.}
#'
#' Taking logarithms linearizes it into a no-intercept regression of
#' \eqn{z = \log((B - y)/A)} on \eqn{u = t - t_{min}} whose closed-form slope
#'
#' \deqn{k_0 = -\frac{\sum u\,z}{\sum u^2}}
#'
#' is used as the starting value for a nonlinear least-squares refit on the
#' untransformed scale. The refit matters because the log transform gives the
#' near-baseline points — where the measurement noise is proportionally largest
#' — as much weight as the large early deficits that actually carry the signal.
#' `rate_method` records which estimate was kept, and `rate_r_squared` the fit
#' quality on the original scale; a low value is a warning that the return was
#' not exponential (a plateau, a rebound, or a second stress).
#' `recovery_half_life` \eqn{= \log 2 / k} restates `k` in time units.
#'
#' `k` and `t50` answer different questions: `t50` is model free and always
#' defined, `k` assumes a shape but summarizes the entire branch in one number
#' and is what makes recovery curves comparable between experiments with
#' different sampling designs.
#'
#' ## Temporal latency
#'
#' \deqn{\mathrm{latency} = \min\{t \ge a : y(t) \ge v_{min} +
#'   \varepsilon A\} - t_{rew}, \qquad a = \max(t_{min},\, t_{rew}),}
#'
#' with \eqn{\varepsilon =} `latency_fraction`: the delay between restoring water
#' and the first measurable upturn. The anchor \eqn{a} is whichever of the
#' trough and the rewatering onset comes last, and that detail is what keeps the
#' metric interpretable in both situations that occur in practice. When the
#' response keeps falling after irrigation the trough lies inside the rewatering
#' window and the latency measures exactly that continued decline; when the
#' trough is instead the last drought observation, the search starts at the
#' rewatering onset rather than before it, which would otherwise return a
#' negative delay. Either way the metric isolates the inertia of the system from
#' the speed of the recovery that follows.
#'
#' The companion `lag_to_minimum` \eqn{= t_{min} - t_{rew}} reports how long the
#' decline continued past rewatering, and is negative when the trough was
#' reached before the phase label changed. `damage_latency` is the symmetric
#' delay between the onset of the stress and the first measurable decline, a
#' proxy for how much buffering (stored soil water, hydraulic capacitance) the
#' plant had.
#'
#' @return A tibble with one row per group and cycle containing the landmark
#'   columns (`baseline`, `drought_minimum`, `stress_detected`,
#'   `deficit_amplitude`,
#'   `time_minimum`, `time_rewatering`), the areas (`deficit_auc`,
#'   `deficit_auc_damage`, `deficit_auc_recovery`, `deficit_auc_normalized`,
#'   `auc_damage_share`), the timings (`t50`, `t50_damage`,
#'   `recovery_symmetry`, `latency`, `lag_to_minimum`, `damage_latency`), the
#'   exponential fit (`recovery_rate_k`, `recovery_half_life`,
#'   `rate_r_squared`, `rate_method`, `rate_n_points`) and the endpoint
#'   descriptors (`residual_deficit`, `overshoot`).
#'
#' @references
#' Lloret et al. (2011) <doi:10.1111/j.1600-0706.2011.19372.x>;
#' Xu et al. (2010) <doi:10.4161/psb.5.6.11398>.
#'
#' @seealso [recovery_period_metrics()], [mean_reduction_metrics()], [plot_deficit_auc()].
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
#'
#' shape <- curve_shape_metrics(segments)
#' head(shape)
#'
#' # Speed of recovery is not the same as completeness of recovery:
#' shape[, c("t50", "recovery_rate_k", "residual_deficit")]
#' @export
curve_shape_metrics <- function(
    data,
    time = time,
    value = transformed_value,
    group_by = c("genotype", "treatment", "replicate", "plant_id", "variable", "cycle"),
    latency_fraction = 0.05,
    damage_fraction = 0.05,
    baseline_summary = c("mean", "median", "last"),
    fit_rate = TRUE
) {
  baseline_summary <- match.arg(baseline_summary)
  .check_fraction(latency_fraction, "latency_fraction")
  .check_fraction(damage_fraction, "damage_fraction")

  compute_group <- function(tbl) {
    g <- .cycle_geometry(tbl, baseline_summary = baseline_summary)

    if (!isTRUE(g$valid)) {
      return(.curve_shape_empty())
    }

    B <- g$baseline
    A <- g$amplitude
    from <- if (is.finite(g$t_stress)) g$t_stress else g$t_start

    # ---- areas --------------------------------------------------------------
    win <- is.finite(g$x) & g$x >= from
    xw <- g$x[win]
    yw <- g$y[win]
    dw <- .deficit(yw, B)

    auc_total <- .trapezoid_integral(xw, dw)

    dmg <- xw <= g$t_min
    rec <- xw >= g$t_min
    auc_damage <- if (sum(dmg) >= 2L) .trapezoid_integral(xw[dmg], dw[dmg]) else NA_real_
    auc_recovery <- if (sum(rec) >= 2L) .trapezoid_integral(xw[rec], dw[rec]) else NA_real_

    span <- g$t_end - from
    auc_norm <- .safe_div(auc_total, span * abs(B))
    auc_share <- .safe_div(auc_damage, auc_total)

    # ---- half times ---------------------------------------------------------
    t50 <- NA_real_
    t50_damage <- NA_real_
    latency <- NA_real_
    damage_latency <- NA_real_

    if (is.finite(A) && A > 0) {
      t_half <- .first_crossing(g$x, g$y, level = g$v_min + 0.5 * A,
                                direction = "up", after = g$t_min)
      t50 <- t_half - g$t_min

      t_half_dmg <- .first_crossing(g$x, g$y, level = B - 0.5 * A,
                                    direction = "down", after = from)
      t50_damage <- t_half_dmg - from

      # The upturn is searched from whichever comes last, the trough or the
      # rewatering onset. If the trough falls inside the rewatering window the
      # anchor is the trough (the plant kept declining after irrigation); if the
      # trough is the last drought observation the anchor is the rewatering
      # onset. Anchoring on the trough alone would find a crossing *before*
      # water was restored and report a negative latency.
      anchor <- max(c(g$t_min, g$t_rewater), na.rm = TRUE)
      t_onset <- .first_crossing(g$x, g$y, level = g$v_min + latency_fraction * A,
                                 direction = "up", after = anchor)
      latency <- max(t_onset - g$t_rewater, 0)

      t_decline <- .first_crossing(g$x, g$y, level = B - damage_fraction * A,
                                   direction = "down", after = from)
      damage_latency <- t_decline - from
    }

    symmetry <- .safe_div(t50, t50 + t50_damage)

    # ---- exponential recovery rate -----------------------------------------
    fit <- if (isTRUE(fit_rate)) {
      .fit_recovery_rate(g$x_post, g$y_post, baseline = B, v_min = g$v_min,
                         t_min = g$t_min)
    } else {
      list(k = NA_real_, half_life = NA_real_, r_squared = NA_real_,
           n_points = 0L, method = NA_character_)
    }

    # ---- endpoint descriptors ----------------------------------------------
    y_end <- g$y[[which.max(g$x)]]
    residual_deficit <- .safe_div(B - y_end, abs(B))
    post_max <- .safe_max(g$y_post)
    overshoot <- .safe_div(post_max - B, abs(B))
    if (is.finite(overshoot) && overshoot < 0) overshoot <- 0

    tibble::tibble(
      baseline = B,
      drought_minimum = g$v_min,
      stress_detected = isTRUE(is.finite(A) && is.finite(B) && abs(B) > 0 &&
                                 A / abs(B) >= 0.01),
      deficit_amplitude = A,
      time_minimum = g$t_min,
      time_rewatering = g$t_rewater,
      deficit_auc = auc_total,
      deficit_auc_damage = auc_damage,
      deficit_auc_recovery = auc_recovery,
      deficit_auc_normalized = auc_norm,
      auc_damage_share = auc_share,
      t50 = t50,
      t50_damage = t50_damage,
      recovery_symmetry = symmetry,
      latency = latency,
      lag_to_minimum = g$t_min - g$t_rewater,
      damage_latency = damage_latency,
      recovery_rate_k = fit$k,
      recovery_half_life = fit$half_life,
      rate_r_squared = fit$r_squared,
      rate_n_points = as.integer(fit$n_points),
      rate_method = fit$method,
      residual_deficit = residual_deficit,
      overshoot = overshoot
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

.curve_shape_empty <- function() {
  tibble::tibble(
    baseline = NA_real_,
    drought_minimum = NA_real_,
    stress_detected = NA,
    deficit_amplitude = NA_real_,
    time_minimum = NA_real_,
    time_rewatering = NA_real_,
    deficit_auc = NA_real_,
    deficit_auc_damage = NA_real_,
    deficit_auc_recovery = NA_real_,
    deficit_auc_normalized = NA_real_,
    auc_damage_share = NA_real_,
    t50 = NA_real_,
    t50_damage = NA_real_,
    recovery_symmetry = NA_real_,
    latency = NA_real_,
    lag_to_minimum = NA_real_,
    damage_latency = NA_real_,
    recovery_rate_k = NA_real_,
    recovery_half_life = NA_real_,
    rate_r_squared = NA_real_,
    rate_n_points = NA_integer_,
    rate_method = NA_character_,
    residual_deficit = NA_real_,
    overshoot = NA_real_
  )
}

#' Deficit Curve of One Cycle
#'
#' Return the point-by-point deficit `d(t) = max(0, B - y(t))` used by
#' [curve_shape_metrics()], together with the running (cumulative) area. This is
#' what [plot_deficit_auc()] shades, and it is exported so the shaded region can
#' be reproduced or exported for a custom figure.
#'
#' @inheritParams curve_shape_metrics
#'
#' @return A tibble with the grouping columns plus `time`, `value`, `baseline`,
#'   `deficit`, `cumulative_deficit` and `phase`.
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
#' head(deficit_curve(segments))
#' @export
deficit_curve <- function(
    data,
    time = time,
    value = transformed_value,
    group_by = c("genotype", "treatment", "replicate", "plant_id", "variable", "cycle"),
    baseline_summary = c("mean", "median", "last")
) {
  baseline_summary <- match.arg(baseline_summary)

  compute_group <- function(tbl) {
    g <- .cycle_geometry(tbl, baseline_summary = baseline_summary)

    if (!isTRUE(g$valid)) {
      return(tibble::tibble(
        time = numeric(), value = numeric(), baseline = numeric(),
        deficit = numeric(), cumulative_deficit = numeric(), phase = character()
      ))
    }

    d <- .deficit(g$y, g$baseline)

    # Running trapezoidal area, so the cumulative column can be read directly
    # off a plot at any time point.
    cum <- rep(0, length(d))
    if (length(d) > 1L) {
      steps <- diff(g$x) * (utils::head(d, -1L) + utils::tail(d, -1L)) / 2
      steps[!is.finite(steps)] <- 0
      cum <- c(0, cumsum(steps))
    }

    tibble::tibble(
      time = g$x,
      value = g$y,
      baseline = g$baseline,
      deficit = d,
      cumulative_deficit = cum,
      phase = g$phase
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
