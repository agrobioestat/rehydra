# ------------------------------------------------------------------------------
# Internal curve geometry used by recovery_period_metrics(), mean_reduction_metrics() and
# curve_shape_metrics().
#
# Every index in those three families is a different reading of the SAME five
# landmarks extracted from one drought-rewatering cycle of a physiological time
# series y(t):
#
#   B       baseline (undisturbed performance level)
#   t_str   onset of the stress (first observation labelled "Drought")
#   t_rew   onset of rewatering (first observation labelled "Rewatering")
#   t_min   time of the trough, argmin of y over Drought U Rewatering
#   v_min   y(t_min), the trough value
#   t_end   last observation of the cycle
#
# and from the *deficit function*
#
#   d(t) = max(0, B - y(t))
#
# which is the vertical distance between the undisturbed baseline and the
# observed trajectory, floored at zero so that overcompensation (y > B) does not
# subtract from accumulated damage. All areas below are trapezoidal integrals of
# d(t); all "time to X" quantities are linearly interpolated between the two
# observations that bracket the crossing, so the resolution of an index is not
# limited to the sampling grid.
#
# Defining the landmarks once, here, is what keeps the period, reduction and
# curve-shape indices mutually consistent: they never disagree about where the
# trough or the baseline is.
# ------------------------------------------------------------------------------

#' Interpolated First Crossing of a Level
#'
#' @param x Numeric time vector (assumed sorted, finite).
#' @param y Numeric response vector.
#' @param level Numeric threshold.
#' @param direction `"up"` for the first `y >= level`, `"down"` for the first
#'   `y <= level`.
#' @param after Only crossings at or after this time are considered.
#'
#' @return The interpolated time of the crossing, or `NA_real_`.
#' @noRd
.first_crossing <- function(x, y, level, direction = c("up", "down"),
                            after = -Inf) {
  direction <- match.arg(direction)

  ok <- is.finite(x) & is.finite(y)
  if (!any(ok) || !is.finite(level)) {
    return(NA_real_)
  }

  x <- x[ok]
  y <- y[ok]
  ord <- order(x)
  x <- x[ord]
  y <- y[ord]

  keep <- x >= after
  if (!any(keep)) {
    return(NA_real_)
  }
  x <- x[keep]
  y <- y[keep]

  hit <- if (direction == "up") which(y >= level) else which(y <= level)
  if (length(hit) == 0) {
    return(NA_real_)
  }

  i <- hit[[1]]
  if (i == 1L) {
    return(x[[1]])
  }

  y0 <- y[[i - 1L]]
  y1 <- y[[i]]
  if (!is.finite(y0) || !is.finite(y1) || isTRUE(all.equal(y0, y1))) {
    return(x[[i]])
  }

  # Linear interpolation between the bracketing observations:
  #   t* = t0 + (level - y0) * (t1 - t0) / (y1 - y0)
  as.numeric(x[[i - 1L]] + (level - y0) * (x[[i]] - x[[i - 1L]]) / (y1 - y0))
}

#' Total Time Spent Below a Level
#'
#' Length of the time interval over which the linearly interpolated trajectory
#' stays at or below `level`, computed as the integral of the indicator
#' `1{y(t) <= level}` over the observation window.
#'
#' @noRd
.time_below <- function(x, y, level) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 2L || !is.finite(level)) {
    return(NA_real_)
  }

  x <- x[ok]
  y <- y[ok]
  ord <- order(x)
  x <- x[ord]
  y <- y[ord]

  total <- 0
  for (i in seq_len(length(x) - 1L)) {
    x0 <- x[[i]]
    x1 <- x[[i + 1L]]
    y0 <- y[[i]]
    y1 <- y[[i + 1L]]
    dt <- x1 - x0
    if (!is.finite(dt) || dt <= 0) next

    b0 <- y0 <= level
    b1 <- y1 <= level

    if (b0 && b1) {
      total <- total + dt
    } else if (b0 || b1) {
      # One endpoint is below and the other above: add only the fraction of the
      # segment on the impaired side, found by linear interpolation.
      frac <- if (isTRUE(all.equal(y0, y1))) 0.5 else (level - y0) / (y1 - y0)
      frac <- min(1, max(0, frac))
      total <- total + dt * if (b0) frac else (1 - frac)
    }
  }

  total
}

#' Deficit Integrated Over the Impaired Window Only
#'
#' Integrate `ref - y(t)` restricted to the set where `y(t) <= level`, together
#' with the length of that set.
#'
#' @details
#' Zeroing the deficit at the observations that sit above the threshold and then
#' integrating over the whole window is *not* equivalent: a single impaired
#' observation surrounded by healthy ones still contributes two half-triangles
#' spanning its neighbouring intervals, so the area would be spread over a much
#' longer stretch than the time actually spent impaired, and dividing it by that
#' time can push the mean relative reduction above 1. Because `y` is taken to be
#' piecewise linear, the impaired part of each segment is itself an interval and
#' the exact contribution is a trapezoid evaluated at its interpolated ends.
#'
#' @return A list with `area` and `duration`.
#' @noRd
.integrate_below <- function(x, y, level, ref) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 2L || !is.finite(level) || !is.finite(ref)) {
    return(list(area = NA_real_, duration = NA_real_))
  }

  x <- x[ok]
  y <- y[ok]
  ord <- order(x)
  x <- x[ord]
  y <- y[ord]

  area <- 0
  duration <- 0

  for (i in seq_len(length(x) - 1L)) {
    x0 <- x[[i]]
    x1 <- x[[i + 1L]]
    y0 <- y[[i]]
    y1 <- y[[i + 1L]]
    dt <- x1 - x0
    if (!is.finite(dt) || dt <= 0) next

    b0 <- y0 <= level
    b1 <- y1 <= level
    if (!b0 && !b1) next

    if (b0 && b1) {
      a <- x0
      b <- x1
      ya <- y0
      yb <- y1
    } else {
      frac <- if (isTRUE(all.equal(y0, y1))) 0.5 else (level - y0) / (y1 - y0)
      frac <- min(1, max(0, frac))
      x_cross <- x0 + frac * dt
      if (b0) {
        a <- x0; b <- x_cross; ya <- y0; yb <- level
      } else {
        a <- x_cross; b <- x1; ya <- level; yb <- y1
      }
    }

    seg <- b - a
    if (!is.finite(seg) || seg <= 0) next

    duration <- duration + seg
    area <- area + seg * ((ref - ya) + (ref - yb)) / 2
  }

  list(area = area, duration = duration)
}

#' Extract the Landmarks of One Drought-Rewatering Cycle
#'
#' @param tbl A tibble for a single group and cycle containing `.time`,
#'   `.value` and `phase`.
#' @param baseline_summary How to summarize the pre-drought phase.
#' @param baseline_value Optional externally supplied baseline that overrides
#'   the pre-drought summary (for example a matched control mean).
#'
#' @return A list with the landmarks documented at the top of this file plus the
#'   ordered series of the cycle.
#' @noRd
.cycle_geometry <- function(tbl,
                            baseline_summary = c("mean", "median", "last"),
                            baseline_value = NULL) {
  baseline_summary <- match.arg(baseline_summary)

  tbl <- tbl[order(tbl$.time), , drop = FALSE]
  ok <- is.finite(tbl$.time) & is.finite(tbl$.value)

  x <- tbl$.time
  y <- tbl$.value
  ph <- as.character(tbl$phase)

  empty <- list(
    baseline = NA_real_, t_stress = NA_real_, t_rewater = NA_real_,
    t_min = NA_real_, v_min = NA_real_, t_end = NA_real_, t_start = NA_real_,
    amplitude = NA_real_, x = numeric(), y = numeric(), phase = character(),
    x_post = numeric(), y_post = numeric(), valid = FALSE
  )

  if (sum(ok) < 3L) {
    return(empty)
  }

  x <- x[ok]
  y <- y[ok]
  ph <- ph[ok]

  baseline <- if (!is.null(baseline_value) && is.finite(baseline_value)) {
    as.numeric(baseline_value)
  } else {
    .pick_summary(y[ph == "PreDrought"], baseline_summary)
  }

  # Without a pre-drought phase, fall back to the highest early value so the
  # deficit function still has a meaningful reference.
  if (!is.finite(baseline)) {
    head_n <- max(1L, floor(0.2 * length(y)))
    baseline <- .safe_max(y[seq_len(head_n)])
  }
  if (!is.finite(baseline)) {
    return(empty)
  }

  stress_idx <- which(ph %in% c("Drought", "Rewatering", "PostDrought",
                                "RecoveryPlateau"))
  if (length(stress_idx) == 0L) {
    return(empty)
  }

  t_stress <- .safe_min(x[ph == "Drought"])
  if (!is.finite(t_stress)) {
    t_stress <- x[[stress_idx[[1]]]]
  }

  t_rewater <- .safe_min(x[ph == "Rewatering"])

  # The trough is searched over drought AND rewatering: leaf water status often
  # keeps falling for a few hours after water is restored, and the real minimum
  # then sits inside the rewatering window.
  trough_idx <- which(ph %in% c("Drought", "Rewatering"))
  if (length(trough_idx) == 0L) {
    trough_idx <- stress_idx
  }
  j <- trough_idx[which.min(y[trough_idx])[[1]]]
  t_min <- x[[j]]
  v_min <- y[[j]]

  if (!is.finite(t_rewater)) {
    # No explicit rewatering label: rewatering is assumed to start at the trough.
    t_rewater <- t_min
  }

  post <- x >= t_min

  list(
    baseline = baseline,
    t_stress = t_stress,
    t_rewater = t_rewater,
    t_min = t_min,
    v_min = v_min,
    t_start = .safe_min(x),
    t_end = .safe_max(x),
    amplitude = baseline - v_min,
    x = x,
    y = y,
    phase = ph,
    x_post = x[post],
    y_post = y[post],
    valid = TRUE
  )
}

#' Deficit Function of a Cycle
#'
#' `d(t) = max(0, B - y(t))`, the non-negative distance to the baseline.
#'
#' @noRd
.deficit <- function(y, baseline) {
  d <- baseline - y
  d[!is.finite(d)] <- NA_real_
  d[is.finite(d) & d < 0] <- 0
  d
}

#' Exponential Recovery Rate Constant
#'
#' Fit `y(t) = B - (B - v_min) * exp(-k * (t - t_min))` on the recovery branch.
#'
#' @details
#' The model says the remaining deficit decays exponentially once rewatering has
#' taken effect. Taking the deficit ratio removes both `B` and the amplitude:
#'
#'   `(B - y(t)) / (B - v_min) = exp(-k (t - t_min))`
#'
#' so that, with `u = t - t_min` and `z = log((B - y(t)) / (B - v_min))`,
#'
#'   `z = -k * u`
#'
#' a straight line through the origin. The closed-form least-squares slope of a
#' no-intercept regression gives the starting value
#'
#'   `k0 = -sum(u * z) / sum(u^2)`
#'
#' which is then refined by nonlinear least squares on the untransformed scale
#' (the log transform down-weights the large deficits that carry most of the
#' signal). If the refinement does not converge the log-linear estimate is
#' returned and `method` records which one was used.
#'
#' @return A list with `k`, `half_life`, `r_squared`, `n_points` and `method`.
#' @noRd
.fit_recovery_rate <- function(x, y, baseline, v_min, t_min,
                               min_amplitude_fraction = 0.01) {
  out <- list(k = NA_real_, half_life = NA_real_, r_squared = NA_real_,
              n_points = 0L, method = NA_character_)

  amplitude <- baseline - v_min
  if (!is.finite(amplitude) || amplitude <= 0) {
    return(out)
  }

  # A trough that never departed meaningfully from the baseline (a control
  # series, say) carries no recovery signal: fitting a decay to measurement
  # noise would return a huge, meaningless rate constant.
  if (!is.finite(baseline) || abs(baseline) <= 0 ||
      amplitude / abs(baseline) < min_amplitude_fraction) {
    return(out)
  }

  ok <- is.finite(x) & is.finite(y) & x > t_min
  if (sum(ok) < 2L) {
    return(out)
  }

  u <- x[ok] - t_min
  yy <- y[ok]

  deficit <- baseline - yy
  # Points already at or above the baseline carry no information about the decay
  # rate and would produce log(<= 0); they are dropped from the linearization
  # but still counted by the R-squared computed on the original scale.
  usable <- is.finite(deficit) & deficit > 0
  if (sum(usable) < 2L) {
    return(out)
  }

  z <- log(deficit[usable] / amplitude)
  uu <- u[usable]

  denom <- sum(uu^2, na.rm = TRUE)
  if (!is.finite(denom) || denom <= 0) {
    return(out)
  }

  k <- -sum(uu * z, na.rm = TRUE) / denom
  method <- "loglinear"

  if (!is.finite(k) || k <= 0) {
    return(out)
  }

  if (sum(ok) >= 3L) {
    # `warnOnly = TRUE` keeps a non-converging fit instead of erroring, and the
    # singular-gradient warnings it emits on flat series are not actionable for
    # the user, so they are absorbed here; the log-linear estimate is kept
    # whenever the refit does not return a usable positive rate.
    fit <- suppressWarnings(tryCatch(
      stats::nls(
        yy ~ baseline - amplitude * exp(-kk * u),
        start = list(kk = k),
        control = stats::nls.control(warnOnly = TRUE, maxiter = 100)
      ),
      error = function(e) NULL
    ))
    if (!is.null(fit)) {
      k_nls <- tryCatch(as.numeric(stats::coef(fit)[["kk"]]),
                        error = function(e) NA_real_)
      if (is.finite(k_nls) && k_nls > 0) {
        k <- k_nls
        method <- "nls"
      }
    }
  }

  fitted <- baseline - amplitude * exp(-k * u)
  ss_res <- sum((yy - fitted)^2, na.rm = TRUE)
  ss_tot <- sum((yy - mean(yy, na.rm = TRUE))^2, na.rm = TRUE)
  r2 <- if (is.finite(ss_tot) && ss_tot > 0) 1 - ss_res / ss_tot else NA_real_

  list(
    k = k,
    half_life = log(2) / k,
    r_squared = r2,
    n_points = sum(ok),
    method = method
  )
}

#' Apply a Per-Cycle Metric Function Over Groups
#'
#' Shared plumbing for the metric families: resolve the time/value columns,
#' check that segmentation has been run, and dispatch `fun` group by group.
#'
#' @noRd
.metrics_by_group <- function(data, time_quo, value_quo, group_by, fun,
                              require_phase = TRUE) {
  dat <- tibble::as_tibble(data)

  if (require_phase && !"phase" %in% names(dat)) {
    rlang::abort("`phase` column is required. Run segment_drought_cycle() first.")
  }

  time_col <- .as_col_name(dat, time_quo, "time")
  value_col <- .as_col_name(dat, value_quo, "value")
  group_cols <- .resolve_group_cols(dat, group_by)

  dat <- dat |>
    dplyr::mutate(
      .time = suppressWarnings(as.numeric(.data[[time_col]])),
      .value = suppressWarnings(as.numeric(.data[[value_col]]))
    )

  if (length(group_cols) == 0) {
    return(fun(dat))
  }

  dat |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::group_modify(~fun(.x)) |>
    dplyr::ungroup()
}
