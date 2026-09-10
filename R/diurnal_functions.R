#' Remove the Diurnal Cycle from a Sub-Daily Series
#'
#' Separate the daily rhythm from the drought signal in series measured several
#' times a day. Stomatal conductance and photosynthesis swing by more than the
#' drought effect itself between dawn and midday, so a trough detected on the
#' raw series can be noon rather than water stress.
#'
#' @param data A prepared or segmented data frame.
#' @param time Time column. May be numeric in days, `Date` or `POSIXct`.
#' @param value Value column to detrend.
#' @param group_by Grouping columns within which the diurnal pattern is
#'   estimated. Estimating it per genotype and variable is usually right; the
#'   rhythm of a fluorimeter reading is not the rhythm of a porometer reading.
#' @param method How the daily rhythm is estimated: `"harmonic"` (a sine and
#'   cosine pair, plus higher harmonics), `"mean_by_hour"` (the mean of each
#'   time-of-day bin) or `"none"` (only compute the phase columns).
#' @param harmonics Number of harmonics when `method = "harmonic"`. `1` fits a
#'   single daily wave, `2` also captures an asymmetric morning/afternoon shape.
#' @param bins Number of time-of-day bins when `method = "mean_by_hour"`.
#' @param origin_hour Hour of day treated as the start of the daily cycle.
#' @param min_points Minimum finite observations required in a group before a
#'   rhythm is fitted.
#' @param suffix Suffix appended to the new columns.
#'
#' @details
#' Time is first split into a day number and a phase within the day,
#'
#' \deqn{\phi(t) = \left(t - \lfloor t \rfloor\right) \in [0, 1),}
#'
#' after shifting by `origin_hour`. The **harmonic** method then regresses the
#' response on sine and cosine pairs of that phase,
#'
#' \deqn{y(t) = \mu + \sum_{k=1}^{K}\left[a_k \sin(2\pi k\phi) +
#'   b_k \cos(2\pi k\phi)\right] + \varepsilon,}
#'
#' which is an ordinary linear model, so it needs no iterative fitting and is
#' stable with few points per day. The fitted daily wave is the `diurnal`
#' column, and `detrended` is the original series minus that wave. Two harmonics
#' are usually enough: real diurnal courses are smooth, and adding harmonics
#' eventually starts absorbing the drought signal itself, which is exactly what
#' must not happen.
#'
#' The **mean_by_hour** method subtracts the mean of each time-of-day bin
#' instead. It assumes nothing about the shape and is preferable with dense,
#' regular sampling; with few points per bin it is noisier than the harmonic fit.
#'
#' Both methods estimate the rhythm **alongside** an across-days component - a
#' continuous linear term in time, plus one effect per day when the days are
#' well sampled - and then subtract only the rhythm. That detail is what keeps
#' the drought signal intact. A series drifting downwards over several days
#' declines *within* each day too, and that within-day ramp is correlated with
#' the sine terms; fitting harmonics on their own lets it leak into them, and
#' subtracting those harmonics would remove part of the very decline being
#' measured. On a two-day simulation with a true daily amplitude of `0.06`,
#' harmonics alone recover `0.090` and shrink the drought slope from `-0.096` to
#' `-0.076`, while fitting the trend jointly recovers `0.060` and `-0.096`
#' exactly.
#'
#' Only the rhythm is removed, so the level and the multi-day course survive
#' untouched, the detrended series keeps its original units, and the baseline
#' ratios of [resilience_index()] stay interpretable.
#'
#' Sub-daily resolution is required. A series with one point per day carries no
#' information about the shape of the day, and the function returns the input
#' unchanged with a warning rather than fitting noise.
#'
#' @return The input with added columns `day`, `phase_of_day`,
#'   `diurnal<suffix>` (the fitted rhythm) and `detrended<suffix>` (the value to
#'   analyse). Pass the detrended column as `value` to
#'   [segment_drought_cycle()] and to the metric functions.
#'
#' @seealso [prepare_rehydra_data()], [segment_drought_cycle()].
#'
#' @examples
#' # A two-day series sampled every three hours, with a strong daily rhythm
#' # superimposed on a linear drought decline.
#' t_hours <- seq(0, 47, by = 3)
#' phase <- (t_hours %% 24) / 24
#' sim <- data.frame(
#'   genotype = "G1",
#'   treatment = "Drought",
#'   replicate = "1",
#'   variable = "stomatal_conductance",
#'   time = t_hours / 24,
#'   transformed_value = 0.30 - 0.004 * t_hours + 0.06 * sin(2 * pi * phase)
#' )
#'
#' out <- detrend_diurnal(sim, group_by = c("genotype", "variable"))
#' head(out[, c("time", "transformed_value", "diurnal", "detrended")])
#'
#' # The daily swing is gone; the drought decline is preserved.
#' round(sd(out$transformed_value), 4)
#' round(sd(out$detrended), 4)
#' @export
detrend_diurnal <- function(
    data,
    time = time,
    value = transformed_value,
    group_by = c("genotype", "treatment", "variable"),
    method = c("harmonic", "mean_by_hour", "none"),
    harmonics = 2,
    bins = 8,
    origin_hour = 0,
    min_points = 6,
    suffix = ""
) {
  method <- match.arg(method)

  dat <- tibble::as_tibble(data)
  time_col <- .as_col_name(dat, rlang::enquo(time), "time")
  value_col <- .as_col_name(dat, rlang::enquo(value), "value")
  group_cols <- .resolve_group_cols(dat, group_by)

  t_num <- .as_numeric_time(dat[[time_col]]) - origin_hour / 24
  y <- suppressWarnings(as.numeric(dat[[value_col]]))

  dat$day <- floor(t_num)
  dat$phase_of_day <- t_num - floor(t_num)

  diurnal_col <- paste0("diurnal", suffix)
  detrended_col <- paste0("detrended", suffix)

  dat[[diurnal_col]] <- NA_real_
  dat[[detrended_col]] <- y

  if (method == "none") {
    return(dat)
  }

  # Sub-daily resolution is the precondition: with one observation per day every
  # phase is the same and any "rhythm" fitted would be pure noise.
  distinct_phase <- length(unique(round(dat$phase_of_day[is.finite(dat$phase_of_day)], 6)))
  if (distinct_phase < 3L) {
    warning(
      "Fewer than three distinct times of day were found; the series does not ",
      "appear to be sub-daily and was returned undetrended.",
      call. = FALSE
    )
    return(dat)
  }

  split_key <- if (length(group_cols) == 0) {
    rep("all", nrow(dat))
  } else {
    do.call(paste, c(unname(as.list(dat[group_cols])), list(sep = "\r")))
  }

  for (k in unique(split_key)) {
    idx <- which(split_key == k)
    yy <- y[idx]
    pp <- dat$phase_of_day[idx]
    dd <- dat$day[idx]
    tt <- t_num[idx]
    ok <- is.finite(yy) & is.finite(pp) & is.finite(dd) & is.finite(tt)

    if (sum(ok) < min_points || length(unique(round(pp[ok], 6))) < 3L) {
      next
    }

    fitted <- rep(NA_real_, length(idx))

    # The slow, across-days component has to be in the model while the rhythm is
    # estimated. Fitting harmonics alone against a series that is also drifting
    # downwards lets the drift leak into the sine terms, and subtracting them
    # would then remove part of the drought signal itself. Regressing the two
    # together and subtracting ONLY the harmonic part keeps them separate.
    trend <- .diurnal_trend_basis(tt, dd, ok)

    if (method == "harmonic") {
      n_harm <- max(1L, as.integer(harmonics))
      # Each harmonic costs two coefficients; leave residual degrees of freedom.
      budget <- sum(ok) - ncol(trend) - 2L
      n_harm <- min(n_harm, max(1L, floor(budget / 2L)))

      H <- do.call(cbind, lapply(seq_len(n_harm), function(h) {
        cbind(sin(2 * pi * h * pp), cos(2 * pi * h * pp))
      }))
      colnames(H) <- paste0(rep(c("s", "c"), n_harm),
                            rep(seq_len(n_harm), each = 2))

      X <- cbind(trend, H)
      fit <- tryCatch(
        stats::lm.fit(x = cbind(`(Intercept)` = 1, X[ok, , drop = FALSE]),
                      y = yy[ok]),
        error = function(e) NULL
      )
      if (is.null(fit)) next

      beta <- fit$coefficients
      beta[!is.finite(beta)] <- 0
      harm_beta <- beta[colnames(H)]
      harm_beta[!is.finite(harm_beta)] <- 0

      # Only the harmonic contribution is called "diurnal"; the intercept and
      # the trend belong to the signal and stay in the detrended series.
      fitted <- as.numeric(H %*% harm_beta)
    } else {
      cut_bins <- cut(pp, breaks = seq(0, 1, length.out = max(2L, bins) + 1L),
                      include.lowest = TRUE, labels = FALSE)

      present <- sort(unique(cut_bins[ok]))
      if (length(present) < 2L) next

      # The bin effects are estimated JOINTLY with the trend, exactly as the
      # harmonics are. Removing the trend first and then averaging by bin would
      # use a trend that was itself fitted to a series still containing the
      # rhythm, and the bias comes straight back: on a three-day simulation that
      # two-step version recovered a slope of -0.110 instead of -0.096.
      B <- vapply(
        present[-1L],
        function(b) as.numeric(!is.na(cut_bins) & cut_bins == b),
        numeric(length(pp))
      )
      B <- matrix(B, nrow = length(pp))
      colnames(B) <- paste0("bin", present[-1L])

      X <- cbind(trend, B)
      fit <- tryCatch(
        stats::lm.fit(x = cbind(`(Intercept)` = 1, X[ok, , drop = FALSE]),
                      y = yy[ok]),
        error = function(e) NULL
      )
      if (is.null(fit)) next

      beta <- fit$coefficients
      beta[!is.finite(beta)] <- 0
      bin_beta <- beta[colnames(B)]
      bin_beta[!is.finite(bin_beta)] <- 0

      # The reference bin is coded as zero; centring makes the rhythm sum to
      # zero so that removing it does not shift the level of the series.
      effects <- stats::setNames(c(0, bin_beta), as.character(present))
      effects <- effects - mean(effects)
      fitted <- as.numeric(effects[as.character(cut_bins)])
    }

    fitted[!is.finite(fitted)] <- 0

    dat[[diurnal_col]][idx] <- fitted
    # Subtracting only the rhythm leaves the level and the slow trend untouched,
    # so the detrended series keeps its original units and the ratio-based
    # indices stay interpretable.
    dat[[detrended_col]][idx] <- yy - fitted
  }

  dat
}

#' Basis for the Across-Days Component of a Sub-Daily Series
#'
#' A continuous linear term in time, plus one dummy per day when the days are
#' well enough sampled to afford them.
#'
#' @details
#' The continuous term is the part that matters, and it is easy to get wrong.
#' Day dummies alone are constant *within* a day, so they cannot absorb the
#' portion of a multi-day decline that falls between dawn and dusk — and it is
#' precisely that within-day ramp which correlates with the sine terms and
#' corrupts the estimated rhythm. On a two-day series with a linear decline and
#' a known daily amplitude of `0.06`, day dummies alone recover `0.090` (the
#' same as fitting no trend at all) while a linear term in time recovers exactly
#' `0.060`.
#'
#' The day dummies are still worth adding on top: they let the multi-day course
#' bend from day to day, which a single slope cannot. On a simulated six-day
#' Gaussian drought trough, the linear term alone leaves a maximum trend error
#' of `3.5e-4` against `5e-5` once the dummies are included.
#'
#' @return A matrix with one or more columns, aligned with the input length.
#' @noRd
.diurnal_trend_basis <- function(t_num, day, ok) {
  n <- length(t_num)

  lin <- matrix(as.numeric(t_num), ncol = 1L, dimnames = list(NULL, "time_linear"))
  lin[!is.finite(lin)] <- 0

  days <- sort(unique(day[ok]))
  n_days <- length(days)
  if (n_days < 2L) {
    return(lin)
  }

  per_day <- table(day[ok])
  # Each dummy costs a degree of freedom, so they are only added when every day
  # carries enough observations to estimate its own level.
  enough <- all(per_day >= 3L) && n_days <= max(2L, floor(sum(ok) / 3L))
  if (!enough) {
    return(lin)
  }

  # Treatment coding against the first day; the intercept carries that level.
  dummies <- vapply(days[-1L], function(d) as.numeric(day == d), numeric(n))
  dummies <- matrix(dummies, nrow = n)
  colnames(dummies) <- paste0("day", seq_len(ncol(dummies)))
  dummies[!is.finite(dummies)] <- 0

  cbind(lin, dummies)
}
