#' Segment Drought-Rewatering Cycles
#'
#' Segment plant time series into drought and rewatering phases using manual
#' events or automatic heuristics.
#'
#' @param data A standardized data frame, typically from
#'   [prepare_rehydra_data()].
#' @param time Time column.
#' @param value Value column used for segmentation.
#' @param group_by Grouping columns.
#' @param method Segmentation method: `"auto"` or `"manual"`.
#' @param events Optional event table for manual segmentation.
#' @param smoothing Logical; apply smoothing before automatic segmentation.
#' @param smooth_span Loess span used when `smoothing = TRUE`.
#' @param min_drop Minimum relative drop from baseline to mark drought.
#' @param min_recovery Minimum relative recovery after rewatering.
#' @param n_cycles Optional expected number of cycles.
#' @param response_direction Physiological response direction mode.
#' @param fallback_manual Logical; fallback to manual heuristic when
#'   automatic segmentation is uncertain.
#'
#' @details
#' Required phase labels are `PreDrought`, `Drought`, `Rewatering`, and
#' `PostDrought`. `RecoveryPlateau` can be assigned manually.
#'
#' Automatic mode uses smoothed trajectories, first derivatives, local minima,
#' and relative baseline drop/recovery thresholds. If detection confidence is
#' low, the function emits an informative warning and optionally falls back to a
#' deterministic manual heuristic. If `fallback_manual = FALSE`, automatic
#' detection failures return an informative error.
#'
#' @return A tibble with original data and added columns:
#' `cycle`, `phase`, `phase_id`, `event`, `confidence`, and `method`.
#'
#' @references
#' Lloret et al. (2011) <doi:10.1111/j.1600-0706.2011.19372.x>;
#' Xu et al. (2010) <doi:10.4161/psb.5.6.11398>;
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
#'
#' segments <- segment_drought_cycle(
#'   data = prepared,
#'   time = time,
#'   value = transformed_value,
#'   group_by = c("genotype", "treatment", "replicate", "variable"),
#'   method = "auto"
#' )
#'
#' head(segments)
#' @export
segment_drought_cycle <- function(
    data,
    time,
    value,
    group_by = NULL,
    method = c("auto", "manual"),
    events = NULL,
    smoothing = TRUE,
    smooth_span = 0.25,
    min_drop = 0.15,
    min_recovery = 0.10,
    n_cycles = NULL,
    response_direction = c("higher_is_better", "lower_is_better", "more_negative_is_worse", "auto"),
    fallback_manual = TRUE
) {
  method <- match.arg(method)
  response_direction <- match.arg(response_direction)

  dat <- tibble::as_tibble(data)
  time_col <- .as_col_name(dat, rlang::enquo(time), "time")
  value_col <- .as_col_name(dat, rlang::enquo(value), "value")
  group_cols <- .resolve_group_cols(dat, group_by)

  work <- dat |>
    dplyr::mutate(
      .time_seg = suppressWarnings(as.numeric(.data[[time_col]])),
      .value_seg = suppressWarnings(as.numeric(.data[[value_col]]))
    )

  if (value_col != "transformed_value") {
    if (response_direction == "auto" && "response_direction" %in% names(work)) {
      work <- work |>
        dplyr::mutate(
          .value_seg = purrr::map2_dbl(
            .data$.value_seg,
            .data$response_direction,
            .apply_direction
          )
        )
    } else {
      work <- work |>
        dplyr::mutate(
          .value_seg = .apply_direction(.data$.value_seg, response_direction)
        )
    }
  }

  work <- work |>
    dplyr::mutate(.value_seg = .to_positive_performance(.data$.value_seg))

  if (any(!is.finite(work$.time_seg))) {
    rlang::abort("`time` must be numeric or coercible to numeric values.")
  }

  assign_by_quantiles <- function(tbl, cycles = NULL, method_label = "manual_default", confidence = 0.55) {
    times <- sort(unique(tbl$.time_seg))
    n_t <- length(times)

    if (n_t < 4) {
      out <- tbl
      out$cycle <- 1L
      out$phase <- "PreDrought"
      out$phase_id <- "C1_PreDrought"
      out$event <- "PreDrought"
      out$confidence <- confidence
      out$method <- method_label
      return(out)
    }

    n_cyc <- cycles %||% ifelse(n_t >= 16, 2L, 1L)

    cyc_labels <- cut(times,
      breaks = n_cyc,
      labels = FALSE,
      include.lowest = TRUE
    )

    cyc_map <- tibble::tibble(
      .time_seg = times,
      .cycle_est = as.integer(cyc_labels)
    )

    out <- tbl |>
      dplyr::left_join(cyc_map, by = ".time_seg") |>
      dplyr::mutate(cycle = .data$.cycle_est) |>
      dplyr::group_by(.data$cycle) |>
      dplyr::arrange(.data$.time_seg, .by_group = TRUE) |>
      dplyr::mutate(
        .idx = dplyr::row_number(),
        .n = dplyr::n(),
        phase = dplyr::case_when(
          .data$.idx <= pmax(1, floor(0.25 * .data$.n)) ~ "PreDrought",
          .data$.idx <= pmax(2, floor(0.50 * .data$.n)) ~ "Drought",
          .data$.idx <= pmax(3, floor(0.75 * .data$.n)) ~ "Rewatering",
          TRUE ~ "PostDrought"
        )
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        phase = as.character(.phase_factor(.data$phase)),
        phase_id = paste0("C", .data$cycle, "_", .data$phase),
        event = .data$phase,
        confidence = confidence,
        method = method_label
      ) |>
      dplyr::select(-".idx", -".n", -".cycle_est")

    out
  }

  apply_manual_events <- function(tbl, events_df) {
    ev <- .parse_manual_events(events_df)

    if (all(c("cycle", "phase", "start", "end") %in% names(ev))) {
      ev <- ev |>
        dplyr::mutate(
          cycle = as.integer(.data$cycle),
          phase = as.character(.data$phase),
          start = suppressWarnings(as.numeric(.data$start)),
          end = suppressWarnings(as.numeric(.data$end))
        )

      out <- tbl
      out$cycle <- NA_integer_
      out$phase <- NA_character_

      for (i in seq_len(nrow(ev))) {
        idx <- out$.time_seg >= ev$start[[i]] & out$.time_seg <= ev$end[[i]]
        out$cycle[idx] <- ev$cycle[[i]]
        out$phase[idx] <- ev$phase[[i]]
      }

      out$cycle[is.na(out$cycle)] <- 1L
      out$phase[is.na(out$phase)] <- "PostDrought"
      out$phase <- as.character(.phase_factor(out$phase))
      out$phase_id <- paste0("C", out$cycle, "_", out$phase)
      out$event <- out$phase
      out$confidence <- 1
      out$method <- "manual_events_interval"
      return(out)
    }

    if (all(c("time", "phase") %in% names(ev))) {
      if (!"cycle" %in% names(ev)) {
        ev$cycle <- 1L
      }

      ev <- ev |>
        dplyr::mutate(
          cycle = as.integer(.data$cycle),
          phase = as.character(.data$phase)
        )

      out <- tbl |>
        dplyr::left_join(ev |>
          dplyr::select("time", "cycle", "phase"), by = c(".time_seg" = "time"))

      out$cycle[is.na(out$cycle)] <- 1L
      out$phase[is.na(out$phase)] <- "PostDrought"
      out$phase <- as.character(.phase_factor(out$phase))
      out$phase_id <- paste0("C", out$cycle, "_", out$phase)
      out$event <- out$phase
      out$confidence <- 1
      out$method <- "manual_events_time"
      return(out)
    }

    assign_by_quantiles(tbl, cycles = n_cycles, method_label = "manual_default", confidence = 0.55)
  }

  detect_cycles_auto <- function(tbl) {
    tbl <- tbl |>
      dplyr::arrange(.data$.time_seg)

    if (all(c("cycle", "phase") %in% names(tbl)) &&
      sum(!is.na(tbl$cycle)) > 0 &&
      sum(!is.na(tbl$phase)) > 0) {
      out <- tbl |>
        dplyr::mutate(
          cycle = as.integer(.data$cycle),
          phase = as.character(.phase_factor(.data$phase)),
          phase_id = paste0("C", .data$cycle, "_", .data$phase),
          event = .data$phase,
          confidence = 1,
          method = "auto_preannotated"
        )
      return(out)
    }

    if (nrow(tbl) < 8) {
      return(NULL)
    }

    x <- suppressWarnings(as.numeric(tbl$.time_seg))
    y_raw <- suppressWarnings(as.numeric(tbl$.value_seg))

    valid_xy <- is.finite(x) & is.finite(y_raw)
    if (sum(valid_xy) < 6) {
      return(NULL)
    }

    # Fill sparse missing values for detection only; original values are retained.
    if (any(!is.finite(y_raw))) {
      y <- stats::approx(
        x = x[valid_xy],
        y = y_raw[valid_xy],
        xout = x,
        method = "linear",
        rule = 2
      )$y
    } else {
      y <- y_raw
    }

    if (smoothing && length(unique(x[is.finite(x)])) >= 5) {
      lo <- tryCatch(
        stats::loess(y ~ x, span = smooth_span, control = stats::loess.control(surface = "direct")),
        error = function(e) NULL
      )
      if (!is.null(lo)) {
        pred <- tryCatch(stats::predict(lo, newdata = x), error = function(e) NULL)
        if (!is.null(pred) && any(is.finite(pred))) {
          y[is.finite(pred)] <- as.numeric(pred[is.finite(pred)])
        }
      }
    }

    if (all(!is.finite(y))) {
      return(NULL)
    }

    expected_cycles <- n_cycles %||% ifelse(length(unique(x)) >= 16, 2L, 1L)
    expected_cycles <- max(1L, as.integer(expected_cycles))

    dx <- diff(x)
    dx[dx == 0] <- NA_real_
    slopes <- diff(y) / dx

    turn <- diff(sign(slopes))
    minima_idx <- which(turn > 0) + 1L
    minima_idx <- minima_idx[is.finite(y[minima_idx])]

    if (length(minima_idx) < expected_cycles) {
      ranked <- order(y, decreasing = FALSE, na.last = NA)
      for (idx in ranked) {
        if (length(minima_idx) == 0 || all(abs(minima_idx - idx) > 2)) {
          minima_idx <- c(minima_idx, idx)
        }
        if (length(minima_idx) >= expected_cycles) break
      }
    }

    minima_idx <- unique(minima_idx)
    minima_idx <- minima_idx[minima_idx > 1 & minima_idx < length(y)]
    if (length(minima_idx) < expected_cycles) {
      return(NULL)
    }
    if (length(minima_idx) > expected_cycles) {
      keep <- minima_idx[order(y[minima_idx], decreasing = FALSE)][seq_len(expected_cycles)]
      minima_idx <- sort(keep)
    } else {
      minima_idx <- sort(minima_idx)
    }

    starts <- integer(expected_cycles)
    ends <- integer(expected_cycles)
    cycle_conf <- numeric(expected_cycles)

    from <- 1L
    for (i in seq_len(expected_cycles)) {
      to <- minima_idx[[i]]
      if (i > 1) {
        from <- ends[[i - 1]] + 1L
      }

      if (from >= to || (to - from + 1) < 4) {
        return(NULL)
      }

      pre_slice <- y[from:to]
      pre_base <- .safe_mean(utils::head(pre_slice, max(2, floor(length(pre_slice) * 0.3))))
      drop <- .safe_div(pre_base - y[[to]], abs(pre_base))

      if (!is.finite(drop) || drop < min_drop) {
        return(NULL)
      }

      target <- y[[to]] + min_recovery * abs(pre_base)
      after <- which((seq_along(y) > to) & (y >= target))
      end_idx <- if (length(after) > 0) {
        after[[1]]
      } else {
        min(length(y), to + max(2L, floor(0.15 * length(y))))
      }

      if (i < expected_cycles) {
        end_idx <- min(end_idx, minima_idx[[i + 1]] - 1L)
      }

      if (end_idx <= to) {
        end_idx <- min(length(y), to + 3L)
      }

      starts[[i]] <- from
      ends[[i]] <- end_idx
      recovery_hit <- any((seq_along(y) > to & seq_along(y) <= end_idx) & (y >= target), na.rm = TRUE)
      cycle_conf[[i]] <- .clamp01(0.55 * .clamp01(drop / max(min_drop, 1e-8)) + 0.45 * ifelse(recovery_hit, 1, 0.6))
    }

    cyc <- rep(NA_integer_, length(y))
    phs <- rep(NA_character_, length(y))
    conf <- rep(NA_real_, length(y))

    for (i in seq_len(expected_cycles)) {
      idx <- starts[[i]]:ends[[i]]
      idx <- idx[idx >= 1 & idx <= length(y)]
      if (length(idx) < 4) {
        return(NULL)
      }

      n_idx <- length(idx)
      split1 <- max(1L, floor(0.25 * n_idx))
      split2 <- max(split1 + 1L, floor(0.50 * n_idx))
      split3 <- max(split2 + 1L, floor(0.75 * n_idx))
      split3 <- min(split3, n_idx - 1L)
      split2 <- min(split2, split3 - 1L)
      split1 <- min(split1, split2 - 1L)

      pre_idx <- idx[seq_len(split1)]
      dr_idx <- idx[(split1 + 1L):split2]
      rew_idx <- idx[(split2 + 1L):split3]
      post_idx <- idx[(split3 + 1L):n_idx]

      cyc[idx] <- i
      phs[pre_idx] <- "PreDrought"
      phs[dr_idx] <- "Drought"
      phs[rew_idx] <- "Rewatering"
      phs[post_idx] <- "PostDrought"
      conf[idx] <- cycle_conf[[i]]
    }

    last_idx <- max(which(!is.na(cyc)))
    if (is.finite(last_idx) && last_idx < length(cyc)) {
      tail_idx <- seq.int(last_idx + 1L, length(cyc))
      cyc[tail_idx] <- max(cyc, na.rm = TRUE)
      phs[tail_idx] <- "PostDrought"
      conf[tail_idx] <- cycle_conf[[length(cycle_conf)]]
    }

    if (any(is.na(cyc)) || any(is.na(phs))) {
      return(NULL)
    }

    out <- tbl
    out$cycle <- as.integer(cyc)
    out$phase <- as.character(.phase_factor(phs))
    out$phase_id <- paste0("C", out$cycle, "_", out$phase)
    out$event <- out$phase
    out$confidence <- .clamp01(conf)
    out$method <- "auto_heuristic"

    out
  }

  warned_auto <- FALSE

  segment_group <- function(tbl) {
    group_label <- if (length(group_cols) > 0) {
      vals <- vapply(
        group_cols,
        function(nm) paste0(nm, "=", as.character(tbl[[nm]][[1]])),
        FUN.VALUE = character(1)
      )
      paste(vals, collapse = ", ")
    } else {
      "all data"
    }

    if (method == "manual") {
      if (!is.null(events)) {
        return(apply_manual_events(tbl, events))
      }
      return(assign_by_quantiles(tbl, cycles = n_cycles, method_label = "manual_default", confidence = 0.6))
    }

    auto <- detect_cycles_auto(tbl)
    if (!is.null(auto)) {
      return(auto)
    }

    warn_txt <- paste(
      "Automatic segmentation could not confidently identify drought phases.",
      "Provide `events` for manual mode or tune `min_drop`, `min_recovery`, and smoothing parameters.",
      paste0("Group: ", group_label, ".")
    )

    if (isTRUE(fallback_manual)) {
      if (!warned_auto) {
        warning(paste(warn_txt, "Falling back to deterministic manual segmentation."), call. = FALSE)
        warned_auto <<- TRUE
      }
      return(assign_by_quantiles(tbl, cycles = n_cycles, method_label = "auto_fallback_manual", confidence = 0.45))
    }

    rlang::abort(warn_txt)
  }

  split_key <- if (length(group_cols) == 0) {
    rep("all", nrow(work))
  } else {
    apply(work[, group_cols, drop = FALSE], 1, paste, collapse = "::")
  }

  out <- split(work, split_key) |>
    lapply(segment_group) |>
    dplyr::bind_rows()

  keep <- intersect(
    c(
      "plant_id", "genotype", "treatment", "replicate", "block", "plot_id", "pot_id",
      "variable", "time", "value", "transformed_value", "response_direction",
      "cycle", "phase", "phase_id", "event", "confidence", "method"
    ),
    names(out)
  )

  sort_cols <- intersect(c("variable", "genotype", "treatment", "replicate", "time"), names(out))
  out <- out |>
    dplyr::select(dplyr::all_of(keep))

  if (length(sort_cols) > 0) {
    out <- out |>
      dplyr::arrange(dplyr::across(dplyr::all_of(sort_cols)))
  }

  out
}
