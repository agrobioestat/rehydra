# Internal utilities for rehydra

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

.default_group_columns <- function(data) {
  intersect(
    c(
      "genotype", "treatment", "replicate", "block", "plot_id",
      "pot_id", "plant_id", "variable", "cycle"
    ),
    names(data)
  )
}

.required_standard_columns <- function() {
  c(
    "plant_id", "genotype", "treatment", "replicate", "block", "plot_id", "pot_id",
    "time", "cycle", "phase", "variable", "value", "response_direction", "transformed_value"
  )
}

.as_col_name <- function(data, quo, arg_name, required = TRUE) {
  expr <- rlang::get_expr(quo)
  col <- NULL

  if (rlang::quo_is_null(quo) || is.null(expr)) {
    if (!required) return(NULL)
    rlang::abort(paste0("`", arg_name, "` must be provided."))
  }

  if (is.symbol(expr)) {
    nm <- rlang::as_name(expr)
    if (nm %in% names(data)) {
      col <- nm
    } else {
      value <- tryCatch(rlang::eval_tidy(quo), error = function(e) NULL)
      if (is.character(value) && length(value) == 1L) {
        col <- value
      } else if (!required) {
        return(NULL)
      } else {
        col <- nm
      }
    }
  } else if (is.character(expr) && length(expr) == 1L) {
    col <- expr
  } else {
    value <- tryCatch(rlang::eval_tidy(quo), error = function(e) NULL)
    if (is.character(value) && length(value) == 1L) {
      col <- value
    }
  }

  if (is.null(col) && required) {
    rlang::abort(paste0("Could not parse column from `", arg_name, "`."))
  }

  if (!is.null(col) && !col %in% names(data)) {
    rlang::abort(paste0("Column `", col, "` (from `", arg_name, "`) was not found in `data`."))
  }

  col
}

.resolve_col_vector <- function(data, quo, arg_name = "variables") {
  if (rlang::quo_is_null(quo)) {
    return(character())
  }

  expr <- rlang::get_expr(quo)
  out <- character()

  if (is.symbol(expr)) {
    nm <- rlang::as_name(expr)
    if (nm %in% names(data)) {
      out <- nm
    } else {
      value <- tryCatch(rlang::eval_tidy(quo), error = function(e) NULL)
      if (is.character(value)) {
        out <- value
      } else {
        out <- nm
      }
    }
  } else if (is.character(expr)) {
    out <- as.character(expr)
  } else if (is.call(expr) && identical(expr[[1]], as.name("c"))) {
    parts <- as.list(expr)[-1]
    out <- vapply(
      parts,
      function(p) {
        if (is.symbol(p)) return(rlang::as_name(p))
        if (is.character(p) && length(p) == 1L) return(p)
        NA_character_
      },
      FUN.VALUE = character(1)
    )
    out <- out[!is.na(out)]
  } else {
    value <- tryCatch(rlang::eval_tidy(quo, data = data), error = function(e) NULL)
    if (is.character(value)) {
      out <- value
    }
  }

  out <- unique(out)
  missing <- setdiff(out, names(data))
  if (length(missing) > 0) {
    rlang::abort(
      paste0(
        "Columns from `", arg_name, "` not found in `data`: ",
        paste(missing, collapse = ", "),
        "."
      )
    )
  }

  out
}

.resolve_group_cols <- function(data, group_by = NULL) {
  if (is.null(group_by)) {
    return(.default_group_columns(data))
  }

  if (is.character(group_by)) {
    cols <- group_by
  } else {
    cols <- .resolve_col_vector(data, rlang::enquo(group_by), arg_name = "group_by")
  }

  unique(cols[cols %in% names(data)])
}

.as_numeric_time <- function(x) {
  if (inherits(x, "Date")) {
    return(as.numeric(x - min(x, na.rm = TRUE)))
  }

  if (inherits(x, "POSIXt")) {
    return(as.numeric(difftime(x, min(x, na.rm = TRUE), units = "days")))
  }

  suppressWarnings(as.numeric(x))
}

.finite_or_na <- function(x) {
  if (length(x) == 0) {
    return(NA_real_)
  }

  x <- suppressWarnings(as.numeric(x))
  if (!any(is.finite(x))) {
    return(NA_real_)
  }

  x[!is.finite(x)] <- NA_real_
  x
}

.safe_mean <- function(x) {
  x <- .finite_or_na(x)
  if (length(x) == 1 && is.na(x)) return(NA_real_)
  mean(x, na.rm = TRUE)
}

.safe_median <- function(x) {
  x <- .finite_or_na(x)
  if (length(x) == 1 && is.na(x)) return(NA_real_)
  stats::median(x, na.rm = TRUE)
}

.safe_min <- function(x) {
  x <- .finite_or_na(x)
  if (length(x) == 1 && is.na(x)) return(NA_real_)
  min(x, na.rm = TRUE)
}

.safe_max <- function(x) {
  x <- .finite_or_na(x)
  if (length(x) == 1 && is.na(x)) return(NA_real_)
  max(x, na.rm = TRUE)
}

.trapezoid_integral <- function(x, y) {
  idx <- which(is.finite(x) & is.finite(y))
  if (length(idx) < 2) {
    return(NA_real_)
  }

  xx <- x[idx]
  yy <- y[idx]
  ord <- order(xx)
  xx <- xx[ord]
  yy <- yy[ord]

  dx <- diff(xx)
  mids <- (yy[-1] + yy[-length(yy)]) / 2
  sum(dx * mids, na.rm = TRUE)
}

.safe_div <- function(num, den, min_abs_den = sqrt(.Machine$double.eps)) {
  num <- suppressWarnings(as.numeric(num))
  den <- suppressWarnings(as.numeric(den))

  out <- num / den
  small_den <- is.finite(den) & abs(den) < min_abs_den
  out[small_den] <- NA_real_
  out[!is.finite(out)] <- NA_real_
  out
}

.rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (!all(is.finite(rng))) {
    return(rep(NA_real_, length(x)))
  }
  if (diff(rng) == 0) {
    return(rep(0, length(x)))
  }
  (x - rng[1]) / diff(rng)
}

.clamp01 <- function(x) {
  pmin(1, pmax(0, x))
}

.detect_direction <- function(variable_name) {
  nm <- tolower(variable_name)
  if (grepl("water_potential|deficit|psi", nm)) return("more_negative_is_worse")
  if (grepl("temperature", nm)) return("lower_is_better")
  "higher_is_better"
}

.apply_direction <- function(value, direction) {
  if (direction == "higher_is_better") {
    return(value)
  }

  if (direction == "lower_is_better") {
    return(-1 * value)
  }

  if (direction == "more_negative_is_worse") {
    return(value)
  }

  value
}

.to_positive_performance <- function(x, epsilon = NULL, epsilon_fraction = 0.01) {
  x <- suppressWarnings(as.numeric(x))
  out <- x
  finite_idx <- is.finite(out)

  if (!any(finite_idx)) {
    out[] <- NA_real_
    return(out)
  }

  finite_vals <- out[finite_idx]
  min_val <- min(finite_vals, na.rm = TRUE)
  if (!is.finite(min_val)) {
    out[!finite_idx] <- NA_real_
    return(out)
  }

  if (is.null(epsilon)) {
    rng <- range(finite_vals, na.rm = TRUE)
    span <- diff(rng)
    if (!is.finite(span) || span <= 0) {
      epsilon <- 1e-6
    } else {
      epsilon <- max(1e-6, span * epsilon_fraction)
    }
  }
  epsilon <- suppressWarnings(as.numeric(epsilon))
  if (!is.finite(epsilon) || epsilon <= 0) {
    epsilon <- 1e-6
  }
  epsilon <- max(1e-12, epsilon)

  if (min_val <= 0) {
    out[finite_idx] <- out[finite_idx] - min_val + epsilon
  } else if (min_val < epsilon) {
    out[finite_idx] <- out[finite_idx] + (epsilon - min_val)
  }

  out[!finite_idx] <- NA_real_
  out
}

.phase_levels <- function() {
  c("PreDrought", "Drought", "Rewatering", "PostDrought", "RecoveryPlateau")
}

.phase_factor <- function(x) {
  factor(x, levels = .phase_levels())
}

.ensure_suggested <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    rlang::abort(paste0("Package `", pkg, "` is required for this feature."))
  }
}

.bootstrap_ci <- function(x, fun = mean, conf_level = 0.95, n_boot = 999, seed = NULL) {
  x <- x[is.finite(x)]

  if (length(x) == 0) {
    return(list(estimate = NA_real_, standard_error = NA_real_, conf_low = NA_real_, conf_high = NA_real_))
  }

  if (length(x) == 1) {
    est <- as.numeric(fun(x))
    return(list(estimate = est, standard_error = NA_real_, conf_low = NA_real_, conf_high = NA_real_))
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  bt <- boot::boot(
    data = x,
    statistic = function(data, idx) fun(data[idx]),
    R = n_boot
  )

  probs <- c((1 - conf_level) / 2, 1 - (1 - conf_level) / 2)
  qs <- stats::quantile(bt$t, probs = probs, na.rm = TRUE)

  list(
    estimate = as.numeric(fun(x)),
    standard_error = stats::sd(bt$t, na.rm = TRUE),
    conf_low = as.numeric(qs[[1]]),
    conf_high = as.numeric(qs[[2]])
  )
}

.pick_summary <- function(x, method = c("mean", "median", "last", "minimum")) {
  method <- match.arg(method)
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)

  if (method == "mean") return(mean(x))
  if (method == "median") return(stats::median(x))
  if (method == "last") return(x[[length(x)]])
  min(x)
}

.parse_manual_events <- function(events) {
  if (is.null(events)) {
    return(NULL)
  }

  ev <- tibble::as_tibble(events)
  names(ev) <- tolower(names(ev))

  if (all(c("cycle", "phase", "start", "end") %in% names(ev))) {
    return(ev)
  }

  if (all(c("time", "phase") %in% names(ev))) {
    ev <- ev |>
      dplyr::mutate(time = suppressWarnings(as.numeric(.data$time)))
    return(ev)
  }

  rlang::abort(
    "`events` must contain either columns `cycle`, `phase`, `start`, `end` or columns `time`, `phase`."
  )
}

.check_fraction <- function(x, arg_name, lower = 0, upper = 1) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) != 1L || !is.finite(x) || x < lower || x > upper) {
    rlang::abort(
      paste0("`", arg_name, "` must be a single number between ", lower,
             " and ", upper, ".")
    )
  }
  invisible(x)
}

.safe_sd <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 2L) return(NA_real_)
  stats::sd(x)
}
