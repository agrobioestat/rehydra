#' Replication Needed to Detect a Memory Effect
#'
#' How many replicates per genotype-by-treatment cell are needed to detect a
#' given change in an index between two drought cycles? Answering this before
#' the experiment is cheaper than discovering afterwards that a real effect went
#' undetected.
#'
#' @param effect Size of the difference to detect, in the units of the index
#'   (for `Rs`, `0.05` means five percentage points of the pre-drought level).
#' @param sd Residual standard deviation of the index. Take it from
#'   `rehydra_anova()$diagnostics$residual_se` of a pilot experiment, or from
#'   [memory_power_pilot()].
#' @param n Replicates per cell. Give `n` to compute power, or leave `NULL` and
#'   give `power` to solve for `n`.
#' @param power Target power. Give `power` to solve for `n`, or leave `NULL` and
#'   give `n` to compute power.
#' @param alpha Significance level.
#' @param n_groups Number of cells being compared. Used only when
#'   `adjust = "bonferroni"` or `"tukey"`.
#' @param design `"paired"` when the same plants are measured in both cycles
#'   (the usual drought-memory design), `"independent"` when different plants
#'   are used.
#' @param correlation Correlation between the two cycles measured on the same
#'   plant, used when `design = "paired"`.
#' @param adjust Multiplicity correction: `"none"`, `"bonferroni"` or
#'   `"tukey"`.
#' @param max_n Largest `n` searched when solving for replication.
#'
#' @details
#' The comparison of an index between two cycles is a two-sample or paired
#' t-test on the cell means. Writing \eqn{\delta} for `effect` and \eqn{\sigma}
#' for `sd`, the standardized effect size is
#'
#' \deqn{d = \frac{\delta}{\sigma\sqrt{c}}, \qquad
#'   c = \begin{cases} 2 & \text{independent} \\
#'   2(1 - \rho) & \text{paired}\end{cases}}
#'
#' and the power of the test with \eqn{\nu} degrees of freedom is
#'
#' \deqn{1 - \beta = P\left(|T_{\nu,\lambda}| > t_{1-\alpha/2,\,\nu}\right),
#'   \qquad \lambda = d\sqrt{n},}
#'
#' computed from the non-central t distribution rather than from a normal
#' approximation, which matters at the replication levels ecophysiology
#' experiments actually use (4 to 8 pots per cell), where the normal
#' approximation overstates power appreciably.
#'
#' The `design = "paired"` case is the one that usually applies and is worth
#' being explicit about: measuring cycle 1 and cycle 2 on the *same* plant
#' removes the between-plant variance from the contrast, and the factor
#' \eqn{2(1-\rho)} shrinks the variance the test has to overcome. With a
#' correlation of `0.5` the required replication roughly halves relative to
#' independent groups. Estimate \eqn{\rho} from a pilot with
#' [memory_power_pilot()]; assuming `0` when the design is paired is
#' conservative but wasteful.
#'
#' `adjust` accounts for testing several cells at once: `"bonferroni"` divides
#' `alpha` by `n_groups`, and `"tukey"` uses the studentized range, which is
#' less conservative for all-pairs comparisons and matches what
#' [rehydra_anova()] reports.
#'
#' Power analysis answers a design question, not a data question. Computing it
#' *after* seeing the result, with the observed effect as `effect`, is
#' "observed power" and is a known fallacy: it is a deterministic function of
#' the p-value and adds nothing. Use a pilot, or the smallest effect that would
#' be biologically meaningful.
#'
#' @return A tibble with `effect`, `sd`, `n`, `power`, `alpha`, `design`,
#'   `correlation`, `adjust`, `effect_size` and `df`.
#'
#' @seealso [rehydra_anova()], [memory_effect()], [memory_power_pilot()].
#'
#' @examples
#' # How many pots per cell to detect a 0.05 change in Rs, sd = 0.03?
#' memory_power(effect = 0.05, sd = 0.03, power = 0.8)
#'
#' # Power actually available with 4 pots per cell
#' memory_power(effect = 0.05, sd = 0.03, n = 4)
#'
#' # Same question for a paired design with a correlation of 0.5
#' memory_power(effect = 0.05, sd = 0.03, power = 0.8,
#'              design = "paired", correlation = 0.5)
#'
#' # A range of scenarios at once
#' memory_power(effect = c(0.03, 0.05, 0.10), sd = 0.03, power = 0.8)
#' @export
memory_power <- function(
    effect,
    sd,
    n = NULL,
    power = NULL,
    alpha = 0.05,
    n_groups = 2,
    design = c("independent", "paired"),
    correlation = 0,
    adjust = c("none", "bonferroni", "tukey"),
    max_n = 1000
) {
  design <- match.arg(design)
  adjust <- match.arg(adjust)

  if (is.null(n) && is.null(power)) {
    rlang::abort("Give either `n` (to compute power) or `power` (to solve for `n`).")
  }
  if (!is.null(n) && !is.null(power)) {
    rlang::abort("Give only one of `n` and `power`; the other one is computed.")
  }

  .check_fraction(alpha, "alpha", lower = 1e-10, upper = 0.5)
  if (design == "paired") {
    .check_fraction(correlation, "correlation", lower = -0.999, upper = 0.999)
  }

  grid <- expand.grid(
    effect = as.numeric(effect),
    sd = as.numeric(sd),
    n = if (is.null(n)) NA_real_ else as.numeric(n),
    power = if (is.null(power)) NA_real_ else as.numeric(power),
    KEEP.OUT.ATTRS = FALSE
  )

  out <- lapply(seq_len(nrow(grid)), function(i) {
    eff <- grid$effect[[i]]
    s <- grid$sd[[i]]

    if (!is.finite(eff) || !is.finite(s) || s <= 0) {
      rlang::abort("`effect` must be finite and `sd` must be finite and positive.")
    }

    # Variance multiplier of the contrast between two cycle means.
    vmult <- if (design == "paired") 2 * (1 - correlation) else 2
    d <- abs(eff) / (s * sqrt(vmult))

    if (is.na(grid$n[[i]])) {
      target <- grid$power[[i]]
      .check_fraction(target, "power", lower = 1e-6, upper = 0.999999)

      found <- NA_real_
      for (cand in 2:max_n) {
        if (.power_at(cand, d, alpha, n_groups, design, adjust) >= target) {
          found <- cand
          break
        }
      }
      n_use <- found
      pow <- if (is.finite(found)) {
        .power_at(found, d, alpha, n_groups, design, adjust)
      } else {
        NA_real_
      }
    } else {
      n_use <- grid$n[[i]]
      if (!is.finite(n_use) || n_use < 2) {
        rlang::abort("`n` must be at least 2.")
      }
      pow <- .power_at(n_use, d, alpha, n_groups, design, adjust)
    }

    tibble::tibble(
      effect = eff,
      sd = s,
      n = n_use,
      power = pow,
      alpha = alpha,
      design = design,
      correlation = if (design == "paired") correlation else NA_real_,
      adjust = adjust,
      effect_size = d,
      df = .power_df(n_use, design)
    )
  })

  res <- dplyr::bind_rows(out)

  if (any(is.na(res$n))) {
    warning(
      "Some scenarios need more than max_n = ", max_n,
      " replicates per cell; `n` is NA for those.",
      call. = FALSE
    )
  }

  res
}

.power_df <- function(n, design) {
  if (!is.finite(n)) return(NA_real_)
  if (design == "paired") n - 1 else 2 * (n - 1)
}

.power_at <- function(n, d, alpha, n_groups, design, adjust) {
  df <- .power_df(n, design)
  if (!is.finite(df) || df < 1) {
    return(NA_real_)
  }

  ncp <- d * sqrt(n)

  if (adjust == "tukey") {
    # Critical value from the studentized range, converted to the t scale:
    # q_crit / sqrt(2) is the |t| a pairwise contrast must exceed.
    q_crit <- stats::qtukey(1 - alpha, nmeans = max(2, n_groups), df = df)
    crit <- q_crit / sqrt(2)
  } else {
    a <- if (adjust == "bonferroni") alpha / max(1, choose(max(2, n_groups), 2)) else alpha
    crit <- stats::qt(1 - a / 2, df = df)
  }

  # Two-sided power from the non-central t; the lower tail is negligible but is
  # included so the result stays correct for small effects.
  stats::pt(-crit, df = df, ncp = ncp) +
    stats::pt(crit, df = df, ncp = ncp, lower.tail = FALSE)
}

#' Pilot Estimates for a Power Calculation
#'
#' Extract the residual standard deviation of an index, and the between-cycle
#' correlation within a plant, from a pilot dataset. These are the two numbers
#' [memory_power()] needs.
#'
#' @param indices A per-cycle index table, for example from
#'   [resilience_index()] or [rehydra_indices()].
#' @param metric Name of the index column.
#' @param group_by Columns identifying a treatment cell.
#' @param unit Column identifying the plant measured repeatedly across cycles.
#'
#' @details
#' `sd` is the pooled within-cell standard deviation, the same quantity the
#' ANOVA uses as its error term. `correlation` is the Pearson correlation
#' between the first two cycles of the same plant, which is what makes a paired
#' design more efficient than an independent one; it is `NA` when fewer than
#' three plants have both cycles.
#'
#' @return A tibble with `variable` (when present), `metric`, `sd`,
#'   `correlation`, `n_units` and `n_cycles`.
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
#' pilot <- memory_power_pilot(resilience_index(segments), metric = "Rs")
#' pilot
#'
#' memory_power(effect = 0.05, sd = pilot$sd[[1]], power = 0.8,
#'              design = "paired", correlation = pilot$correlation[[1]])
#' @export
memory_power_pilot <- function(
    indices,
    metric = "Rs",
    group_by = c("genotype", "treatment", "cycle"),
    unit = "plant_id"
) {
  dat <- tibble::as_tibble(indices)

  if (!metric %in% names(dat)) {
    rlang::abort(paste0("`metric` column `", metric, "` was not found in `indices`."))
  }

  by_var <- intersect("variable", names(dat))

  compute <- function(tbl) {
    cell_cols <- intersect(group_by, names(tbl))
    y <- suppressWarnings(as.numeric(tbl[[metric]]))

    # Pooled within-cell SD: the same error term the ANOVA tests against.
    key <- if (length(cell_cols) == 0) {
      rep("all", nrow(tbl))
    } else {
      do.call(paste, c(unname(as.list(tbl[cell_cols])), list(sep = "\r")))
    }

    parts <- split(y, key)
    ss <- sum(vapply(parts, function(v) {
      v <- v[is.finite(v)]
      if (length(v) < 2L) return(0)
      sum((v - mean(v))^2)
    }, numeric(1)))
    df <- sum(vapply(parts, function(v) max(0, sum(is.finite(v)) - 1L), numeric(1)))
    pooled_sd <- if (df > 0) sqrt(ss / df) else NA_real_

    rho <- NA_real_
    n_units <- NA_integer_
    if (unit %in% names(tbl) && "cycle" %in% names(tbl)) {
      cycles <- sort(unique(tbl$cycle))
      if (length(cycles) >= 2L) {
        w <- tbl |>
          dplyr::filter(.data$cycle %in% cycles[1:2]) |>
          dplyr::select(dplyr::all_of(c(unit, "cycle")), dplyr::all_of(metric)) |>
          tidyr::pivot_wider(names_from = "cycle", values_from = dplyr::all_of(metric),
                             names_prefix = "cycle_")
        cc <- stats::complete.cases(w[, -1, drop = FALSE])
        n_units <- sum(cc)
        if (n_units >= 3L) {
          rho <- suppressWarnings(stats::cor(
            w[[2]][cc], w[[3]][cc], use = "complete.obs"
          ))
        }
      }
    }

    tibble::tibble(
      metric = metric,
      sd = pooled_sd,
      correlation = as.numeric(rho),
      n_units = as.integer(n_units),
      n_cycles = dplyr::n_distinct(tbl$cycle)
    )
  }

  if (length(by_var) == 0) {
    return(compute(dat))
  }

  dat |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by_var))) |>
    dplyr::group_modify(~compute(.x)) |>
    dplyr::ungroup()
}
