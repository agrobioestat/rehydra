#' Analyse rehydra Indices as a Designed Experiment
#'
#' Fit the analysis of variance implied by the experimental design to any index
#' produced by the package, with the treatment structure (genotype, treatment,
#' their interaction), the blocking structure and, optionally, random effects.
#' This is the step that turns descriptive indices into an inferential result.
#'
#' @param indices A per-replicate index table, for example the output of
#'   [resilience_index()], [curve_shape_metrics()], [recovery_period_metrics()] or
#'   [mean_reduction_metrics()].
#' @param response Name of the index column to analyse.
#' @param factors Character vector of fixed-effect factors.
#' @param interaction Logical; include the full factorial interaction between
#'   `factors`.
#' @param block Optional blocking column, entered as an additive fixed effect
#'   (randomized complete block design).
#' @param random Optional character vector of grouping columns to fit as random
#'   intercepts, for example `"plant_id"` when the same plants were measured in
#'   successive cycles. Requires the `lme4` package; when it is not installed
#'   the function falls back to the fixed-effects model and says so. Install
#'   `lmerTest` as well to obtain p-values.
#' @param by Columns that split the data into separate analyses (a distinct
#'   ANOVA per variable and per cycle by default).
#' @param conf_level Confidence level for the marginal means.
#' @param p_adjust Method passed to [stats::p.adjust()] for the pairwise
#'   comparisons.
#'
#' @details
#' The model fitted for each subset of `by` is
#'
#' \deqn{y = \mu + \mathrm{block} + A + B + AB + \varepsilon}
#'
#' with `A`, `B` the entries of `factors`, `AB` present only when
#' `interaction = TRUE` and the block term present only when `block` is given.
#' This is the ordinary randomized-complete-block factorial analysis, which is
#' how drought-rewatering experiments are almost always laid out.
#'
#' Splitting **by variable and by cycle** rather than pooling is deliberate.
#' Repeated cycles measured on the same plants are not independent, so a single
#' model that treated cycle as another crossed factor would understate the
#' standard errors. Analysing each cycle separately keeps every model within one
#' level of the repeated factor, and the change *between* cycles is then a
#' question for [memory_trend()] and [memory_effect()], which are built for it.
#' To model the cycles jointly anyway, see *Repeated measures across cycles*
#' below: `cycle` moves into `factors` and the plant becomes a random intercept,
#' which is the correct way to keep the dependence in the model rather than
#' ignoring it.
#'
#' Four things come back for each subset:
#'
#' - `anova`: the sequential (Type I) ANOVA table. With a balanced design, which
#'   is the usual case here, Type I, II and III sums of squares coincide; when
#'   the design is unbalanced the `balanced` column of `$models` flags it, and
#'   the term order in `factors` then matters.
#' - `means`: cell means with standard errors and confidence intervals built on
#'   the **residual** standard error and residual degrees of freedom of the
#'   model, not on the within-cell standard deviation, so the intervals use the
#'   pooled error term the ANOVA is testing against. With a random effect the
#'   means are model based and their standard errors come from the fixed-effect
#'   covariance matrix, so they carry the estimated variance components.
#' - `comparisons`: all pairwise contrasts of the cell means with Tukey's
#'   honest-significant-difference p-values, then adjusted again with
#'   `p_adjust` across the subsets.
#' - `diagnostics`: Shapiro-Wilk on the residuals and Bartlett's test of
#'   homogeneity across cells, plus `model_type` and `p_value_method` so it is
#'   always visible which model produced the numbers above. These are
#'   diagnostics, not gatekeepers: with the replication typical of an
#'   ecophysiology experiment they have little power, so read them together with
#'   the residual plot rather than as a pass or fail.
#'
#' # Repeated measures across cycles
#'
#' The default splits by cycle precisely to avoid the non-independence of
#' repeated measurements. To model the cycles jointly instead, put `cycle` among
#' the `factors`, drop it from `by`, and declare the plant as a random intercept:
#'
#' ```
#' rehydra_anova(res, response = "Rs",
#'               factors = c("genotype", "treatment", "cycle"),
#'               by = "variable", random = "plant_id")
#' ```
#'
#' Every table then comes from the mixed model: the ANOVA, the marginal means
#' (model based, with standard errors from the fixed-effect covariance matrix,
#' so they carry the between-plant variance component) and the Tukey contrasts.
#' `diagnostics$model_type` reports `"mixed"`, and `p_value_method` reports
#' `"satterthwaite"` when `lmerTest` is installed or `"none"` when it is not, in
#' which case the F statistics are given without p-values rather than with
#' p-values computed from residual degrees of freedom a mixed model does not
#' have.
#'
#' @return An object of class `rehydra_anova`, a list with elements `anova`,
#'   `means`, `comparisons`, `diagnostics`, `models` and `parameters`.
#'
#' @seealso [memory_trend()], [compare_genotypes()].
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
#' fit <- rehydra_anova(res, response = "Rs", factors = c("genotype", "treatment"))
#' fit
#' head(fit$anova)
#' @export
rehydra_anova <- function(
    indices,
    response = "Rs",
    factors = c("genotype", "treatment"),
    interaction = TRUE,
    block = NULL,
    random = NULL,
    by = c("variable", "cycle"),
    conf_level = 0.95,
    p_adjust = "holm"
) {
  dat <- tibble::as_tibble(indices)

  if (!response %in% names(dat)) {
    rlang::abort(paste0("`response` column `", response, "` was not found in `indices`."))
  }
  if (!is.numeric(dat[[response]])) {
    rlang::abort(paste0("`response` column `", response, "` must be numeric."))
  }

  factors <- intersect(factors, names(dat))
  if (length(factors) == 0) {
    rlang::abort("None of the `factors` columns were found in `indices`.")
  }

  block <- if (!is.null(block) && block %in% names(dat)) block else NULL
  by <- intersect(by, names(dat))

  # Drop factors that have a single level in the whole table: they cannot be
  # estimated and would abort the fit for every subset.
  usable <- vapply(factors, function(f) dplyr::n_distinct(dat[[f]]) > 1L, logical(1))
  dropped_factors <- factors[!usable]
  factors <- factors[usable]
  if (length(factors) == 0) {
    rlang::abort("Every requested factor has a single level; there is nothing to test.")
  }

  use_random <- FALSE
  random_note <- NA_character_
  if (!is.null(random)) {
    random <- intersect(random, names(dat))
    if (length(random) == 0) {
      random_note <- "No `random` column was found; a fixed-effects model was fitted."
    } else if (!requireNamespace("lme4", quietly = TRUE)) {
      random_note <- "Package 'lme4' is not installed; a fixed-effects model was fitted."
    } else {
      use_random <- TRUE
    }
  }

  subsets <- if (length(by) == 0) {
    list(dat)
  } else {
    split(dat, lapply(by, function(b) dat[[b]]), drop = TRUE)
  }

  anova_tbl <- list()
  means_tbl <- list()
  comp_tbl <- list()
  diag_tbl <- list()
  models <- list()

  for (nm in names(subsets) %||% seq_along(subsets)) {
    sub <- subsets[[nm]]
    key <- if (length(by) == 0) {
      tibble::tibble()
    } else {
      sub[1, by, drop = FALSE]
    }

    fit <- .fit_design_model(
      sub, response = response, factors = factors, interaction = interaction,
      block = block, random = if (use_random) random else NULL
    )

    if (is.null(fit$model)) {
      next
    }

    models[[as.character(nm)]] <- fit

    if (!is.null(fit$anova)) {
      anova_tbl[[length(anova_tbl) + 1L]] <- dplyr::bind_cols(key, fit$anova)
    }
    if (!is.null(fit$means)) {
      means_tbl[[length(means_tbl) + 1L]] <- dplyr::bind_cols(key, fit$means)
    }
    if (!is.null(fit$comparisons) && nrow(fit$comparisons) > 0) {
      comp_tbl[[length(comp_tbl) + 1L]] <- dplyr::bind_cols(key, fit$comparisons)
    }
    if (!is.null(fit$diagnostics)) {
      diag_tbl[[length(diag_tbl) + 1L]] <- dplyr::bind_cols(key, fit$diagnostics)
    }
  }

  comparisons <- dplyr::bind_rows(comp_tbl)
  if (nrow(comparisons) > 0 && "p_value" %in% names(comparisons)) {
    comparisons$p_value_adj <- stats::p.adjust(comparisons$p_value, method = p_adjust)
  }

  out <- list(
    anova = dplyr::bind_rows(anova_tbl),
    means = dplyr::bind_rows(means_tbl),
    comparisons = comparisons,
    diagnostics = dplyr::bind_rows(diag_tbl),
    models = models,
    parameters = list(
      response = response,
      factors = factors,
      dropped_factors = dropped_factors,
      interaction = interaction,
      block = block,
      random = if (use_random) random else NULL,
      random_note = random_note,
      by = by,
      conf_level = conf_level,
      p_adjust = p_adjust
    )
  )

  class(out) <- "rehydra_anova"
  out
}

.fit_design_model <- function(data, response, factors, interaction, block, random) {
  dat <- as.data.frame(data)
  dat[[response]] <- suppressWarnings(as.numeric(dat[[response]]))

  for (f in c(factors, block)) {
    if (!is.null(f)) dat[[f]] <- factor(as.character(dat[[f]]))
  }

  dat <- dat[is.finite(dat[[response]]), , drop = FALSE]

  keep <- vapply(factors, function(f) nlevels(droplevels(dat[[f]])) > 1L, logical(1))
  fit_factors <- factors[keep]
  if (nrow(dat) < 3L || length(fit_factors) == 0) {
    return(list(model = NULL))
  }

  treat_term <- if (isTRUE(interaction) && length(fit_factors) > 1L) {
    paste(fit_factors, collapse = " * ")
  } else {
    paste(fit_factors, collapse = " + ")
  }

  rhs <- treat_term
  if (!is.null(block) && nlevels(droplevels(dat[[block]])) > 1L) {
    rhs <- paste(block, "+", rhs)
  }

  # Cells of the treatment structure, used for the marginal means and the
  # pairwise comparisons.
  cell <- interaction(dat[fit_factors], drop = TRUE, sep = " : ")
  dat$.cell <- cell

  n_cells <- nlevels(droplevels(cell))
  if (nrow(dat) <= n_cells) {
    # Saturated: no residual degrees of freedom left to estimate the error.
    return(list(model = NULL))
  }

  form <- stats::as.formula(paste(response, "~", rhs))
  model <- tryCatch(stats::aov(form, data = dat), error = function(e) NULL)
  if (is.null(model)) {
    return(list(model = NULL))
  }

  mixed <- NULL
  if (!is.null(random)) {
    re <- paste(paste0("(1 | ", random, ")"), collapse = " + ")
    mform <- stats::as.formula(paste(response, "~", rhs, "+", re))
    mixed <- tryCatch(
      suppressMessages(suppressWarnings(lme4::lmer(mform, data = dat))),
      error = function(e) NULL
    )
  }

  balanced <- length(unique(table(dat$.cell))) == 1L

  # Everything reported below - the ANOVA, the marginal means, the contrasts and
  # the residual scale in the diagnostics - comes from ONE model. When a random
  # effect was requested and converged, that model is the mixed one; otherwise it
  # is the fixed-effects fit. Mixing the two (fitting `lmer` but reporting `aov`)
  # would silently treat repeated measures on the same plant as independent,
  # inflating the residual degrees of freedom and the significance of every term.
  if (!is.null(mixed)) {
    fitted_from <- .mixed_report(mixed, dat, response, fit_factors, rhs, block)
    model_type <- "mixed"
  } else {
    df_res <- stats::df.residual(model)
    mse <- sum(stats::residuals(model)^2) / df_res
    fitted_from <- list(
      anova = .tidy_aov(model),
      means = .cell_means(dat, response, fit_factors, mse, df_res,
                          conf_level = 0.95),
      comparisons = .tukey_pairs(dat, response, mse, df_res),
      df_residual = df_res,
      residual_se = sqrt(mse),
      p_value_method = "f-test",
      residuals = stats::residuals(model)
    )
    model_type <- "fixed"
  }

  resid <- fitted_from$residuals
  shapiro_p <- if (length(resid) >= 3L && length(resid) <= 5000L) {
    tryCatch(stats::shapiro.test(resid)$p.value, error = function(e) NA_real_)
  } else {
    NA_real_
  }
  bartlett_p <- tryCatch(
    stats::bartlett.test(dat[[response]], dat$.cell)$p.value,
    error = function(e) NA_real_
  )

  diagnostics <- tibble::tibble(
    n_obs = nrow(dat),
    n_cells = n_cells,
    balanced = balanced,
    model_type = model_type,
    p_value_method = fitted_from$p_value_method,
    df_residual = fitted_from$df_residual,
    residual_se = fitted_from$residual_se,
    shapiro_p = as.numeric(shapiro_p),
    bartlett_p = as.numeric(bartlett_p),
    mixed_model = identical(model_type, "mixed")
  )

  list(
    model = model,
    mixed = mixed,
    model_type = model_type,
    formula = form,
    anova = fitted_from$anova,
    means = fitted_from$means,
    comparisons = fitted_from$comparisons,
    diagnostics = diagnostics
  )
}

#' Report a Mixed Model on the Same Footing as the Fixed-Effects Fit
#'
#' @details
#' The marginal means are model based rather than raw cell averages: a grid of
#' the treatment cells is turned into a fixed-effects design matrix `X`, and
#'
#' \deqn{\hat{\mu} = X\hat{\beta}, \qquad
#'   \mathrm{Var}(\hat{\mu}) = X\,\mathrm{Var}(\hat{\beta})\,X^{\top},}
#'
#' so the standard errors carry the between-plant variance component that the
#' random intercept estimates. Each pairwise contrast uses the row difference
#' \eqn{c = x_a - x_b}, giving \eqn{c^{\top}\hat{\beta}} with standard error
#' \eqn{\sqrt{c^{\top}\mathrm{Var}(\hat{\beta})c}}. A blocking factor, if
#' present, is held at its first level: it cancels out of every contrast and
#' shifts all cell means by the same constant.
#'
#' P-values come from `lmerTest` (Satterthwaite degrees of freedom) when that
#' package is installed. Without it the F statistics are still reported but the
#' p-values are `NA`, because the residual degrees of freedom of a mixed model
#' are not the naive `n - p` and pretending otherwise is what this whole code
#' path exists to avoid.
#'
#' @noRd
.mixed_report <- function(mixed, dat, response, fit_factors, rhs, block) {
  has_lmertest <- requireNamespace("lmerTest", quietly = TRUE)

  anova_tbl <- NULL
  p_method <- "none"

  if (has_lmertest) {
    tab <- tryCatch(
      {
        mm <- lmerTest::as_lmerModLmerTest(mixed)
        as.data.frame(stats::anova(mm))
      },
      error = function(e) NULL
    )
    if (!is.null(tab) && "Pr(>F)" %in% names(tab)) {
      anova_tbl <- tibble::tibble(
        term = trimws(rownames(tab)),
        df = as.numeric(tab[["NumDF"]]),
        sum_sq = as.numeric(tab[["Sum Sq"]]),
        mean_sq = as.numeric(tab[["Mean Sq"]]),
        statistic = as.numeric(tab[["F value"]]),
        p_value = as.numeric(tab[["Pr(>F)"]])
      )
      p_method <- "satterthwaite"
    }
  }

  if (is.null(anova_tbl)) {
    tab <- as.data.frame(stats::anova(mixed))
    anova_tbl <- tibble::tibble(
      term = trimws(rownames(tab)),
      df = as.numeric(tab[["npar"]] %||% tab[["Df"]]),
      sum_sq = as.numeric(tab[["Sum Sq"]]),
      mean_sq = as.numeric(tab[["Mean Sq"]]),
      statistic = as.numeric(tab[["F value"]]),
      p_value = NA_real_
    )
    p_method <- "none"
  }

  beta <- lme4::fixef(mixed)
  vc <- as.matrix(stats::vcov(mixed))

  cells <- unique(dat[, fit_factors, drop = FALSE])
  cells <- cells[do.call(order, as.list(cells)), , drop = FALSE]
  if (!is.null(block) && block %in% names(dat)) {
    cells[[block]] <- factor(levels(dat[[block]])[1], levels = levels(dat[[block]]))
  }

  xlev <- lapply(
    dat[, intersect(c(fit_factors, block), names(dat)), drop = FALSE],
    levels
  )
  fe_terms <- stats::delete.response(
    stats::terms(stats::as.formula(paste("~", rhs)))
  )
  X <- stats::model.matrix(fe_terms, data = cells, xlev = xlev)
  X <- X[, intersect(names(beta), colnames(X)), drop = FALSE]
  beta <- beta[colnames(X)]
  vc <- vc[colnames(X), colnames(X), drop = FALSE]

  labels <- do.call(paste, c(lapply(fit_factors, function(f) as.character(cells[[f]])),
                             list(sep = " : ")))

  est <- as.vector(X %*% beta)
  se <- sqrt(pmax(0, diag(X %*% vc %*% t(X))))

  df_res <- tryCatch(as.numeric(stats::df.residual(mixed)), error = function(e) NA_real_)
  if (!is.finite(df_res) || df_res <= 0) {
    df_res <- max(1, nrow(dat) - length(beta))
  }
  tcrit <- stats::qt(0.975, df = df_res)

  n_cell <- as.integer(table(dat$.cell)[labels])
  n_cell[is.na(n_cell)] <- NA_integer_

  means <- tibble::tibble(cell = labels)
  for (f in fit_factors) {
    means[[f]] <- as.character(cells[[f]])
  }
  means$n <- n_cell
  means$estimate <- est
  means$std_error <- se
  means$conf_low <- est - tcrit * se
  means$conf_high <- est + tcrit * se

  comparisons <- if (length(labels) < 2L) {
    tibble::tibble(
      group1 = character(), group2 = character(), difference = numeric(),
      std_error = numeric(), statistic = numeric(), p_value = numeric()
    )
  } else {
    pairs <- utils::combn(seq_along(labels), 2L)
    dplyr::bind_rows(lapply(seq_len(ncol(pairs)), function(j) {
      a <- pairs[1L, j]
      b <- pairs[2L, j]
      cvec <- X[a, ] - X[b, ]
      diff <- as.numeric(crossprod(cvec, beta))
      se_c <- sqrt(max(0, as.numeric(t(cvec) %*% vc %*% cvec)))
      q <- if (se_c > 0) abs(diff) / (se_c / sqrt(2)) else NA_real_
      p <- if (is.finite(q)) {
        stats::ptukey(q, nmeans = length(labels), df = df_res, lower.tail = FALSE)
      } else {
        NA_real_
      }
      tibble::tibble(
        group1 = labels[[a]], group2 = labels[[b]],
        difference = diff, std_error = se_c,
        statistic = q, p_value = as.numeric(p)
      )
    }))
  }

  list(
    anova = anova_tbl,
    means = means,
    comparisons = comparisons,
    df_residual = df_res,
    residual_se = as.numeric(stats::sigma(mixed)),
    p_value_method = p_method,
    residuals = stats::residuals(mixed)
  )
}

.tidy_aov <- function(model) {
  tab <- as.data.frame(stats::anova(model))
  tibble::tibble(
    term = trimws(rownames(tab)),
    df = as.numeric(tab[["Df"]]),
    sum_sq = as.numeric(tab[["Sum Sq"]]),
    mean_sq = as.numeric(tab[["Mean Sq"]]),
    statistic = as.numeric(tab[["F value"]]),
    p_value = as.numeric(tab[["Pr(>F)"]])
  )
}

.cell_means <- function(dat, response, factors, mse, df_res, conf_level = 0.95) {
  split_vals <- split(dat[[response]], dat$.cell, drop = TRUE)
  n <- vapply(split_vals, function(v) sum(is.finite(v)), numeric(1))
  m <- vapply(split_vals, function(v) mean(v, na.rm = TRUE), numeric(1))

  # Standard error from the pooled error term of the model, which is the
  # variance the ANOVA actually tests against.
  se <- sqrt(mse / n)
  tcrit <- stats::qt(1 - (1 - conf_level) / 2, df = df_res)

  labels <- names(split_vals)
  parts <- strsplit(labels, " : ", fixed = TRUE)

  out <- tibble::tibble(cell = labels)
  for (i in seq_along(factors)) {
    out[[factors[[i]]]] <- vapply(
      parts,
      function(p) if (length(p) >= i) p[[i]] else NA_character_,
      character(1)
    )
  }

  out$n <- as.integer(n)
  out$estimate <- as.numeric(m)
  out$std_error <- as.numeric(se)
  out$conf_low <- out$estimate - tcrit * out$std_error
  out$conf_high <- out$estimate + tcrit * out$std_error
  out
}

.tukey_pairs <- function(dat, response, mse, df_res) {
  split_vals <- split(dat[[response]], dat$.cell, drop = TRUE)
  labels <- names(split_vals)

  if (length(labels) < 2L) {
    return(tibble::tibble(
      group1 = character(), group2 = character(), difference = numeric(),
      std_error = numeric(), statistic = numeric(), p_value = numeric()
    ))
  }

  n <- vapply(split_vals, function(v) sum(is.finite(v)), numeric(1))
  m <- vapply(split_vals, function(v) mean(v, na.rm = TRUE), numeric(1))

  pairs <- utils::combn(seq_along(labels), 2L)

  out <- lapply(seq_len(ncol(pairs)), function(j) {
    a <- pairs[1L, j]
    b <- pairs[2L, j]
    diff <- m[[a]] - m[[b]]
    se <- sqrt(mse * (1 / n[[a]] + 1 / n[[b]]))
    q <- if (is.finite(se) && se > 0) abs(diff) / (se / sqrt(2)) else NA_real_

    # Studentized range distribution: the Tukey adjustment for the whole family
    # of pairwise comparisons among `length(labels)` means.
    p <- if (is.finite(q) && df_res > 0) {
      stats::ptukey(q, nmeans = length(labels), df = df_res, lower.tail = FALSE)
    } else {
      NA_real_
    }

    tibble::tibble(
      group1 = labels[[a]],
      group2 = labels[[b]],
      difference = diff,
      std_error = se,
      statistic = q,
      p_value = as.numeric(p)
    )
  })

  dplyr::bind_rows(out)
}

#' Print Method for rehydra ANOVA Objects
#'
#' @param x A `rehydra_anova` object.
#' @param ... Not used.
#'
#' @return Invisibly returns `x`.
#' @method print rehydra_anova
#' @export
print.rehydra_anova <- function(x, ...) {
  p <- x$parameters
  cat("<rehydra_anova>\n")
  cat("Response:  ", p$response, "\n", sep = "")
  cat("Factors:   ", paste(p$factors, collapse = if (isTRUE(p$interaction)) " * " else " + "), "\n", sep = "")
  if (!is.null(p$block)) cat("Block:     ", p$block, "\n", sep = "")
  if (!is.null(p$random)) cat("Random:    ", paste(p$random, collapse = ", "), "\n", sep = "")
  if (length(p$by) > 0) cat("Split by:  ", paste(p$by, collapse = ", "), "\n", sep = "")
  cat("Models fitted: ", length(x$models), "\n", sep = "")

  if (length(p$dropped_factors) > 0) {
    cat("Dropped (single level): ", paste(p$dropped_factors, collapse = ", "), "\n", sep = "")
  }
  if (!is.na(p$random_note)) {
    cat("Note: ", p$random_note, "\n", sep = "")
  }

  if (nrow(x$anova) > 0) {
    sig <- x$anova[is.finite(x$anova$p_value) & x$anova$p_value < 0.05, , drop = FALSE]
    cat("Significant terms (p < 0.05): ", nrow(sig), " of ",
        sum(is.finite(x$anova$p_value)), "\n", sep = "")
  }

  invisible(x)
}

#' Summary Method for rehydra ANOVA Objects
#'
#' @param object A `rehydra_anova` object.
#' @param ... Not used.
#'
#' @return The ANOVA table as a tibble.
#' @method summary rehydra_anova
#' @export
summary.rehydra_anova <- function(object, ...) {
  object$anova
}

#' Tidy Method for rehydra ANOVA Objects
#'
#' @param x A `rehydra_anova` object.
#' @param ... Not used.
#'
#' @return The ANOVA table as a tibble.
#' @method tidy rehydra_anova
#' @export
tidy.rehydra_anova <- function(x, ...) {
  x$anova
}
