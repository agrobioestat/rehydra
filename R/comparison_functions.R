#' Compare Genotypes for rehydra Metrics
#'
#' Perform pairwise genotype comparisons for resilience, damage, recovery,
#' stability, or memory metrics.
#'
#' @param data A data frame containing genotype-level metrics.
#' @param metric Metric column to compare (string or bare column name).
#' @param genotype_col Genotype column name.
#' @param group_by Additional grouping columns used before pairwise comparison.
#' @param adjust_method Adjustment method passed to [stats::p.adjust()].
#' @param conf_level Confidence level for pairwise confidence intervals.
#'
#' @return A list with two tibbles:
#' - `pairwise`: pairwise differences, confidence intervals, p-values, and
#'   adjusted p-values.
#' - `ranking`: genotype ranking by mean metric in each group.
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
#' compare_genotypes(res, metric = "Rs")
#' @export
compare_genotypes <- function(
    data,
    metric = "Rs",
    genotype_col = "genotype",
    group_by = c("treatment", "variable", "cycle"),
    adjust_method = "BH",
    conf_level = 0.95
) {
  dat <- tibble::as_tibble(data)

  metric_name <- if (is.character(metric)) {
    metric[[1]]
  } else {
    rlang::as_name(rlang::ensym(metric))
  }

  if (!metric_name %in% names(dat)) {
    rlang::abort(paste0("Metric column `", metric_name, "` was not found in `data`."))
  }

  if (!genotype_col %in% names(dat)) {
    rlang::abort(paste0("Genotype column `", genotype_col, "` was not found in `data`."))
  }

  grp <- intersect(group_by, names(dat))

  dat <- dat |>
    dplyr::mutate(
      .genotype = as.character(.data[[genotype_col]]),
      .metric = suppressWarnings(as.numeric(.data[[metric_name]]))
    )

  compare_group <- function(tbl) {
    genotypes <- sort(unique(tbl$.genotype))
    if (length(genotypes) < 2) {
      return(tibble::tibble())
    }

    pairs <- t(utils::combn(genotypes, 2))
    pairwise <- lapply(seq_len(nrow(pairs)), function(i) {
      g1 <- pairs[[i, 1]]
      g2 <- pairs[[i, 2]]

      x <- tbl$.metric[tbl$.genotype == g1]
      y <- tbl$.metric[tbl$.genotype == g2]

      mean_g1 <- mean(x, na.rm = TRUE)
      mean_g2 <- mean(y, na.rm = TRUE)
      diff_est <- mean_g1 - mean_g2

      p_val <- NA_real_
      conf_low <- NA_real_
      conf_high <- NA_real_

      if (sum(is.finite(x)) > 1 && sum(is.finite(y)) > 1) {
        tt <- tryCatch(
          stats::t.test(x, y, conf.level = conf_level),
          error = function(e) NULL
        )

        if (!is.null(tt)) {
          p_val <- tt$p.value
          conf_low <- tt$conf.int[[1]]
          conf_high <- tt$conf.int[[2]]
        }
      }

      tibble::tibble(
        genotype_1 = g1,
        genotype_2 = g2,
        mean_1 = mean_g1,
        mean_2 = mean_g2,
        difference = diff_est,
        conf_low = conf_low,
        conf_high = conf_high,
        p_value = p_val
      )
    }) |>
      dplyr::bind_rows()

    pairwise
  }

  pairwise <- if (length(grp) == 0) {
    compare_group(dat)
  } else {
    dat |>
      dplyr::group_by(dplyr::across(dplyr::all_of(grp))) |>
      dplyr::group_modify(~compare_group(.x)) |>
      dplyr::ungroup()
  }

  if (nrow(pairwise) > 0) {
    if (length(grp) == 0) {
      pairwise <- pairwise |>
        dplyr::mutate(p_adjust = stats::p.adjust(.data$p_value, method = adjust_method))
    } else {
      pairwise <- pairwise |>
        dplyr::group_by(dplyr::across(dplyr::all_of(grp))) |>
        dplyr::mutate(p_adjust = stats::p.adjust(.data$p_value, method = adjust_method)) |>
        dplyr::ungroup()
    }
  }

  ranking <- dat |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(grp, ".genotype")))) |>
    dplyr::summarise(
      mean_metric = mean(.data$.metric, na.rm = TRUE),
      n = sum(is.finite(.data$.metric)),
      .groups = "drop"
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grp))) |>
    dplyr::arrange(dplyr::desc(.data$mean_metric), .by_group = TRUE) |>
    dplyr::mutate(rank = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::rename(genotype = ".genotype")

  list(
    pairwise = pairwise,
    ranking = ranking,
    metric = metric_name
  )
}
