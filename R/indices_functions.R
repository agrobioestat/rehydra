#' Every Index Family in One Table
#'
#' Compute the resilience, damage, recovery, recovery-period, mean-reduction and
#' curve-shape families in a single call and return them joined on the group-and-cycle key,
#' one row per group per cycle.
#'
#' @param data Segmented data from [segment_drought_cycle()].
#' @param time Time column.
#' @param value Value column used for the calculations.
#' @param group_by Grouping columns; the join key is these columns.
#' @param families Which families to include. Any of `"resilience"`,
#'   `"damage"`, `"recovery"`, `"recovery_period"`, `"mean_reduction"`,
#'   `"shape"`, `"stability"`.
#' @param prefix Logical; prefix each column with its family (`Rs` becomes
#'   `resilience_Rs`). Useful when several families expose columns with the same
#'   name.
#' @param ... Passed to every underlying metric function, so shared arguments
#'   such as `baseline_summary` stay consistent across families.
#'
#' @details
#' The families overlap on purpose: `baseline`, `drought_minimum` and
#' `stress_detected` are computed from the same landmarks by
#' [recovery_period_metrics()], [mean_reduction_metrics()] and [curve_shape_metrics()], and
#' `PreDrought` appears in both [damage_metrics()] and [recovery_metrics()].
#' Duplicated columns are kept only once, from the first family that provides
#' them, unless `prefix = TRUE`. Because all families read one shared geometry
#' the duplicates are identical by construction, which the package tests check.
#'
#' The result is the natural input to [rehydra_anova()] and [memory_trend()]:
#' both take a per-replicate index table and a `response`/`metrics` argument, so
#' one call here gives every index they can be pointed at.
#'
#' @return A tibble with one row per group and cycle.
#'
#' @seealso [summarize_rehydra()] for the full pipeline including memory and
#'   segmentation.
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
#'
#' idx <- rehydra_indices(segments)
#' dim(idx)
#' idx[, c("genotype", "cycle", "Rs", "deficit_auc", "mean_reduction")]
#' @export
rehydra_indices <- function(
    data,
    time = time,
    value = transformed_value,
    group_by = c("genotype", "treatment", "replicate", "plant_id", "variable", "cycle"),
    families = c("resilience", "damage", "recovery", "recovery_period",
                 "mean_reduction", "shape", "stability"),
    prefix = FALSE,
    ...
) {
  families <- match.arg(families, several.ok = TRUE)

  dat <- tibble::as_tibble(data)
  key <- intersect(group_by, names(dat))
  if (length(key) == 0) {
    rlang::abort("None of the `group_by` columns were found in `data`.")
  }

  dots <- list(...)
  tables <- list()

  if ("resilience" %in% families) {
    tables$resilience <- resilience_index(
      data = dat, time = {{ time }}, value = {{ value }}, group_by = group_by,
      ci_method = dots$ci_method %||% "none",
      conf_level = dots$conf_level %||% 0.95,
      n_boot = dots$n_boot %||% 999
    )
  }
  if ("damage" %in% families) {
    tables$damage <- damage_metrics(
      data = dat, time = {{ time }}, value = {{ value }}, group_by = group_by
    )
  }
  if ("recovery" %in% families) {
    tables$recovery <- recovery_metrics(
      data = dat, time = {{ time }}, value = {{ value }}, group_by = group_by
    )
  }
  if ("recovery_period" %in% families) {
    tables$recovery_period <- recovery_period_metrics(
      data = dat, time = {{ time }}, value = {{ value }}, group_by = group_by,
      baseline_summary = dots$baseline_summary %||% "mean"
    )
  }
  if ("mean_reduction" %in% families) {
    tables$mean_reduction <- mean_reduction_metrics(
      data = dat, time = {{ time }}, value = {{ value }}, group_by = group_by,
      baseline_summary = dots$baseline_summary %||% "mean"
    )
  }
  if ("shape" %in% families) {
    tables$shape <- curve_shape_metrics(
      data = dat, time = {{ time }}, value = {{ value }}, group_by = group_by,
      baseline_summary = dots$baseline_summary %||% "mean"
    )
  }
  if ("stability" %in% families) {
    tables$stability <- overall_stability(
      data = dat, time = {{ time }}, value = {{ value }}, group_by = group_by,
      baseline_summary = dots$baseline_summary %||% "mean"
    )
  }

  tables <- tables[!vapply(tables, is.null, logical(1))]
  if (length(tables) == 0) {
    return(tibble::tibble())
  }

  out <- NULL
  seen <- key

  for (nm in names(tables)) {
    tbl <- tables[[nm]]
    metric_cols <- setdiff(names(tbl), key)

    if (isTRUE(prefix)) {
      names(tbl)[match(metric_cols, names(tbl))] <- paste0(nm, "_", metric_cols)
      metric_cols <- paste0(nm, "_", metric_cols)
    } else {
      # Shared landmark columns (baseline, drought_minimum, stress_detected...)
      # are identical across families by construction, so the first one wins and
      # the rest are dropped instead of becoming .x/.y suffixes.
      dup <- intersect(metric_cols, seen)
      tbl <- tbl[, setdiff(names(tbl), dup), drop = FALSE]
      metric_cols <- setdiff(metric_cols, dup)
    }

    seen <- union(seen, metric_cols)
    out <- if (is.null(out)) tbl else dplyr::full_join(out, tbl, by = key)
  }

  out
}
