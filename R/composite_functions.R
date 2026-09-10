#' Composite Multivariable Resilience Score
#'
#' Collapse an index measured on several physiological variables into a single
#' score per group and cycle. A drought experiment usually measures
#' `Fv/Fm`, stomatal conductance, water potential and photosynthesis together;
#' this is the number a paper reports when it says "genotype A was more
#' resilient" without qualifying which variable.
#'
#' @param indices A per-cycle index table with a `variable` column, for example
#'   the output of [resilience_index()] or [rehydra_indices()].
#' @param metric Name of the index column to combine.
#' @param method How to combine the variables:
#'   `"pca"` (first principal component), `"mean"` (unweighted mean of the
#'   standardized variables) or `"weighted"` (weights supplied in `weights`).
#' @param weights Named numeric vector of weights, one per variable, used when
#'   `method = "weighted"`. Rescaled to sum to one.
#' @param group_by Grouping columns that identify one observational unit. The
#'   variables are spread within each of these.
#' @param standardize Logical; centre and scale each variable before combining.
#' @param align_sign Logical; flip the composite so that it correlates
#'   positively with the mean standardized variable.
#'
#' @details
#' The index is first spread to a matrix `Z` with one row per observational unit
#' (group and cycle) and one column per variable, then standardized column-wise,
#'
#' \deqn{z_{ij} = \frac{x_{ij} - \bar{x}_j}{s_j},}
#'
#' so that variables measured in MPa, mol m-2 s-1 and dimensionless ratios
#' contribute on the same footing. Without this step the variable with the
#' largest numeric range would dominate the composite for purely unit-related
#' reasons.
#'
#' **`"pca"`** takes the first principal component of `Z`, the linear
#' combination that captures the most variance across variables. Its `loading`
#' for each variable is reported, together with `variance_explained`: a first
#' component explaining, say, 0.8 means the variables largely agree and one
#' score is a fair summary, whereas 0.4 means they disagree and the composite is
#' hiding a trade-off that should be reported per variable instead. That
#' diagnostic is the reason to prefer PCA over a plain mean here.
#'
#' The sign of a principal component is arbitrary. With `align_sign = TRUE`
#' (the default) the component is flipped when needed so that a higher score
#' always means better performance, on the convention that the supplied `metric`
#' is already oriented that way; for damage-type metrics such as `deficit_auc`,
#' [memory_direction()] returns `TRUE` and the interpretation reverses.
#'
#' **`"mean"`** is the unweighted mean of the standardized columns, and
#' **`"weighted"`** the weighted mean. Both are more transparent than PCA and
#' preferable when the relative importance of the variables is decided a priori
#' rather than by the data.
#'
#' Rows with a missing value in any variable are dropped, because a composite
#' built on a different set of variables per row is not comparable across rows;
#' `n_variables` records how many entered the score.
#'
#' @return A tibble with the grouping columns, `composite`, `n_variables` and
#'   `method`. For `method = "pca"` the loadings and the variance explained are
#'   attached as the attributes `"loadings"` and `"variance_explained"`, and are
#'   also returned by [composite_loadings()].
#'
#' @seealso [rehydra_indices()], [rehydra_anova()], [memory_trend()].
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
#' score <- composite_resilience(res, metric = "Rs")
#' head(score)
#' composite_loadings(score)
#' @export
composite_resilience <- function(
    indices,
    metric = "Rs",
    method = c("pca", "mean", "weighted"),
    weights = NULL,
    group_by = c("genotype", "treatment", "replicate", "plant_id", "cycle"),
    standardize = TRUE,
    align_sign = TRUE
) {
  method <- match.arg(method)

  dat <- tibble::as_tibble(indices)

  if (!"variable" %in% names(dat)) {
    rlang::abort("`indices` must contain a `variable` column to combine across.")
  }
  if (!metric %in% names(dat)) {
    rlang::abort(paste0("`metric` column `", metric, "` was not found in `indices`."))
  }
  if (!is.numeric(dat[[metric]])) {
    rlang::abort(paste0("`metric` column `", metric, "` must be numeric."))
  }

  key <- intersect(group_by, names(dat))
  if (length(key) == 0) {
    rlang::abort("None of the `group_by` columns were found in `indices`.")
  }

  wide <- dat |>
    dplyr::select(dplyr::all_of(c(key, "variable", metric))) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(key, "variable")))) |>
    dplyr::summarise(.value = .safe_mean(.data[[metric]]), .groups = "drop") |>
    tidyr::pivot_wider(names_from = "variable", values_from = ".value")

  var_cols <- setdiff(names(wide), key)
  if (length(var_cols) < 2L) {
    rlang::abort(
      "At least two variables are needed for a composite score; found: ",
      paste(var_cols, collapse = ", ")
    )
  }

  complete <- stats::complete.cases(wide[, var_cols, drop = FALSE])
  wide <- wide[complete, , drop = FALSE]
  if (nrow(wide) < 2L) {
    rlang::abort("Fewer than two rows have a value for every variable.")
  }

  z <- as.matrix(wide[, var_cols, drop = FALSE])
  if (isTRUE(standardize)) {
    z <- scale(z)
    # A variable that is constant across units has zero variance and would give
    # NaN; it carries no information about differences between units.
    constant <- !is.finite(attr(z, "scaled:scale")) | attr(z, "scaled:scale") == 0
    if (any(constant)) {
      z <- z[, !constant, drop = FALSE]
      var_cols <- var_cols[!constant]
      if (ncol(z) < 2L) {
        rlang::abort("Fewer than two variables vary across units after standardization.")
      }
    }
    z <- matrix(as.numeric(z), nrow = nrow(z), dimnames = list(NULL, var_cols))
  }

  loadings <- NULL
  var_explained <- NA_real_

  if (method == "pca") {
    pca <- stats::prcomp(z, center = !standardize, scale. = FALSE)
    score <- as.numeric(pca$x[, 1L])
    loadings <- stats::setNames(as.numeric(pca$rotation[, 1L]), var_cols)
    var_explained <- (pca$sdev[1L]^2) / sum(pca$sdev^2)
  } else {
    w <- if (method == "weighted") {
      if (is.null(weights) || is.null(names(weights))) {
        rlang::abort("`weights` must be a named numeric vector when method = \"weighted\".")
      }
      missing_w <- setdiff(var_cols, names(weights))
      if (length(missing_w) > 0) {
        rlang::abort(paste0("`weights` is missing: ", paste(missing_w, collapse = ", "), "."))
      }
      ww <- as.numeric(weights[var_cols])
      if (any(!is.finite(ww)) || sum(ww) == 0) {
        rlang::abort("`weights` must be finite and not sum to zero.")
      }
      ww / sum(ww)
    } else {
      rep(1 / length(var_cols), length(var_cols))
    }

    score <- as.numeric(z %*% w)
    loadings <- stats::setNames(w, var_cols)
  }

  if (isTRUE(align_sign)) {
    reference <- rowMeans(z)
    if (stats::sd(score) > 0 && stats::sd(reference) > 0) {
      if (stats::cor(score, reference) < 0) {
        score <- -score
        loadings <- -loadings
      }
    }
  }

  out <- wide[, key, drop = FALSE]
  out$composite <- score
  out$n_variables <- length(var_cols)
  out$method <- method

  attr(out, "loadings") <- loadings
  attr(out, "variance_explained") <- var_explained
  out
}

#' Loadings of a Composite Resilience Score
#'
#' @param composite Output of [composite_resilience()].
#'
#' @return A tibble with `variable`, `loading` and `variance_explained`.
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
#' composite_loadings(composite_resilience(resilience_index(segments)))
#' @export
composite_loadings <- function(composite) {
  loadings <- attr(composite, "loadings")
  if (is.null(loadings)) {
    return(tibble::tibble(
      variable = character(), loading = numeric(), variance_explained = numeric()
    ))
  }

  tibble::tibble(
    variable = names(loadings),
    loading = as.numeric(loadings),
    variance_explained = attr(composite, "variance_explained") %||% NA_real_
  )
}
