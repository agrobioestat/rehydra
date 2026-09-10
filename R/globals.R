#' @keywords internal
#' @noRd
#' @importFrom rlang .data
#' @importFrom generics tidy augment glance
NULL

# Prevent R CMD check notes for tidy-eval default symbols used in signatures.
utils::globalVariables(c("transformed_value"))
